{-# LANGUAGE ViewPatterns, PackageImports, ScopedTypeVariables, PatternGuards #-}
module Main where

import Control.OldException
import qualified System.IO.Error as IE
import Control.Applicative                    ((<$>))
import Control.Concurrent                     (MVar)
import qualified Control.Concurrent           as V
import qualified Control.Exception            as C
import Control.Monad (when)
import Data.Maybe (isJust)
import "mtl" Control.Monad.Trans              (liftIO,lift)
import Data.Char                              (isLetter,isDigit)
import Data.List                              (isPrefixOf,intercalate)
import Data.Map                               (Map)
import qualified Data.Map                     as M
import qualified Language.Haskell.Exts.Parser as HP
import qualified Language.Haskell.Exts.Syntax as HS
import qualified Language.Haskell.Exts.Pretty as HPP
import Network.FastCGI                        (CGIResult)
import qualified Network.FastCGI              as CGI
import Network.FastCGI              (CGI)
import Network.CGI.Session                    (SessionM)
import qualified Network.CGI.Session          as CGIS
import System.Process                         as P
import System.IO                              as IO
import System.Directory                       (doesFileExist)
import System.Environment                     (getEnvironment)
import Text.JSON.Generic                      (encodeJSON)
import Data.List (nub)
import System.Directory

-- | FastCGI response stuff.
main :: IO ()
main = run where
  run = do
    path <- maybe (error "mueval-core path required") id . lookup "MUEVAL_CORE"
	    <$> getEnvironment
    mueval <- muevalStart path >>= V.newMVar
    bindings <- V.newMVar M.empty
    CGIS.runSessionCGI "HASKELLJSON" (CGI.runFastCGI.catchStuff) $ do
  --         liftIO $ appendFile "haskell-json.log" "New request...\n"
	   lift $ CGI.setHeader "Content-Type" "text/plain"
	   method <- lift $ CGI.getInput "method"
	   case method of
	     Just "eval" -> evalExpr mueval bindings
	     Just "load" -> loadFile mueval
	     _           -> respond [("error","Unknown method.")]

catchStuff = id  

--  bollocks e = appendFile "haskell-json.log" $ show e ++ "\n">>run

-- | A binding from an evaluation state to a set of bindings.
type Bindings = Map String HS.Exp

-- | Method: eval
--   Parameters: expr
--   Returns: {"result":"10","type":"Int","expr":"5*2"} or failure
evalExpr :: MVar Mueval -> MVar Bindings -> SessionM CGIResult
evalExpr mu bindings = do
  contentsProvided <- CGI.getInput "contents"
  case contentsProvided of
    Nothing -> eval
    Just contents -> case validToplevelExprs contents of
      Left e -> respond [("error",e)]
      Right m -> do result <- loadValidDecl mu m
                    case result of 
                      "OK." -> eval
                      e     -> respond $ loadResult result
  where eval = do
          withParam "expr" $ \expr -> do
            if isPrefixOf ":l " expr
               then respond [("error","<no location info>: parse error on input `:'")]
               else do guid <- lift $ CGI.getInput "guid"
                       path <- sessionFile guid
                       result <- evalOrBind mu path expr bindings
                       respondResult result expr

respondResult :: String -> String -> SessionM CGIResult
respondResult result expr = do
  case readMay result of
    Just (_::String,typ,res) -> respond [("result",res),("type",typ),("expr",expr)]
    Nothing             ->
      case result of
        "bind" -> respond [("bind","OK.")]
        _      -> respond $ errorResponse result

-- | Either evaluate an expression or bind a top-level variable.
evalOrBind :: MVar Mueval -> String -> String -> MVar Bindings -> SessionM String
evalOrBind mu path expr bindings
--  | HP.ParseOk decl <- HP.parseDecl expr = bind mu path decl bindings
  | otherwise                            = eval mu path expr bindings

-- | Bind a top-level declaration into the session state.
bind :: MVar Mueval -> String -> HS.Decl -> MVar Bindings -> SessionM String
bind mu uid decl bindings = do
  currentBs <- liftIO $ V.readMVar bindings
  liftIO $ V.modifyMVar_ bindings (return . updateBindings uid decl)
  -- Just to check that their bindings were in scope/type correct:
  result <- eval mu uid "()" bindings
  case readMay result of
    Just (_::String,"()","()") -> return "bind"
    -- If incorrect just reset the bindings and return the error:
    _            -> do liftIO $ V.modifyMVar_ bindings (return . const currentBs)
                       return result

updateBindings :: String -> HS.Decl -> Bindings -> Bindings
updateBindings uid decl = M.insertWith' update uid initial where
  initial = HS.Let (HS.BDecls [decl]) unit
  update _ = nestedBind decl

-- | Nest a binding inside another let binding.
nestedBind :: HS.Decl -- ^ A top-level declaration.
           -> HS.Exp  -- ^ An expression.
           -> HS.Exp  -- ^ An expresion with the decl nested inside it.
nestedBind ds (HS.Let dss HS.Con{}) = HS.Let dss (HS.Let (HS.BDecls [ds]) unit)
nestedBind ds (HS.Let dss inner)    = HS.Let dss (nestedBind ds inner)
nestedBind _    expr                = expr

-- | An expr with bindings.
withBindings :: HS.Exp -- ^ Expression
             -> HS.Exp -- ^ Bindings
             -> HS.Exp -- ^ Expression with bindings in lexical context.
withBindings e (HS.Let ds HS.Con{}) = HS.Let ds e
withBindings e (HS.Let ds inner)    = HS.Let ds (withBindings e inner)
withBindings e _                    = e

unit :: HS.Exp
unit = HS.Con (HS.Special HS.UnitCon)

-- | Method: load
--   Parameters: contents
--   Returns: {"success":""} or failure
loadFile :: MVar Mueval -> SessionM CGIResult
loadFile mvar =
    withParam "contents" $ \contents -> do
      loadModule mvar contents

-- | Write the module contents to file.
loadModule :: MVar Mueval -> String -> SessionM CGIResult
loadModule mvar contents = do
  case validToplevelExprs contents of
    Right m -> do result <- loadValidDecl mvar m
                  respond $ loadResult result
    Left e -> respond [("error",e)]
    
loadValidDecl mvar m = do
  guid <- lift $ CGI.getInput "guid"
  path <- sessionFile guid
  liftIO . writeFile path . limitFileContents $ m
  liftIO . V.modifyMVar mvar . run $ ":l " ++ path

limitFileContents :: String -> String
limitFileContents = take (1024 * 20) -- 20KB

-- | Make a JSON result out of a mueval load result.
loadResult :: String -> [(String,String)]
loadResult "OK." = [("success","OK.")]
loadResult e     = [("error",e)]

-- | Check a module only has the whitelisted top-level declarations.
validToplevelExprs :: String -> Either String String
validToplevelExprs expr = do
  let r = HP.parseModuleWithMode HP.defaultParseMode
                                { HP.parseFilename = "Try Haskell Source" }
                                ("module TryHaskell where\n" ++ unlines (map ("import "++) dmodules) ++ "\n" ++ expr)
  case r of
    HP.ParseFailed{} -> Left "Parse failed."
    HP.ParseOk (HS.Module loc mn _ _ _ (filterImports -> imports) decls) ->
        case all valid decls of
          True -> Right $
                    HPP.prettyPrint $ HS.Module loc mn [] Nothing Nothing imports decls
          False -> Left $ "Invalid top-level expression." ++ show r
   where valid expr' =
             case expr' of
               HS.PatBind{} -> True
               HS.FunBind{} -> True
               HS.DataDecl{} -> True
               HS.ClassDecl{} -> True
               HS.InstDecl{} -> True
               HS.TypeDecl{} -> True
               HS.TypeSig{} -> True
               _ -> False
         filterImports = nub .
                             filter ((`elem` (map HS.ModuleName) allowedModules) . HS.importModule)
	 allowedModules =
	   dmodules ++
	   ["Control.Monad.ST"
	   ,"Control.Monad.State"
	   ,"Control.Monad.Reader"
	   ,"Data.STRef"
	   ,"Control.Monad.ST.Lazy"
	   ,"Control.Monad.ST.Strict"]

dmodules = ["Prelude"
           ,"Control.Monad"
           ,"Control.Monad.Fix"
	   ,"Control.Monad.State"
	   ,"Control.Monad.Reader"
	   ,"Data.STRef"
           ,"Data.Char","Data.Ord"
           ,"Data.Function"
           ,"Data.Maybe"
           ,"Data.List"]

-- | Convert a mueval error to a JSON response.
errorResponse :: String -> [(String,String)]
errorResponse res =
    case readMay res of
      Just err               -> [("error",err)]
      _ | isPrefixOf pre res -> [("exception",core res)]
        | otherwise          -> [("internal","Terminated!")]
      where core = drop (length pre)
            pre = "mueval-core: "

-- | Maybe read a value.
readMay :: Read a => String -> Maybe a
readMay s = case [x | (x,t) <- reads s, ("","") <- lex t] of
              [x] -> Just x
              _ -> Nothing

-- | Generate a filename from the user's session id.
sessionFile :: Maybe String -> SessionM String
sessionFile guid = do
  dir <- fmap (++ "../load/") (liftIO getTemporaryDirectory)
  (dir++) . (++".hs") . (++guid') . show <$> CGIS.sessionId
      where guid' = maybe "" (takeWhile valid) guid
            valid c = isLetter c || isDigit c

-- | Ensure a computation has a param.
withParam :: String -> (String -> SessionM CGIResult) -> SessionM CGIResult
withParam n m = do
  v <- lift $ CGI.getInput n
  maybe (respond [("error","Invalid parameters.")]) m v

-- | Simple responder (in JSON format).
respond :: [(String,String)] -> SessionM CGIResult
respond r = do
  func <- lift $ CGI.getInput "pad"
  let pad it = maybe it (\f -> f ++ "(" ++ it ++ ")") func
  let output = pad . toJson $ r
  let !x = all(=='a')output
  lift . CGI.output $ output

-- | Convert an alist to a json object.
toJson :: [(String,String)] -> String
toJson = braces . intercalate "," . map jsonIt where
    jsonIt (x,y) = encodeJSON x ++ ":" ++ encodeJSON y
    braces x = "{" ++ x ++ "}"

-- | Mueval evaluation handle.
type Mueval = (Handle,Handle,Handle,ProcessHandle,FilePath)

-- | Launch the mueval piped process.
muevalStart :: FilePath -> IO Mueval
muevalStart path =
    do (Just hin,Just hout,Just herr,p)
           <- createProcess (proc path ["-r"])
              { std_in = CreatePipe
              , std_out = CreatePipe
              , std_err = CreatePipe }
       mapM_ (flip hSetBuffering NoBuffering) [hin,hout,herr]
       return (hin,hout,herr,p,path)

-- | Send an expression to mueval, return the result or error message.
run :: String -> Mueval -> IO (Mueval,String)
run expr mueval' =
  repl mueval' `C.catch` (\(_ :: C.IOException) -> handler)
               `C.catch` (\(_ :: Control.OldException.Exception) -> handler)
    where handler = do
	    let (_,_,herr,pid,path) = mueval'
	    err <- hGetLine herr `C.catch` \(_ :: C.IOException) -> return $ "Terminated!"
	    _ <- waitForProcess pid
	    mueval'' <- muevalStart path
	    return (mueval'',err)
          repl mueval''@(hin,hout,_,p,_) = do
	    -- make an mvar
            didRespond <- V.newEmptyMVar
	    -- send the query
            hPutStrLn hin expr
 	    -- get my thread id
            tid <- V.myThreadId
            -- spawn a separate process
            V.forkOS $ do -- after 5 seconds...
	    	       	  V.threadDelay (1000 * 1000 * 5)
			  -- check if we're still waiting
                          isempty <- V.isEmptyMVar didRespond
                          if isempty -- we're still waiting
                             then do terminateProcess p
--                                     C.throwTo tid $ userError "Took too long"
                             else return () -- we're ok
            -- get the result
            ret <- hGetLine hout >>= return . (,) mueval''
            -- update the mvar to say it completed
            V.putMVar didRespond ()
            -- ok
            return ret

-- | Wrapper around the mueval function to ensure the session file is loaded.
eval :: MVar Mueval -> String -> String -> MVar Bindings -> SessionM String
eval mvar path expr bindings = liftIO $ do
  bs <- fmap (M.lookup path) $ V.readMVar bindings
  let exprWithBindings =
        case HP.parseExp expr of
          HP.ParseOk e     -> prettyPrint $ maybe e (withBindings e) bs
            where prettyPrint = HPP.prettyPrintWithMode mode
                  mode = HPP.defaultMode { HPP.layout = HPP.PPNoLayout }
          HP.ParseFailed{} -> expr
      preparedEval arg = V.modifyMVar mvar (\mu -> do
                             (mu',_) <- run arg mu
                             run expr mu')
  pathexists <- doesFileExist path
  if pathexists
     then preparedEval (unwords [":l",path])
     else preparedEval ":reset"

