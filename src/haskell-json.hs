{-# LANGUAGE PackageImports, ScopedTypeVariables #-}
module Main where

import Control.Applicative               ((<$>))
import Control.Concurrent                (MVar)
import qualified Control.Concurrent      as V
import qualified Control.Exception       as C
import "mtl" Control.Monad.Trans         (liftIO,lift)
import Data.Char                         (isLetter,isDigit)
import Data.List                         (isPrefixOf,intercalate)
import qualified Language.Haskell.Parser as HP
import qualified Language.Haskell.Syntax as HS
import qualified Language.Haskell.Pretty as HPP
import Network.FastCGI                   (CGIResult)
import qualified Network.FastCGI         as CGI
import Network.CGI.Session               (SessionM)
import qualified Network.CGI.Session     as CGIS
import System.Process                    as P
import System.IO                         as IO
import System.Directory                  (doesFileExist)
import System.Environment                (getEnvironment)
import Text.JSON.Generic                 (encodeJSON)

-- | FastCGI response stuff.
main :: IO ()
main = do
  path <- maybe (error "mueval-core path required") id . lookup "MUEVAL_CORE"
          <$> getEnvironment
  mueval <- muevalStart path >>= V.newMVar
  CGIS.runSessionCGI "HASKELLJSON" CGI.runFastCGI $ do
         lift $ CGI.setHeader "Content-Type" "text/plain"
         method <- lift $ CGI.getInput "method"
         case method of
           Just "eval" -> evalExpr mueval
           Just "load" -> loadFile mueval
           _           -> respond [("error","Unknown method.")]

-- | Method: eval
--   Parameters: expr
--   Returns: {"result":"10","type":"Int","expr":"5*2"} or failure
evalExpr :: MVar Mueval -> SessionM CGIResult
evalExpr mu = do
    withParam "expr" $ \expr -> do
      if isPrefixOf ":l " expr
         then respond [("error","<no location info>: parse error on input `:'")]
         else do guid <- lift $ CGI.getInput "guid"
                 path <- sessionFile guid
                 result <- eval mu path expr
                 respondResult result
    where respondResult result = do
            case readMay result of
              Just (orig,typ,res) ->
                  respond [("result",res),("type",typ),("expr",orig)]
              Nothing             -> respond $ errorResponse result

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
    Right m -> do guid <- lift $ CGI.getInput "guid"
                  path <- sessionFile guid
                  liftIO . writeFile path . limitFileContents $ m
                  result <- liftIO . V.modifyMVar mvar . run $ ":l " ++ path
                  respond $ loadResult result
    Left e -> respond [("error",e)]

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
                                ("module TryHaskell where\n" ++ expr)
  case r of
    HP.ParseFailed{} -> Left "Parse failed."
    HP.ParseOk (HS.HsModule loc mn n _ exprs) ->
        case all valid exprs of
          True -> Right $ HPP.prettyPrint $ HS.HsModule loc mn n [] exprs
          False -> Left $ "Invalid top-level expression." ++ show r
   where valid expr' =
             case expr' of
               HS.HsPatBind{} -> True
               HS.HsFunBind{} -> True
               HS.HsDataDecl{} -> True
               HS.HsClassDecl{} -> True
               HS.HsInstDecl{} -> True
               HS.HsTypeDecl{} -> True
               HS.HsNewTypeDecl{} -> True
               HS.HsTypeSig{} -> True
               _ -> False

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
  dir <- lift $ (++"/res/") . maybe "" id <$> CGI.getVar "DOCUMENT_ROOT"
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
  lift . CGI.output . pad . toJson $ r

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
    repl mueval' `C.catch` \(_ :: C.IOException) -> do
      let (_,_,herr,pid,path) = mueval'
      err <- hGetLine herr `C.catch` \(_ :: C.IOException) ->
                                        return $ "Terminated!"
      waitForProcess pid
      mueval'' <- muevalStart path
      return (mueval'',err)
    where repl mueval''@(hin,hout,_,p,_) = do
            didRespond <- V.newEmptyMVar
            hPutStrLn hin expr
            tid <- V.myThreadId
            V.forkIO $ do V.threadDelay (1000 * 750 * 1)
                          isempty <- V.isEmptyMVar didRespond
                          if isempty
                             then do terminateProcess p
                                     V.throwTo tid (userError "Took too long.")
                             else return ()
            ret <- hGetLine hout >>= return . (,) mueval''
            V.putMVar didRespond ()
            return ret

-- | Wrapper around the mueval function to ensure the session file is loaded.
eval :: MVar Mueval -> String -> String -> SessionM String
eval mvar path expr = liftIO $ do
  pathexists <- doesFileExist path
  if pathexists
     then liftIO $ V.modifyMVar mvar $ \mu -> do
            (mu',_) <- run (":l " ++ path) mu
            run expr mu'
     else liftIO $ V.modifyMVar mvar $ \mu -> do
            (mu',_) <- run (":reset") mu
            run expr mu'
