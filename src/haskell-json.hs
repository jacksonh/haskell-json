module Main where

import Control.Applicative
import Control.Monad.State (gets)
import Control.Concurrent.MVar
import Control.Monad.Error
import Data.List (isPrefixOf)
import Safe (readMay)
import System.IO
import System.Directory

import qualified Control.Exception as C
import qualified Network.FastCGI as CGI
import qualified Network.CGI.Session as CGI
import qualified System.Process as P
import qualified Text.JSON.JPath as JSON
import qualified Text.JSON.Types as JSON
import qualified Text.JSON as JSON
import qualified Language.Haskell.Parser as HS
import qualified Language.Haskell.Syntax as HS
import qualified Language.Haskell.Pretty as HS

--------------------------------------------------------------------------------
-- Section: Main entry point
-- Keywords: main
-- Description: 1) Starts the Haskell evaluator process.
--              2) Starts the fast CGI process.
--              Only one thread for the whole program. Only one mueval
--              process (for now).

-- | Main program function.
main :: IO ()
main = do mueval <- muevalStart >>= newMVar
          CGI.runSessionCGI "HASKELLJSON" CGI.runFastCGI $ restResponse mueval

--------------------------------------------------------------------------------
-- Section: Allowing top-level expression
-- Keywords: toplevel
-- Description: Checks lines to see if they're top-level expressions, if so,
--              add them to this session's .hs file.

-- | Check a module only has the whitelisted top-level declarations.
validToplevelExprs :: String -> Either String String
validToplevelExprs expr = do
  let r = HS.parseModuleWithMode HS.defaultParseMode
                                 { HS.parseFilename = "Try Haskell Source" }
                                 ("module TryHaskell where " ++ expr)
  case r of
    HS.ParseFailed{} -> Left "Parse failed."
    HS.ParseOk (HS.HsModule loc mn n is exprs) ->
        case all valid exprs of
          True  -> Right $ HS.prettyPrint $ HS.HsModule loc mn n [] exprs
          False -> Left $ "Invalid top-level expression." ++ show r
   where valid expr =
             case expr of
               HS.HsPatBind{} -> True
               HS.HsFunBind{} -> True
               HS.HsDataDecl{} -> True
               HS.HsClassDecl{} -> True
               HS.HsInstDecl{} -> True
               HS.HsTypeDecl{} -> True
               HS.HsNewTypeDecl{} -> True
               HS.HsTypeSig{} -> True
               _ -> False

--------------------------------------------------------------------------------
-- Section: Evaluator
-- Keywords: evaluator
-- Description: Talks to a piped mueval to repeatedly evaluate expressions.

type Mueval = (Handle,Handle,Handle,P.ProcessHandle)

-- | Launch the mueval piped process.
muevalStart :: IO Mueval
muevalStart = do (Just hin,Just hout,Just herr,p)
                      <- P.createProcess (P.proc path ["-r"])
                   { P.std_in = P.CreatePipe
                   , P.std_out = P.CreatePipe
                   , P.std_err = P.CreatePipe }
                 mapM_ (flip hSetBuffering NoBuffering) [hin,hout,herr]
                 return (hin,hout,herr,p)
                 where path = "/home/chris/.cabal/bin/mueval-core"

-- | Send an expression to mueval, return the result or error message.
mueval :: String -> Mueval -> IO (Mueval,String)
mueval expr mueval = 
    repl mueval `C.catch` \(e :: C.IOException) -> do
      let (_,_,herr,pid) = mueval
      err <- hGetLine herr `C.catch` \(e :: C.IOException) ->
                                        return $ "Terminated!" ++ show e
      P.waitForProcess pid
      mueval' <- muevalStart
      return (mueval',err)
    where repl mueval@(hin,hout,herr,p) = do
            hPutStrLn hin expr; hGetLine hout >>= return . (,) mueval

muevalEval :: String -> String -> Mueval -> IO (Mueval,String)
muevalEval path expr mu = do
  pathexists <- doesFileExist path
  if pathexists
     then do (mu',_) <- mueval (":l " ++ path) mu
             mueval expr mu'
     else do (mu',_) <- mueval (":reset") mu
             mueval expr mu'

--------------------------------------------------------------------------------
-- Section: RESTful response
-- Keywords: response
-- Description: Make a RESTful response to an evaluation request. This program
--              adheres to the JSON RFC 4627 [1] and the JSON RPC proposal[2].

-- | RESTful CGI response.
restResponse :: MVar Mueval -> CGI.SessionM CGI.CGIResult
restResponse mueval = do
  -- TODO: Enable when live, disable for testing.
  -- CGI.setHeader "Content-Type" "application/json"
  -- TODO: Remove when live, just for testing.
  lift $ CGI.setHeader "Content-Type" "text/plain"
  request <- parseRPC
  response <- rpcRespond mueval request
  lift $ CGI.output $ JSON.encode response

--------------------------------------------------------------------------------
-- Section: Parse RPC
-- Keywords: parse
-- Description: Parse a remote procedure call request.

-- A request consists of the following[2]:
--
--  jsonrpc  A String specifying the version of the JSON-RPC
--             protocol. MUST be exactly "2.0".
--
--  method   A String containing the name of the procedure to be invoked.
--
--  params   An Array or Object, that holds the actual parameter values
--           for the invocation of the procedure. Can be omitted if empty.
--
--  id       A Request identifier that MUST be a JSON scalar (String,
--           Number, True, False), but SHOULD normally not be Null[^1],
--           and Numbers SHOULD NOT contain fractional parts[^2].
--
-- [^1]: The use of Null for id in Requests is discouraged, because this
--       specification uses an id of Null for Responses with an unknown
--       id, and because JSON-RPC 1.0 uses an id of Null for Notifications.
-- [^2]: Fractional parts may be problematic, since many decimal fractions
--       cannot be represented exactly as binary fractions.

-- | A type for parsing an RPC request. It might use the Error monad in future.
type RPCParser = Either IDAndError RPCRequest

-- | The request ID and an RPC Error.
type IDAndError = (String,RPCError)

-- | An RPC request.
data RPCRequest =
    RPCRequest { rpr_method :: String
               , rpr_params :: String
               , rpr_id     :: String }
    deriving Show

-- | Try to parse the GCI inputs into an RPC value.
parseRPC :: CGI.SessionM RPCParser
parseRPC = do
  maybe (Left notEnoughParams) tryParseParams . sequence . map emptyIsNothing
        <$> mapM (fmap (fmap $ take 512) . lift . CGI.getInput) ["jsonrpc","method","params","id"]
                  -- We don't provide the ID at this point, which may be 
                  -- bad practice.
            where notEnoughParams =
                      ("",rpcError { rpe_code = RPCEError_InvalidParams
                                   , rpe_message = "Not enough parameters\
                                                   \ provided." })
                  -- Empty parameters are classed as not provided at all.
                  emptyIsNothing (Just "") = Nothing
                  emptyIsNothing x         = x

-- | Try to parse a list of RPC inputs to an RPC value.
tryParseParams :: [String] -> RPCParser
tryParseParams [jsonrpc,method,params,id]
    -- This program will only support RPC 2.0.
    | jsonrpc /= "2.0" =
        Left $ (id,rpcError { rpe_code =  RPCEError_InvalidReq
                            , rpe_message = "Invalid RPC version." })
    -- Don't do any further parsing (i.e. of the params) because JPath[3] is
    -- used later.
    | otherwise        = Right $ RPCRequest { rpr_method  = method
                                            , rpr_params = params
                                            , rpr_id     = id
                                            }

--------------------------------------------------------------------------------
-- Section: RPC Response
-- Keywords: response
-- Description: RPC response values.

-- When a remote procedure call is made, the service MUST reply with a Response
-- (except for Notifications). The Response is expressed as a single JSON
-- Object, with the following members[2]:

--  jsonrpc  A String specifying the version of the JSON-RPC protocol. MUST be
--           exactly "2.0".
--
--  result   Required on success, omitted on failure.
--           The Value that was returned by the procedure. Its contents is
--           entirely defined by the procedure.
--           This member MUST be entirely omitted if there was an error invoking
--           the procedure.
--
--  error    Required on error, omitted on success.
--           An Object containing error information about the fault that
--           occurred before, during or after the call.
--           This member MUST be entirely omitted if there was no such fault.
--
--  id       The same id as in the Request it is responding to. If there was an
--           error before detecting the id in the Request (e.g. Parse
--           error/Invalid Request), it MUST be Null. 

data RPCResponse =
    RPCResponse { rps_jsonrpc :: String
                , rps_result  :: Maybe RPCResult
                , rps_error   :: Maybe RPCError
                , rps_id      :: String
                }
    deriving Show

jsonstr = JSON.JSString . JSON.toJSString

instance JSON.JSON RPCResponse where
    readJSON _ = error "readJSON not implemented for RPCResponse"
    showJSON (RPCResponse jsonrpc result error id) =
        JSON.makeObj $ [("jsonrpc",jsonstr jsonrpc)
                       ,("id",jsonstr id)]
                       ++ result' ++ error'
            where result' = case result of
                              Just r  -> [("result",JSON.showJSON r)]
                              Nothing -> []
                  error' = case error of
                              Just r  -> [("error",JSON.showJSON r)]
                              Nothing -> []

-- | Default constructor for RPCResponse.
rpcResponse = RPCResponse "2.0" Nothing Nothing ""

data RPCResult = RPCEvalSuccess { rrs_original :: String
                                , rrs_type     :: String
                                , rrs_result   :: String }
               | RPCEvalError { rss_error :: String }
               | RPCEvalException { rss_exception :: String }
               | RPCEvalInternalError
               | RPCLoadSuccess
               | RPCLoadSyntaxError String
               | RPCLoadGenericError String
               deriving Show

instance JSON.JSON RPCResult where
    readJSON _ = error "readJSON not implemented for RPCResult"
    showJSON (RPCEvalSuccess original typ result) = 
        JSON.makeObj $ [("result",jsonstr result)
                       ,("type",jsonstr typ)
                       ,("expr",jsonstr original)]
    showJSON (RPCEvalError error) = 
        JSON.makeObj $ [("error",jsonstr error)]
    showJSON (RPCEvalException error) = 
        JSON.makeObj $ [("exception",jsonstr error)]
    showJSON RPCEvalInternalError = 
        JSON.makeObj $ [("internal",jsonstr "Terminated!")]
    showJSON RPCLoadSuccess =
        JSON.makeObj $ [("success",jsonstr "")]
    showJSON (RPCLoadSyntaxError e) =
        JSON.makeObj $ [("error",jsonstr e)]
    showJSON (RPCLoadGenericError e) =
        JSON.makeObj $ [("load-error",jsonstr e)]

-- When a remote procedure call fails, the Procedure Return object MUST contain
-- the error member whose value is a JSON Object with the following members:
--
-- code     A Number that indicates the actual error that occurred. This MUST
--          be an integer.
--
-- message  A String providing a short description of the error. The message 
--          SHOULD be limited to a concise single sentence.
--
-- data     Additional information, may be omitted. Its contents is entirely 
--          defined by the application (e.g. detailed error information, nested
--          errors etc.).

-- The error-codes -32768 .. -32000 (inclusive) are reserved for pre-defined
-- errors. Any error-code within this range not defined explicitly below is 
-- reserved for future use. [2]

-- | RPC error object.
data RPCError =
    RPCError { rpe_code    :: RPCEErrorCode
             , rpe_message :: String
             , rpe_data    :: Maybe String
             }
    deriving Show

instance JSON.JSON RPCError where
    readJSON _ = error "readJSON not implemented for RPCError"
    showJSON (RPCError code msg dat) =
        JSON.makeObj $ [("code",jsonstr $ show code),("message",jsonstr msg)]
                         ++ maybe [] (return . (,) "data" . jsonstr) dat

-- | RPC error codes.
data RPCEErrorCode = RPCEError_ParseError
                   | RPCEError_InvalidReq
                   | RPCEError_UnknownMethod
                   | RPCEError_InvalidParams
                   | RPCEError_InternalError
                   | RPCEError_ServerError Integer
                   | RPCEError_ApplicationError Integer
                     deriving (Show,Eq)

-- | Convert an RPC error to an actual integer code.
rpcErrorToInteger :: RPCEErrorCode -> Integer
rpcErrorToInteger RPCEError_ParseError = -32700
rpcErrorToInteger RPCEError_InvalidReq = -32600
rpcErrorToInteger RPCEError_UnknownMethod = -32601
rpcErrorToInteger RPCEError_InvalidParams = -32602
rpcErrorToInteger RPCEError_InternalError = -32603
rpcErrorToInteger (RPCEError_ServerError n) = n
rpcErrorToInteger (RPCEError_ApplicationError n) = n

-- | Default constructor for RPCError.
rpcError = RPCError (error "No error field specified for rpcError.") "" Nothing

--------------------------------------------------------------------------------
-- Section: RPC responder
-- Keywords: respond
-- Description: Make an RPC response to the RPC request, talking to the Haskell
--              interpreter when the "eval" method is requested.

-- | Generate an RPC response for the request.
rpcRespond :: MVar Mueval
           -> Either IDAndError RPCRequest
           -> CGI.SessionM RPCResponse
-- If the request was erroneous, just return an error-reporting response.
rpcRespond _ (Left (id,error)) =
    return $ rpcResponse { rps_id = id, rps_error = Just error }
rpcRespond mvar (Right request)
    | rpr_method request == "eval" = evalRespond mvar request
    | rpr_method request == "load" = loadRespond mvar request
 | otherwise =
    return $ rpcResponse { rps_id = rpr_id request
                         , rps_error =
                             Just $ rpcError { rpe_code    =
                                                   RPCEError_UnknownMethod
                                             , rpe_message = "Unknown method." }
                         }

loadRespond mvar request = 
  case JSON.jPath "/contents" $ rpr_params request of
    Right [JSON.JSString (JSON.JSONString exprs)] -> do
        result <- loadModule mvar exprs
        return $ rpcResponse { rps_result = Just result
                             , rps_id     = rpr_id request
                             }
   -- Unable to parse the parameters, fail it.
    _ -> return $ rpcResponse { rps_id = rpr_id request
                              , rps_error =
                                  Just $ rpcError { rpe_code    =
                                                    RPCEError_InvalidParams
                                                  , rpe_message = "Invalid params." }
                              }

loadModule mvar contents = do
  case validToplevelExprs contents of
    Right m -> do path <- sessionFile
                  liftIO . writeFile path $ m
                  result <- liftIO . modifyMVar mvar . mueval $ ":l " ++ path
                  return $ loadResult result
    Left e -> return . RPCLoadSyntaxError $ e

sessionFile = do
  dir <- lift $ (++"/res/") . maybe "" id
           <$> CGI.getVar "DOCUMENT_ROOT"
  (dir++) . (++".hs") . show <$> CGI.sessionId

-- | Make a JSON result out of a mueval load result.
loadResult :: String -> RPCResult
loadResult "OK." = RPCLoadSuccess
loadResult e     = RPCLoadGenericError e

evalRespond mvar request = 
  case JSON.jPath "/expr" $ rpr_params request of
    Right [JSON.JSString (JSON.JSONString expr)] | not $ ":l " `isPrefixOf` expr -> do
        path <- sessionFile
        result <- liftIO $ modifyMVar mvar $ muevalEval path expr
        return $ rpcResponse { rps_result = Just $ toResult expr result
                             , rps_id     = rpr_id request
                             }
   -- Unable to parse the parameters, fail it.
    _ -> return $ rpcResponse { rps_id = rpr_id request
                              , rps_error =
                                  Just $ rpcError { rpe_code    =
                                                    RPCEError_InvalidParams
                                                  , rpe_message = "Invalid params." }
                              }

-- | Make a JSON result out of a mueval result.
toResult :: String -> String -> RPCResult
toResult expr res =
    case readMay res of
      Just (orig,typ,res) -> RPCEvalSuccess orig typ res
      _ -> case readMay res of
            Just err -> RPCEvalError err
            _  | isPrefixOf pre res -> RPCEvalException (core res)
               | otherwise          -> 
                   case readMay res of
                     Just x@(_:_) -> RPCEvalSuccess expr "" $ show (x :: [String])
                     Nothing      -> RPCEvalInternalError
               where core = drop (length pre)
                     pre =  "mueval-core: "

--------------------------------------------------------------------------------
-- Section: References
-- Keywords: references
-- Description: List of references referred to in this program source code.

-- [1]: JSON RFC 4627 <http://tools.ietf.org/html/rfc4627>
-- [2]: JSON RPC Proposal
--       <http://groups.google.com/group/json-rpc/web/json-rpc-1-2-proposal>
-- [3]: JPath is XPath-inspired query language to query JSON data
--       <http://hackage.haskell.org/package/hjpath-1.0>
