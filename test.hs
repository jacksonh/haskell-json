import qualified Language.Haskell.Parser as HS
import qualified Language.Haskell.Syntax as HS
import qualified Language.Haskell.Pretty as HS

validToplevelExprs :: String -> Either String String
validToplevelExprs expr = do
  let r = HS.parseModuleWithMode HS.defaultParseMode
                                 { HS.parseFilename = "Try Haskell Source" }
                                 ("module TryHaskell where " ++ expr)
  case r of
    HS.ParseFailed{} -> Left "Parse failed."
    HS.ParseOk (HS.HsModule loc mn n is exprs) ->
        case all valid exprs of
          True  -> Right $  HS.prettyPrint $ HS.HsModule loc mn n [] exprs
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
