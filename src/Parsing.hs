module Parsing(
  parseCoreModule) where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Parser

import CoreSyntax
import UniversalSyntax

parseCoreModule :: String -> CoreModule
parseCoreModule text = case parseModule text of
  ParseOk hsModule -> toCoreModule hsModule
  ParseFailed loc msg -> error $ "Error: " ++ show loc ++ " " ++ msg

toCoreModule :: Module -> CoreModule
toCoreModule (Module _ (ModuleName name) _ _ _ _ decls) =
  coreModule name $ map toCoreLanguage decls

toCoreLanguage :: Decl -> CoreDecl
toCoreLanguage decl = coreDecl (var "a") (cVarExpr (var "a"))
