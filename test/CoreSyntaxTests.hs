module CoreSyntaxTests() where

import CoreSyntax
import Lexer
import NormalizedSyntax
import Parser
import TestUtils
import UniversalSyntax

allCoreSyntaxTests = do
  testFunction ((normalizeCoreDecl 0) . parseDecl . strToToks)  normCoreCases

normCoreCases =
  [("f = \\x y z -> x",
    [nFuncDecl (var "f")
     [var "x", var "y", var "z"]
     (nExprBody $ nVarExpr $ var "x")]),
   ("d = 123", [nExprDecl (var "d") (nLitExpr $ intLit 123)]),
   ("d = erER", [nExprDecl (var "d") (nVarExpr $ var "erER")])]


