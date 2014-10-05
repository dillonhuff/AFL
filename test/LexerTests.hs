module LexerTests(
  allLexerTests) where

import Lexer
import TestUtils

allLexerTests = do
  testFunction strToToks lexCases
  
lexCases =
  [("\\", [dRes "\\"]),
   ("->", [dRes "->"]),
   ("case", [dRes "case"]),
   ("of", [dRes "of"]),
   ("module", [dRes "module"]),
   ("where", [dRes "where"]),
   ("let", [dRes "let"]),
   ("=", [dRes "="]),
   ("(", [dlp]),
   (")", [drp]),
   ("+", [dVar "+"]),
   ("-", [dVar "-"]),
   ("*", [dVar "*"]),
   ("/", [dVar "/"]),
   ("&&", [dVar "&&"]),
   ("||", [dVar "||"]),
   ("~", [dVar "~"]),
   ("<", [dVar "<"]),
   (">", [dVar ">"]),
   ("<=", [dVar "<="]),
   (">=", [dVar ">="]),
   ("==", [dVar "=="]),
   ("Never", [dCon "Never"]),
   ("nooo", [dVar "nooo"]),
   ("e34Di3", [dVar "e34Di3"]),
   ("iflle", [dVar "iflle"]),
   ("modules", [dVar "modules"]),
   ("casesof", [dVar "casesof"]),
   ("1 - 45", [dInt 1, dVar "-", dInt 45]),
   ("x + 4", [dVar "x", dVar "+", dInt 4])]
