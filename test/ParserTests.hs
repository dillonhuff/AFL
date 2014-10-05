module ParserTests(allParserTests) where

import CoreSyntax
import Lexer
import Parser
import TestUtils
import UniversalSyntax

allParserTests = do
  testFunction (parseExpr . strToToks) parseExprCases

cve = cVarExpr . var
dcFalse = cDataCon (dataCon "False") []
dcTrue = cDataCon (dataCon "True") []
iLit = cLitExpr . intLit

parseExprCases =
  [("1", iLit 1),
   ("1 - 4", cAp (cAp (cve "-") (iLit 1)) (iLit 4)),
   ("3 * 3", cAp (cAp (cve "*") (iLit 3)) (iLit 3)),
   ("5 / 873", cAp (cAp (cve "/") (iLit 5)) (iLit 873)),
   ("x / 4", cAp (cAp (cve "/") (cve "x")) (iLit 4)),
   ("1 + 7", cAp (cAp (cve "+") (iLit 1)) (iLit 7)),
   ("2 /(4 + 2)",
    cAp (cAp (cve "/") (iLit 2)) (cAp (cAp (cve "+") (iLit 4)) (iLit 2))),
   ("f 2 y", cAp (cAp (cve "f") (iLit 2)) (cve "y")),
   ("f 2 (+) 1", cAp (cAp (cAp (cve "f") (iLit 2)) (cve "+")) (iLit 1)),
   ("f (2 *4) / 4",
    cAp (cAp (cve "/") (cAp (cve "f") (cAp (cAp (cve "*") (iLit 2)) (iLit 4)))) (iLit 4)),
   ("True", cDataCon (dataCon "True") []),
   ("False", cDataCon (dataCon "False") []),
   ("~True", cAp (cve "~") dcTrue),
   ("f True (~False)", cAp (cAp (cve "f") dcTrue) (cAp (cve "~") dcFalse)),
   ("True || False", cAp (cAp (cve "||") dcTrue) dcFalse),
   ("False && x", cAp (cAp (cve "&&") dcFalse) (cve "x")),
   ("x || ~(y && z)",
    cAp (cAp (cve "||") (cve "x")) (cAp (cve "~") (cAp (cAp (cve "&&") (cve "y")) (cve "z")))),
   ("x < y", cAp (cAp (cve "<") (cve "x")) (cve "y")),
   ("1 > 2", cAp (cAp (cve ">") (iLit 1)) (iLit 2)),
   ("x <= y", cAp (cAp (cve "<=") (cve "x")) (cve "y")),
   ("k2 >= k43", cAp (cAp (cve ">=") (cve "k2")) (cve "k43")),
   ("q == z", cAp (cAp (cve "==") (cve "q")) (cve "z")),
   ("f (==) (<) 23", cAp (cAp (cAp (cve "f") (cve "==")) (cve "<")) (iLit 23)),
   ("case 2 > k of | True -> 6 + 3 | False -> f (+) y",
    cCase (cAp (cAp (cve ">") (iLit 2)) (cve "k"))
    [cDataAlt (dataCon "True") [] (cAp (cAp (cve "+") (iLit 6)) (iLit 3)),
     cDataAlt (dataCon "False") [] (cAp (cAp (cve "f") (cve "+")) (cve "y"))]),
   ("\\ x y z -> x + (case 5 <= y of | True -> x | 234 -> z)",
    cLam (var "x") (cLam (var "y") (cLam (var "z") (cAp (cAp (cve "+") (cve "x")) (cCase (cAp (cAp (cve "<=") (iLit 5)) (cve "y")) [cDataAlt (dataCon "True") [] (cve "x"), cLitAlt (intLit 234) (cve "z")]))))),
   ("let qWerty = 45, x = f 1, uIO3 = False in x uIO3 qWerty",
    cLet [coreDecl (var "qWerty") (iLit 45), coreDecl (var "x") (cAp (cve "f") (iLit 1)), coreDecl (var "uIO3") dcFalse] (cAp (cAp (cve "x") (cve "uIO3")) (cve "qWerty")))]
