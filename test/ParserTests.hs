module ParserTests(allParserTests) where

import CoreSyntax
import Lexer
import Parser
import TestUtils
import UniversalSyntax

allParserTests = do
  testFunction (parseExpr . strToToks) parseExprCases

parseExprCases =
  [("1", cLitExpr $ intLit 1),
   ("1 - 4", cAp (cAp (cVarExpr $ var "-") (cLitExpr $ intLit 1)) (cLitExpr $ intLit 4)),
   ("3 * 3", cAp (cAp (cVarExpr $ var "*") (cLitExpr $ intLit 3)) (cLitExpr $ intLit 3)),
   ("5 / 873", cAp (cAp (cVarExpr $ var "/") (cLitExpr $ intLit 5)) (cLitExpr $ intLit 873)),
   ("x / 4", cAp (cAp (cVarExpr $ var "/") (cVarExpr $ var "x")) (cLitExpr $ intLit 4)),
   ("1 + 7", cAp (cAp (cVarExpr $ var "+") (cLitExpr $ intLit 1)) (cLitExpr $ intLit 7)),
   ("2 /(4 + 2)",
    cAp (cAp (cVarExpr $ var "/") (cLitExpr $ intLit 2)) (cAp (cAp (cVarExpr $ var "+") (cLitExpr $ intLit 4)) (cLitExpr $ intLit 2))),
   ("f 2 y", cAp (cAp (cVarExpr $ var "f") (cLitExpr $ intLit 2)) (cVarExpr $ var "y")),
   ("f 2 (+) 1", cAp (cAp (cAp (cVarExpr $ var "f") (cLitExpr $ intLit 2)) (cVarExpr $ var "+")) (cLitExpr $ intLit 1)),
   ("f (2 *4) / 4",
    cAp (cAp (cVarExpr $ var "/") (cAp (cVarExpr $ var "f") (cAp (cAp (cVarExpr $ var "*") (cLitExpr $ intLit 2)) (cLitExpr $ intLit 4)))) (cLitExpr $ intLit 4)),
   ("True", cDataCon (dataCon "True") []),
   ("False", cDataCon (dataCon "False") []),
   ("~True", cAp (cVarExpr $ var "~") (cDataCon (dataCon "True") [])),
   ("f True (~False)", cAp (cAp (cVarExpr $ var "f") (cDataCon (dataCon "True") [])) (cAp (cVarExpr $ var "~") (cDataCon (dataCon "False") []))),
   ("True || False", cAp (cAp (cVarExpr $ var "||") (cDataCon (dataCon "True") [])) (cDataCon (dataCon "False") [])),
   ("False && x", cAp (cAp (cVarExpr $ var "&&") (cDataCon (dataCon "False") [])) (cVarExpr $ var "x")),
   ("x || ~(y && z)",
    cAp (cAp (cVarExpr $ var "||") (cVarExpr $ var "x")) (cAp (cVarExpr $ var "~") (cAp (cAp (cVarExpr $ var "&&") (cVarExpr $ var "y")) (cVarExpr $ var "z")))),
   ("x < y", cAp (cAp (cVarExpr $ var "<") (cVarExpr $ var "x")) (cVarExpr $ var "y")),
   ("1 > 2", cAp (cAp (cVarExpr $ var ">") (cLitExpr $ intLit 1)) (cLitExpr $ intLit 2)),
   ("x <= y", cAp (cAp (cVarExpr $ var "<=") (cVarExpr $ var "x")) (cVarExpr $ var "y")),
   ("k2 >= k43", cAp (cAp (cVarExpr $ var ">=") (cVarExpr $ var "k2")) (cVarExpr $ var "k43")),
   ("q == z", cAp (cAp (cVarExpr $ var "==") (cVarExpr $ var "q")) (cVarExpr $ var "z")),
   ("f (==) (<) 23", cAp (cAp (cAp (cVarExpr $ var "f") (cVarExpr $ var "==")) (cVarExpr $ var "<")) (cLitExpr $ intLit 23)),
   ("case 2 > k of True -> 6 + 3 False -> f (+) y",
    cCase (cAp (cAp (cVarExpr $ var ">") (cLitExpr $ intLit 2)) (cVarExpr $ var "k"))
    [(cDataAlt (dataCon "True") [] (cAp (cAp (cVarExpr $ var "+") (cLitExpr $ intLit 6)) (cLitExpr $ intLit 3))),
    (cDataAlt (dataCon "False") [] (cAp (cAp (cVarExpr $ var "f") (cVarExpr $ var "+")) (cVarExpr $ var "y")))])]
