module NormalizedSyntax() where

import UniversalSyntax

data NormalizedModule = NormalizedModule DataConName [NormedDecl]

data NormedDecl
  = NormedExprDecl VarName NormedExpr
  | NormedDataDecl VarName DataConName [NormedExpr]
  | NormedFuncDecl VarName [VarName] NormedFuncBody

data NormedFuncBody
  = NormedLetBody [NormedDecl] NormedFuncBody
  | NormedCase VarName [NormedAlt]
  | NormedExprBody NormedExpr
  | NormedFail

data NormedExpr
  = NormedVarExpr VarName
  | NormedAp VarName [NormedExpr]
  | NormedLitExpr Literal
  | NormedSingletonData DataConName

data NormedAlt
  = NormedLitAlt Literal NormedFuncBody
  | NormedDataAlt DataConName [VarName] NormedFuncBody
  | NormedWildCardAlt
