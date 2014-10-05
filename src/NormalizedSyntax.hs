module NormalizedSyntax(
  NormedDecl, NormedExpr, NormedFuncBody, NormedAlt,
  nFuncDecl, nExprDecl,
  nVarExpr, nExprBody, nLitExpr) where

import UniversalSyntax

data NormalizedModule = NormalizedModule DataConName [NormedDecl]
                        deriving (Eq, Ord, Show)

data NormedDecl
  = NormedExprDecl VarName NormedExpr
  | NormedDataDecl VarName DataConName [NormedExpr]
  | NormedFuncDecl VarName [VarName] NormedFuncBody
    deriving (Eq, Ord, Show)

nFuncDecl = NormedFuncDecl
nExprDecl = NormedExprDecl

data NormedFuncBody
  = NormedLetBody [NormedDecl] NormedFuncBody
  | NormedCase VarName [NormedAlt]
  | NormedExprBody NormedExpr
  | NormedFail
    deriving (Eq, Ord, Show)

nExprBody = NormedExprBody

data NormedExpr
  = NormedVarExpr VarName
  | NormedAp VarName [NormedExpr]
  | NormedLitExpr Literal
  | NormedUnaryData DataConName
    deriving (Eq, Ord, Show)

nVarExpr = NormedVarExpr
nLitExpr = NormedLitExpr

data NormedAlt
  = NormedLitAlt Literal NormedFuncBody
  | NormedDataAlt DataConName [VarName] NormedFuncBody
  | NormedWildCardAlt
    deriving (Eq, Ord, Show)
