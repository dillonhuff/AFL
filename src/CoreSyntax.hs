module CoreSyntax() where

import UniversalSyntax

data CoreModule = CoreModule DataConName [CoreDecl]

data CoreDecl
  = CoreDecl VarName CoreExpr

data CoreExpr
  = CoreVarExpr VarName
  | CoreDataCon DataConName [CoreExpr]
  | CoreLitExpr Literal
  | CoreAp CoreExpr CoreExpr
  | CoreLambda VarName CoreExpr
  | CoreLet [CoreDecl] CoreExpr
  | CoreCase CoreExpr CoreAlts

data CoreAlts
  = LitAlt Literal CoreExpr
  | DataAlt DataConName [VarName] CoreExpr
