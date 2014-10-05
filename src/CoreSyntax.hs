module CoreSyntax(
  CoreModule, CoreDecl,
  coreModule, coreDecl,
  cVarExpr) where

import Data.Map as M

import UniversalSyntax

data CoreModule = CoreModule DataConName [CoreDecl] (Map DataConName Type)
                  deriving (Eq, Ord, Show)

coreModule = coreModule

data CoreDecl
  = CoreDecl VarName CoreExpr
    deriving (Eq, Ord, Show)

coreDecl = CoreDecl

data CoreExpr
  = CoreVarExpr VarName
  | CoreDataCon DataConName [CoreExpr]
  | CoreLitExpr Literal
  | CoreAp CoreExpr CoreExpr
  | CoreLambda [VarName] CoreExpr
  | CoreLet [CoreDecl] CoreExpr
  | CoreCase CoreExpr CoreAlts
    deriving (Eq, Ord, Show)

cVarExpr = CoreVarExpr

data CoreAlts
  = CoreLitAlt Literal CoreExpr
  | CoreDataAlt DataConName [VarName] CoreExpr
  | CoreWildCardAlt
    deriving (Eq, Ord, Show)
