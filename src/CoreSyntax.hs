module CoreSyntax(
  CoreModule, CoreDecl, CoreExpr,
  coreModule, coreDecl,
  cVarExpr, cAp, cLitExpr, cDataCon, cCase, cLam, cLet,
  cDataAlt, cLitAlt, cWildCardAlt,
  getName, normalizeCoreDecl) where

import Data.Map as M

import NormalizedSyntax
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
  | CoreLambda VarName CoreExpr
  | CoreLet [CoreDecl] CoreExpr
  | CoreCase CoreExpr [CoreAlts]
    deriving (Eq, Ord, Show)

cLitExpr = CoreLitExpr
cVarExpr = CoreVarExpr
cDataCon = CoreDataCon
cLam = CoreLambda
cLet = CoreLet
cAp = CoreAp
cCase = CoreCase

getName (CoreVarExpr name) = getVarName name

data CoreAlts
  = CoreLitAlt Literal CoreExpr
  | CoreDataAlt DataConName [VarName] CoreExpr
  | CoreWildCardAlt
    deriving (Eq, Ord, Show)

cDataAlt = CoreDataAlt
cLitAlt = CoreLitAlt
cWildCardAlt = CoreWildCardAlt

normalizeCoreDecl :: Int -> CoreDecl -> [NormedDecl]
normalizeCoreDecl s (CoreDecl name l@(CoreLambda _ _)) =
  [nFuncDecl name collectedVars body]
  where
    (collectedVars, e) = collectLambda l
    body = normalizeBody s e
normalizeCoreDecl s (CoreDecl name e) = eDecs ++ [normedEDecl]
  where
    (eDecs, normedE) = normalizeExpr s e
    normedEDecl = nExprDecl name normedE

normalizeBody :: Int -> CoreExpr -> NormedFuncBody
normalizeBody _ _ = nExprBody $ nVarExpr $ var "x"

normalizeExpr :: Int -> CoreExpr -> ([NormedDecl], NormedExpr)
normalizeExpr _ (CoreVarExpr var) = ([], nVarExpr var)
normalizeExpr _ (CoreLitExpr lit) = ([], nLitExpr lit)

collectLambda :: CoreExpr -> ([VarName], CoreExpr)
collectLambda expr = recCollectLambda expr []

recCollectLambda :: CoreExpr -> [VarName] -> ([VarName], CoreExpr)
recCollectLambda (CoreLambda v e) vars = recCollectLambda e (v:vars)
recCollectLambda e vars = (reverse vars, e)
