module Parser(
  parseModule,
  parseExpr) where

import Data.List as L
import Data.Map as M
import Text.Parsec.Expr
import Text.ParserCombinators.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim

import Lexer as Lex
import CoreSyntax as Syn
import UniversalSyntax

parseModule :: [Token] -> CoreModule
parseModule toks = case parse asfModule "Module Parser" toks of
  Left err -> error $ show err
  Right p -> p

asfModule = do
  moduleTok
  moduleName <- dataConNameTok
  return $ coreModule (getDataConName moduleName) [] M.empty
  
parseExpr :: [Token] -> CoreExpr
parseExpr toks = case parse expr "Expr Parser" toks of
  Left err -> error $ show err
  Right expression -> expression
  
expr = buildExpressionParser table term

table =
  [[logicalNegation],
   [multiplication, division],
   [addition, subtraction],
   [lessThan, greaterThan, lessThanOrEqual, greaterThanOrEqual],
   [equal],
   [logicalAnd],
   [logicalOr]]
  
-- Arithmetic builtins
addition = Infix (binaryOp "+") AssocLeft
subtraction = Infix (binaryOp "-") AssocLeft
multiplication = Infix (binaryOp "*") AssocLeft
division = Infix (binaryOp "/") AssocLeft

-- Logical builtins
logicalNegation = Prefix (unaryOp "~")
logicalAnd = Infix (binaryOp "&&") AssocLeft
logicalOr = Infix (binaryOp "||") AssocLeft

-- Comparison builtins
equal = Infix (binaryOp "==") AssocLeft
lessThan = Infix (binaryOp "<") AssocLeft
greaterThan = Infix (binaryOp ">") AssocLeft
lessThanOrEqual = Infix (binaryOp "<=") AssocLeft
greaterThanOrEqual = Infix(binaryOp ">=") AssocLeft

binaryOp opName = do
  nameTok opName
  return $ bop opName

bop opName arg1 arg2 = cAp (cAp (cVarExpr $ var opName) arg1) arg2

unaryOp opName = do
  nameTok opName
  return $ unop opName
  
unop opName arg = cAp (cVarExpr $ var opName) arg

term = parens expr
       <|> funcAp
       <|> numberTok
       <|> varNameTok
       <|> dataConstructorCall
       <|> caseExpr

caseExpr = do
  caseTok
  mainExpr <- expr
  ofTok
  alternatives <- many1 alternative
  return $ cCase mainExpr alternatives

alternative = intLitAlt <|> dataAlt

intLitAlt = do
  integerVal <- intTok
  arrowTok
  res <- expr
  return $ cLitAlt (intLit $ intVal integerVal) res

dataAlt = do
  dcName <- dataConNameTok
  vars <- many varNameTok
  arrowTok
  res <- expr
  return $ cDataAlt dcName (L.map (var . getName) vars) res

funcArg = try (parens builtinOperator)
          <|> try (parens expr)
          <|> nullaryDataConstructorCall
          <|> numberTok
          <|> varNameTok
          <|> parens dataConstructorCall

parens e = do
  lParen
  x <- e
  rParen
  return x

numberTok = do
  nt <- intTok
  return $ cLitExpr $ intLit (intVal nt)
  
varNameTok = do
  t <- varTok
  return $ cVarExpr $ var $ nameVal t

nullaryDataConstructorCall = do
  nameOfConstructor <- dataConNameTok
  return $ cDataCon nameOfConstructor []

dataConstructorCall = do
  dcName <- dataConNameTok
  args <- many funcArg
  return $ cDataCon dcName args

dataConNameTok = do
  t <- dataConTok
  return $ dataCon $ nameVal t

builtinOperator = do
  t <- builtinOpTok
  return $ cVarExpr $ var $ nameVal t
  
funcAp = do
  funcName <- anyNameTok
  args <- many funcArg
  let fName = nameVal funcName
  return $ application (cVarExpr $ var fName) args
  
application :: CoreExpr -> [CoreExpr] -> CoreExpr
application e [] = e
application e (x:xs) = application (cAp e x) xs

lParen = ilTok (== dlp)
rParen = ilTok (== drp)

-- Reserved word tokens
moduleTok = ilTok (== (dRes "module"))
ofTok = ilTok (== (dRes "of"))
caseTok = ilTok (== (dRes "case"))
arrowTok = ilTok (== (dRes "->"))

anyNameTokOtherThan forbiddenNames = ilTok (\t -> isName t && (not $ Prelude.elem t forbiddenNames))

anyNameTok :: (Monad m) => ParsecT [Token] u m Token
anyNameTok = ilTok (\t -> isVarName t && (not $ isBuiltinOp t))

varTok = ilTok (\t -> isVarName t && (not $ isBuiltinOp t))

builtinOpTok = ilTok isBuiltinOp

dataConTok :: (Monad m) => ParsecT [Token] u m Token
dataConTok = ilTok isDataConName

intTok :: (Monad m) => ParsecT [Token] u m Token
intTok = ilTok isInt

nameTok :: (Monad m) => String -> ParsecT [Token] u m Token
nameTok name = ilTok (\t -> isName t && nameVal t == name)

ilTok :: (Monad m) => (Token -> Bool) -> ParsecT [Token] u m Token
ilTok condition = tokenPrim show updatePos meetsCond
  where
    meetsCond t = if condition t then Just t else Nothing

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos position _ [] = position
