module Lexer(
  Token,
  strToToks,
  dVar, dCon, dInt, dFloat, dChar, dlp, drp, dRes,
  isBuiltinOp, nameVal, intVal, floatVal, charVal,
  isInt, isFloat, isChar,
  isVarName, isDataConName, isName, pos) where

import Text.ParserCombinators.Parsec
import Text.Parsec.Pos
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Token as Tok

languageDef =
  emptyDef { Tok.commentStart    = "/*",
             Tok.commentEnd      = "*/",
             Tok.commentLine     = "//",
             Tok.identStart      = lower,
             Tok.identLetter     = alphaNum,
             Tok.reservedNames   = ["\\", "->", "=", "case", "of", "module", "where",
                                    "let", "in", "data"],
             Tok.reservedOpNames = ["+", "-", "*", "/", "==", "<", ">", "<=", ">=",
                                    "||", "&&", "~"] }

lexer = Tok.makeTokenParser languageDef

data Token
     = VarName String SourcePos
     | DataConstructorName String SourcePos
     | FloatNum Double SourcePos
     | IntNum Int SourcePos
     | Character Char SourcePos
     | Res String SourcePos
     | Delim String SourcePos
       deriving (Show)
                
instance Eq Token where
  (==) (VarName n1 _) (VarName n2 _) = n1 == n2
  (==) (DataConstructorName n1 _) (DataConstructorName n2 _) = n1 == n2
  (==) (IntNum n1 _) (IntNum n2 _) = n1 == n2
  (==) (FloatNum n1 _) (FloatNum n2 _) = n1 == n2
  (==) (Character n1 _) (Character n2 _) = n1 == n2
  (==) (Res n1 _) (Res n2 _) = n1 == n2
  (==) (Delim n1 _) (Delim n2 _) = n1 == n2
  (==) _ _ = False

pos :: Token -> SourcePos
pos (VarName _ p) = p
pos (DataConstructorName _ p) = p
pos (IntNum _ p) = p
pos (FloatNum _ p) = p
pos (Character _ p) = p
pos (Res _ p) = p
pos (Delim _ p) = p

isName (VarName _ _) = True
isName (DataConstructorName _ _) = True
isName _ = False

isVarName (VarName _ _) = True
isVarName _ = False

isDataConName (DataConstructorName _ _) = True
isDataConName _ = False

isInt (IntNum _ _) = True
isInt _ = False

isFloat (FloatNum _ _) = True
isFloat _ = False

isChar (Character _ _) = True
isChar _ = False

nameVal (VarName n _) = n
nameVal (DataConstructorName n _) = n

intVal (IntNum v _) = v
floatVal (FloatNum v _) = v
charVal (Character c _) = c

builtinOps = ["==", ">", "<", ">=", "<=", "~", "+", "-", "*", "/", "&&", "||"]

isBuiltinOp (VarName n _) = elem n builtinOps
isBuiltinOp _ = False

varName = VarName
dataConstructorName = DataConstructorName
intNum = IntNum
floatNum = FloatNum
delimiter = Delim
res = Res

-- Dummy token creation functions
dummyPos = newPos "DUMMY" 0 0

dVar str = VarName str dummyPos
dCon str = DataConstructorName str dummyPos
dInt val = IntNum val dummyPos
dFloat val = FloatNum val dummyPos
dChar val = Character val dummyPos

dlp = Delim "(" dummyPos
drp = Delim ")" dummyPos

dRes name = Res name dummyPos
-------------------------------------

strToToks :: String -> [Token]
strToToks str = case parse (sepBy tok spaces) "Lexer" str of
  Left err -> error $ show err
  Right toks -> toks

tok :: Parser Token
tok = varOrResword
      <|> number
      <|> delim
      <|> resSymbolOrBuiltinOp
      <|> dataConstructor

varOrResword :: Parser Token
varOrResword = try var <|> resWord

resWord :: Parser Token
resWord = do
  pos <- getPosition
  resStr <- string "case"
            <|> string "of"
            <|> string "module"
            <|> string "where"
            <|> string "let"
            <|> string "data"
            <|> string "in"
  return $ res resStr pos

dataConstructor :: Parser Token
dataConstructor = do
  pos <- getPosition
  first <- upper
  rest <- many alphaNum
  return $ dataConstructorName (first:rest) pos
  
parseVarName = Tok.identifier lexer

var :: Parser Token
var = do
  pos <- getPosition
  first <- parseVarName
  return $ varName first pos
  
oneCharVar :: Parser String
oneCharVar = do
  x <- lower
  return $ (x:[])

manyCharVar :: Parser String
manyCharVar = do
  x <- lower
  rest <- many1 alphaNum
  return $ (x:rest)
  
number :: Parser Token
number = do
  pos <- getPosition
  nums <- many1 digit
  return $ intNum (read nums :: Int) pos
  
delim :: Parser Token
delim = do
  pos <- getPosition
  d <- string "(" 
       <|> string ")"
  return $ delimiter d pos

resSymbolOrBuiltinOp :: Parser Token
resSymbolOrBuiltinOp = do
  pos <- getPosition
  op <- try (string "==")
        <|> try (string "<=")
        <|> try (string ">=")
        <|> try (string "->")
        <|> string "="
        <|> string "-"
        <|> string "<"
        <|> string ">"
        <|> string "+" 
        <|> string "*"
        <|> string "/"
        <|> string "||"
        <|> string "&&"
        <|> string "~"
        <|> string "\\"
  case op of
    "=" -> return $ res op pos
    "->" -> return $ res op pos
    "\\" -> return $ res op pos
    _ -> return $ varName op pos


