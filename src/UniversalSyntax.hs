module UniversalSyntax(
  VarName, DataConName, Literal,
  Type,
  var, dataCon, intLit, floatLit, charLit,
  getVarName, getDataConName) where

-- This is a module for syntax elements that are the same across all intermediate
-- representations, from the core syntax to the imperative representation

data Type
  = TypeCon String Type Type
  | TypeVar String
  | Integer
  | Floating
  | Character
    deriving (Eq, Ord, Show)

data Literal
  = IntLit Int
  | FloatLit Double
  | CharLit Char
    deriving (Eq, Ord, Show)

intLit = IntLit
floatLit = FloatLit
charLit = CharLit

data VarName = VarName String
               deriving (Eq, Ord, Show)

var = VarName
getVarName (VarName n) = n

data DataConName = DataConName String
                   deriving (Eq, Ord, Show)

dataCon = DataConName
getDataConName (DataConName n) = n
