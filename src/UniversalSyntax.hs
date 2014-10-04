module UniversalSyntax(
  VarName, DataConName, Literal) where

-- This is a module for syntax elements that are the same across all intermediate
-- representations, from the core syntax to the imperative representation

data Literal
  = IntLit
  | FloatLit Double
  | CharLit Char

data VarName = VarName String

data DataConName = DataConName String
