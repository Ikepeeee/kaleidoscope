module Language.Kaleidoscope.AST where

newtype AST = AST [(String, Func)] deriving (Show)

type NamedFunc = (String, Func)
data Func = Func Expr (Maybe Type) deriving (Show)

data Type
  = Comp String Double
  deriving (Show)

data Expr
  = Call String
  | Number Double
  | Negation Expr
  | Sum      Expr Expr
  | Sub    Expr Expr
  | Product  Expr Expr
  | Division Expr Expr
  deriving (Eq, Ord, Show)
