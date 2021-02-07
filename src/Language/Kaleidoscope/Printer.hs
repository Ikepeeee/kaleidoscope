module Language.Kaleidoscope.Printer where

import Data.List (intercalate)
import Language.Kaleidoscope.AST

printJS :: AST -> String
printJS (AST fs) = intercalate "\n" $ map printJSFn fs

printJSFn :: (String, Func) -> String
printJSFn (name, Func expr _) = "const " <> name <> " = " <> "() => " <>  printExpr expr <> ";"

printExpr :: Expr -> String
printExpr (Number n) = show n
printExpr (Negation expr) = "(" <> "-" <> printExpr expr <> ")"
printExpr (Sum a b) = "(" <> printExpr a <> " + " <> printExpr b <> ")"
printExpr (Sub a b) = "(" <> printExpr a <> " - " <> printExpr b <> ")"
printExpr (Product a b) = "(" <> printExpr a <> " * " <> printExpr b <> ")"
printExpr (Division a b) = "(" <> printExpr a <> " / " <> printExpr b <> ")"
printExpr (Call name') = name' <> "()"
