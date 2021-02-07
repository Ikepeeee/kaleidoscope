module Language.Kaleidoscope.Printer where

import Data.List (intercalate)
import Language.Kaleidoscope.AST

printJS :: AST -> String
printJS (AST fs) = intercalate "\n" $ map printJSFn fs

printJSFn :: (String, Func) -> String
printJSFn (name, Func (Number n) _) = "const " <> name <> " = " <> "() => " <>  show n <> ";"
printJSFn (name, Func (Call name') _) = "const " <> name <> " = " <> "() => " <>  name' <> "()" <> ";"
