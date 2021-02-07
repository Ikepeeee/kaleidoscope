module Language.Kaleidoscope.TypeChecker where

import Data.Maybe (mapMaybe, isNothing)
import Control.Monad.State
import Data.SBV ((.&&), SDouble, Symbolic, forall, sDouble, OrdSymbolic, EqSymbolic, SBool, isSatisfiable, literal, (.=>), (.==), (.>=), (.<=), (.<), (.>), output)
import Language.Kaleidoscope.AST
import GHC.IO.Unsafe (unsafePerformIO)
import Data.Functor (($>))
import Data.Either (lefts)

data TypeError
  = NotDefinedError String
  | NotMatchError String
  deriving (Show)

type Types = [(String, Type)]
type Funcs = [(String, Func)]

check :: AST -> [TypeError]
check (AST fns) = solve (infer fns) fns

solve :: Types -> Funcs -> [TypeError]
solve types fns = lefts $ map (solveFn types) fns

solveFn :: Types -> (String ,Func) -> Either TypeError ()
solveFn types (name, Func expr Nothing) = pure ()
solveFn types (name, Func expr (Just t)) = do
  condition <- expr `match'` t
  if unsafePerformIO $ isSatisfiable condition
    then pure ()
    else Left $ NotMatchError name
  where
    match' = match types

match :: Types -> Expr -> Type -> Either TypeError (Symbolic SBool)
match types (Number n) (Comp op tn) = pure $ pure $ literal n <?> literal tn
  where
    (<?>) = compOp op
match types (Call name) (Comp op tn) = do
  (Comp op' tn') <- maybeToRight (NotDefinedError name) (lookup name types)
  pure $ do
    x <- forall name
    pure $ compOp op' x (literal tn') .=> compOp op x (literal tn)

infer :: Funcs -> Types
infer fs = mapMaybe (inferFn fs) fs

inferFn :: Funcs -> (String, Func) -> Maybe (String, Type)
inferFn _ (name, Func _ (Just t)) = Just (name, t)
inferFn fns (name, Func expr Nothing) = (,) name <$> inferExpr fns expr

inferExpr :: Funcs -> Expr -> Maybe Type
inferExpr _ (Number n) = Just $ Comp "==" n
inferExpr fns (Call name) = do
  (Func expr _) <- lookup name fns
  inferExpr fns expr

compOp :: OrdSymbolic a => String -> a -> a -> SBool
compOp "==" = (.==)
compOp ">=" = (.>=)
compOp "<=" = (.<=)
compOp ">" = (.>)
compOp "<" = (.<)

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight e Nothing = Left e
maybeToRight _ (Just a) = Right a
