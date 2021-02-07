module Language.Kaleidoscope.TypeChecker where

import Data.Maybe (mapMaybe, isNothing)
import Control.Monad.State
import Data.SBV ((.&&), SDouble, Symbolic, forall, sDouble, OrdSymbolic, EqSymbolic, SBool, isSatisfiable, literal, (.=>), (.==), (.>=), (.<=), (.<), (.>), output, fpNeg)
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

solve :: [(String, SDouble -> SBool)] -> Funcs -> [TypeError]
solve types fns = lefts $ map (solveFn fns types) fns

solveFn :: Funcs -> [(String, SDouble -> SBool)] -> (String ,Func) -> Either TypeError ()
solveFn fns types (name, Func expr Nothing) = pure ()
solveFn fns types (name, Func expr (Just t)) = do
  condition <- expr `match'` t
  if unsafePerformIO $ isSatisfiable condition
    then pure ()
    else Left $ NotMatchError name
  where
    match' = match fns types

match :: Funcs -> [(String, SDouble -> SBool)] -> Expr -> Type -> Either TypeError (Symbolic SBool)
match _ types (Call name) (Comp op tn) = do
  t <- maybeToRight (NotDefinedError name) (lookup name types)
  pure $ do
    x <- forall name
    pure $ t x .=> compOp op x (literal tn)
match fns types expr (Comp op tn) = do
  t <- maybeToRight (NotDefinedError "oh no") (fromExpr fns expr)
  pure $ pure $ compOp op t (literal tn)

infer :: Funcs -> [(String, SDouble -> SBool)]
infer fs = mapMaybe (inferFn fs) fs

inferFn :: Funcs -> (String, Func) -> Maybe (String, SDouble -> SBool)
inferFn _ (name, Func _ (Just (Comp op tn))) = Just (name, compOp op (literal tn))
inferFn fns (name, Func expr Nothing) = (,) name <$> inferExpr fns expr

inferExpr :: Funcs -> Expr -> Maybe (SDouble -> SBool)
inferExpr fns expr = (.==) <$> fromExpr fns expr

fromExpr :: Funcs -> Expr -> Maybe SDouble
fromExpr _ (Number n) = pure $ literal n
fromExpr fns (Negation a) = fpNeg <$> fromExpr fns a
fromExpr fns (Sum a b) = (+) <$> fromExpr fns a <*> fromExpr fns b
fromExpr fns (Sub a b) = (-) <$> fromExpr fns a <*> fromExpr fns b
fromExpr fns (Product a b) = (-) <$> fromExpr fns a <*> fromExpr fns b
fromExpr fns (Division a b) = (-) <$> fromExpr fns a <*> fromExpr fns b
fromExpr fns (Call name) = do
  (Func expr _) <- lookup name fns
  fromExpr fns expr

compOp :: OrdSymbolic a => String -> a -> a -> SBool
compOp "==" = (.==)
compOp ">=" = (.>=)
compOp "<=" = (.<=)
compOp ">" = (.>)
compOp "<" = (.<)

maybeToRight :: b -> Maybe a -> Either b a
maybeToRight e Nothing = Left e
maybeToRight _ (Just a) = Right a
