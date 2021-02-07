{-# LANGUAGE OverloadedStrings #-}
module Language.Kaleidoscope.Parser where

import Data.Text (Text, unpack)
import Data.Void (Void)
import Data.Functor (($>))
import Data.Scientific (toRealFloat)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

import Language.Kaleidoscope.AST


type Parser = Parsec Void Text

pAST :: Parser AST
pAST = AST <$> many pFunc

pFunc :: Parser (String, Func)
pFunc = do
  name <- tIdent
  sign <- optional $ tColon *> pType
  expr <- tEqual *> pExpr
  return (name, Func expr sign)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

pExpr :: Parser Expr
pExpr = choice
  [ pNumber
  , pVariable
  ]

pType :: Parser Type
pType = braces $ choice
  [ symbol "==" *> (Comp "==" <$> tNumber)
  , symbol ">=" *> (Comp ">=" <$> tNumber)
  , symbol "<=" *> (Comp "<=" <$> tNumber)
  , symbol ">" *> (Comp ">" <$> tNumber)
  , symbol "<" *> (Comp "<" <$> tNumber)
  ]

pNumber :: Parser Expr
pNumber = Number <$> tNumber

pVariable :: Parser Expr
pVariable = Call <$> tIdent

tIdent :: Parser String
tIdent = L.lexeme sc $ (:) <$> lowerChar <*> many alphaNumChar

tNumber :: Parser Double
tNumber = toRealFloat <$> L.lexeme sc (L.signed sc L.scientific)

tColon :: Parser ()
tColon = symbol ":" $> ()

tEqual :: Parser ()
tEqual = symbol "=" $> ()

braces :: Parser a -> Parser a
braces parser = symbol "{" *> parser <* symbol "}"

sc :: Parser ()
sc = L.space
  (skipSome (char ' ' <|> char '\t' <|> char '\n' <|> char '\r'))
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

symbol :: Text -> Parser Text
symbol = L.symbol sc
