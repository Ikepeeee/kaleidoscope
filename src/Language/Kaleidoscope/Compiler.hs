module Language.Kaleidoscope.Compiler where

import System.FilePath.Posix (replaceExtension)
import qualified Data.Text as DT
import Text.Megaparsec (parse)
import Data.List (intercalate)
import Text.Megaparsec.Error
import Language.Kaleidoscope.Parser (pAST)
import Language.Kaleidoscope.TypeChecker (check)
import Language.Kaleidoscope.Printer (printJS)

import Debug.Trace (trace)

trace' :: (Show a) => String -> a -> a
trace' m a = trace (m <> show a) a

-- TODO compile Kaleidoscope source to JavaScript code

compile :: [(FilePath, DT.Text)] -> Either String [(FilePath, DT.Text)]
compile [(fileName, src)] = case parse pAST fileName src of
    Right ast -> case check ast of
      [] -> Right [(replaceExtension fileName "js", DT.pack $ printJS ast)]
      es -> Left $ show es
    Left e -> Left $ errorBundlePretty e
