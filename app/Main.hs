-- usage: stack runghc app/Main.hs [src file]
module Main where

import System.IO
import System.Environment (getArgs)
import qualified Data.Text.IO as DTI
import Language.Kaleidoscope.Compiler (compile)

main :: IO ()
main = do
  [fileName] <- getArgs
  src <- DTI.readFile fileName
  case compile [(fileName, src)] of
    Right [(distFile, dist)] -> DTI.writeFile distFile dist
    Left e -> hPutStrLn stderr e
  return ()
