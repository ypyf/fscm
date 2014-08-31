module Main
    where

import Scheme.Interp (runREPL, runOnce)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  if null args then runREPL
  else runOnce args
