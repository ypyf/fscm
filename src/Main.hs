module Main
    where

import Scheme.Interp (runREPL, runOnce)
import System.Environment (getArgs)

info :: String
info = "Type (quit) to quit FSCM."

main :: IO ()
main = do
  args <- getArgs
  if null args then putStrLn info >> runREPL "> "
  else runOnce args
