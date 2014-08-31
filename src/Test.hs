-- Scheme FFI Test Module
module Test where

import Scheme.Types
import Control.Monad.Trans

foo :: [Lisp] -> InterpM Lisp
foo [] = do
  liftIO $ putStrLn "hello haskell"
  return Void
