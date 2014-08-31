-- Scheme FFI Test Module
module Test where

import Scheme.Types
import Control.Monad.Trans

foo :: InterpM Lisp
foo = do
  liftIO $ putStrLn "hello ffi"
  return Void
