-- Scheme FFI Test Module
module Test where

import Scheme.Types
import qualified System.Plugins.Load as PL
import Control.Monad.Error
import Control.Monad.Trans
import Foreign.C.Types(CDouble(..))
--import Foreign.Ptr(FunPtr)


hsSquare :: [Lisp] -> InterpM Lisp
hsSquare [Fixnum a] = do
    mv <- liftIO $ PL.load "t.o" ["."] [] "square"
    case mv of
        PL.LoadFailure msg -> throwError $ Default $ show msg
        PL.LoadSuccess _ v -> do
            let sq = v :: CDouble -> IO CDouble
            x <- liftIO $ sq (fromIntegral a)
            return $ Fixnum $ round x

foo :: [Lisp] -> InterpM Lisp
foo [] = do
  liftIO $ putStrLn "hello haskell"
  return Void
