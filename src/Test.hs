{-# LANGUAGE ForeignFunctionInterface #-}
module Test where

import Scheme.Types
import qualified System.Plugins.Load as PL
import Control.Monad.Error
import Control.Monad.Trans
import Foreign.C
--import Foreign.Ptr(FunPtr)

{-
foreign import ccall "square" c_square :: CDouble -> CDouble

hsSquare :: [Lisp] -> InterpM Lisp
hsSquare [Fixnum a] = do
    return $ Fixnum $ round $ c_square (fromIntegral a)
-}
hsSquare :: [Lisp] -> InterpM Lisp
hsSquare [Fixnum a] = do
    mv <- liftIO $ PL.load "t.o" [".", "dist/build/plugintest/plugintest-tmp"] [] "square"
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
