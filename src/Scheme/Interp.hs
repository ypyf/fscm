module Scheme.Interp (defaultInterp, defaultEnv, runInterp, runREPL, runOnce) where

import Scheme.Types
import Scheme.Primitives

--import System.Posix.Signals
--import Control.Concurrent (forkFinally, )
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Control.Exception.Base
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Cont
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M

-- 缺省存储
emptyStore :: Store
emptyStore = M.empty

-- 缺省环境
defaultEnv :: IO Env
defaultEnv = do
  let env = keywordsBindings ++ primitiveBindings ++ primitiveIoBindings
  lst <- mapM (\(k, v) -> newIORef v >>= \v -> return (k, v)) env
  return $ M.fromList lst
  
--pio_interaction_environment [] = return $ Environment builtins

-- 绑定内置函数(常量)到环境
primitiveBindings :: [(String, Lisp)]
primitiveBindings = [(k, v) | (k, f) <- primitives, let v = Func f]

primitiveIoBindings :: [(String, Lisp)]
primitiveIoBindings = [(k, v) | (k, f) <- primitivesIo, let v = IOFunc f]

-- 语法关键词(特殊形式)
keywordsBindings :: [(String, Lisp)]
keywordsBindings = [(k, v) | (k, f) <- keywords, let v = Syntax f]

-- 以命令行参数方式运行
-- TODO 增加在命令行解释执行的功能
runOnce :: [String] -> IO ()
runOnce args = putStrLn "runOnce Unimplementation!"
-- runOnce args = do
--   -- 初始化环境并将命令行参数绑定到环境中
--   env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
--   -- 从命令行载入文件
--   (runIOThrows $ liftM show $ eval env (List [Symbol "load", String (args !! 0)]))
--   >>= hPutStrLn stderr

{- REPL辅助函数 -}

-- 打印提示符并刷新标准输出缓冲区
prompt :: String -> IO ()
prompt = flushStr

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- 运行REPL解释器
runREPL :: IO ()
runREPL = defaultEnv >>= \r -> runInterp r defaultInterp

runInterp :: Env -> InterpM Lisp -> IO ()
runInterp env interp = do
  v <- runErrorT $ runReaderT (runContT interp return) (SC env)
  case v of
    Left e -> putStrLn $ show e
    Right result -> flushStr $ show result

{-
run :: IO ()
run = do
  r <- defaultEnv
  v <- runErrorT $ runReaderT (runContT interp f) (SC r)
  case v of
    Left e -> putStrLn $ show e
    Right result -> putStrLn $ show result
  where
    f (Fixnum a) = return $ Fixnum $ a + 1
    f val = return val
-}

--evalString :: String -> InterpM Lisp
--evalString s = readLisp s >>= evalProc

interp :: InterpM Lisp
interp = do
  x <- readProc [] `catchError` readerErrorHandler
  r <- evalProc [x] `catchError` errorHandler
  case r of
    Void -> return Void
    _    -> do
      liftIO $ putStrLn $ show r
      return Void

-- TODO 使用Scheme本身去写REPL
defaultInterp :: InterpM Lisp
defaultInterp = do
  loadProc [String "stdlib.scm"] `catchError` loaderErrorHandler
  forever interp

errorHandler :: LispError -> InterpM Lisp
errorHandler e = (liftIO $ putStrLn $ show e) >> return Void

loaderErrorHandler :: LispError -> InterpM Lisp
loaderErrorHandler e = (liftIO $ putStrLn $ "load: " ++ show e) >> return Void

readerErrorHandler :: LispError -> InterpM Lisp
readerErrorHandler e = (liftIO $ putStrLn $ "read: " ++ show e) >> return Void

-- TODO 处理Haskell内置异常
handler :: ArithException -> IO ()
handler DivideByZero = putStrLn "Divide by Zero!"
handler _ = putStrLn "Some other error..."
