module Scheme.Interp (defaultEnv, runInterp, evalString, runREPL, runOnce) where

import Scheme.Types
import Scheme.Primitives

import System.IO
import Data.IORef
import Control.Exception.Base
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.Trans.Cont
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M

-- 缺省存储
emptyStore :: Store
emptyStore = M.empty

-- 缺省环境
defaultEnv :: IO Env
defaultEnv = do
  let env = syntaxBindings ++ primitiveBindings ++ primitiveIoBindings
  lst <- mapM (\(k, v) -> newIORef v >>= \v -> return (k, v)) env
  return $ M.fromList lst

--pio_interaction_environment [] = return $ Environment builtins

-- 绑定内置函数(常量)到环境
primitiveBindings :: [(String, LispVal)]
primitiveBindings = [(k, v) | (k, f) <- primitives, let v = Func f]

primitiveIoBindings :: [(String, LispVal)]
primitiveIoBindings = [(k, v) | (k, f) <- primitivesIo, let v = IOFunc f]

-- 语法关键词(特殊形式)
syntaxBindings :: [(String, LispVal)]
syntaxBindings = [(k, v) | (k, f) <- keywords, let v = Syntax f]


prompt :: String -> IO ()
prompt str = putStr str >> hFlush stdout

-- 运行REPL解释器
-- TODO 使用Scheme本身去写REPL
runREPL :: IO ()
runREPL = defaultEnv >>= \r -> runInterp r looper
 where
   looper :: InterpM LispVal
   looper = do
     loadProc [String "stdlib.scm"] `catchError` loaderErrorHandler
     forever $ liftIO (prompt "> ") >> interp
   interp :: InterpM LispVal
   interp = do
     -- TODO 检查语法后再调用readLisp
     x <- readProc [] `catchError` readerErrorHandler
     r <- evalProc [x] `catchError` errorHandler
     case r of
       Void -> return Void
       _    -> liftIO (print r) >> return Void

runInterp :: Env -> InterpM LispVal -> IO ()
runInterp env interp = do
  v <- runExceptT $ runReaderT (evalContT interp) (SC env)
  case v of
    Left e  -> print e  -- 打印错误消息
    Right _ -> return ()

-- just like (eval (read-string str))
evalString :: String -> InterpM LispVal
evalString str = do
  x <- readString [String str]
  r <- evalProc [x]
  case r of
    Void -> return Void
    _    -> liftIO (print r) >> return Void

-- 以命令行参数方式运行
runOnce :: [String] -> IO ()
runOnce [arg] = defaultEnv >>= \r -> runInterp r $ loadProc [String arg]  -- 执行脚本文件
runOnce ("-e":exprs:_) = defaultEnv >>= \r -> runInterp r once
  where
    once :: InterpM LispVal
    once = loadProc [String "stdlib.scm"] >> evalString exprs
runOnce args = putStrLn $ "Invalid Options: " ++ show args


errorHandler :: LispError -> InterpM LispVal
errorHandler e = liftIO (print e) >> return Void

loaderErrorHandler :: LispError -> InterpM LispVal
loaderErrorHandler e = liftIO (putStrLn $ "load: " ++ show e) >> return Void

readerErrorHandler :: LispError -> InterpM LispVal
readerErrorHandler e = liftIO (putStrLn $ "read: " ++ show e) >> return Void

-- TODO 处理Haskell内置异常
handler :: ArithException -> IO ()
handler DivideByZero = putStrLn "Divide by Zero!"
handler _ = putStrLn "Some other error..."
