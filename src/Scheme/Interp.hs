module Scheme.Interp (defaultEnv, runInterp, evalString, runREPL, runOnce) where

import Control.Exception.Base
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Data.IORef
import qualified Data.Map.Strict as M
import Scheme.Primitives
import Scheme.Types
import System.IO

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
runREPL :: String -> IO ()
runREPL str = defaultEnv >>= \e -> runInterp e repl
  where
    repl :: InterpM LispVal
    repl = do
      loadStdlib `catchError` loaderErrorHandler
      forever $ liftIO (prompt str) >> interp
    interp :: InterpM LispVal
    interp = do
      -- TODO 检查语法后再调用readLisp
      x <- readProc [] `catchError` readerErrorHandler
      r <- evalProc [x] `catchError` errorHandler
      case r of
        Undefined -> return Undefined
        List (TailCall a b c d : args) -> evalProc [r]
        _ -> liftIO (print r) >> return Undefined

runInterp :: Env -> InterpM LispVal -> IO ()
runInterp env interp = do
  v <- runExceptT $ runReaderT (evalContT interp) env
  case v of
    Left err -> hPrint stderr err
    Right r -> return ()

-- 以命令行参数方式运行
-- 显示版本号 fscm -v
-- 执行文件中的程序 fscm path/to/program
-- 执行程序前不加载标准库 fscm -x path/to/program
-- 执行单行代码 fscm -e "program"
runOnce :: [String] -> IO ()
runOnce ("-v" : _) = putStrLn "FSCM version 0.1.1"
runOnce ("-x" : path : _) = defaultEnv >>= \e -> runInterp e $ loadProc [String path]
runOnce ("-e" : expr : _) = defaultEnv >>= \e -> runInterp e $ loadStdlib >> evalPrint expr
runOnce [path] = defaultEnv >>= \e -> runInterp e $ loadStdlib >> loadProc [String path]
runOnce args = putStrLn $ "Invalid Options: " ++ show args

loadStdlib :: InterpM LispVal
loadStdlib = loadProc [String "stdlib.scm"]

evalPrint :: String -> InterpM LispVal
evalPrint expr = do
  r <- evalString [String expr]
  case r of
    Undefined -> return Undefined
    _ -> liftIO (print r) >> return Undefined

errorHandler :: LispError -> InterpM LispVal
errorHandler e = liftIO (print e) >> return Undefined

loaderErrorHandler :: LispError -> InterpM LispVal
loaderErrorHandler e = liftIO (putStrLn $ "load: " ++ show e) >> return Undefined

readerErrorHandler :: LispError -> InterpM LispVal
readerErrorHandler e = liftIO (putStrLn $ "read: " ++ show e) >> return Undefined

-- TODO 处理Haskell内置异常
handler :: ArithException -> IO ()
handler DivideByZero = putStrLn "Divide by Zero!"
handler _ = putStrLn "Some other error..."
