module Scheme.Eval (eval, evalqq, evalTail, apply) where

import Control.Monad.Except
import Control.Monad.Reader
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Scheme.Types

eval :: LispVal -> InterpM LispVal
eval (Values []) = return Undefined
eval (Values vals) = fmap last (mapM eval vals)
eval (Symbol name) = do
  r <- ask
  case M.lookup name r of
    Nothing -> throwError $ UnboundName name
    Just x -> do
      v <- liftIO $ readIORef x
      case v of
        Undefined -> throwError $ UnboundName name
        _ -> return v

-- 定义闭包(lambda abstract) call by value
-- 定义时检查重复的形式参数名
eval form@(List [Symbol "lambda", _]) = throwError $ BadSpecialForm "no expression in body" form
-- 固定参数
-- (lambda (x ...) ...)
eval (List (Symbol "lambda" : List params : body)) =
  asks (Closure (map showVal params) Nothing body)
-- 可变参数
-- (lambda x ...)
eval (List (Symbol "lambda" : Symbol vararg : body)) = asks (Closure [] (Just vararg) body)
-- 可变参数
-- (lambda (x y . z) ...)
eval (List (Symbol "lambda" : dl@(DotList params (Symbol vararg)) : body)) =
  asks (Closure (map showVal params) (Just vararg) body)
eval form@(DotList s0 s1) = throwError $ Default $ "illegal use of `.' in: " ++ show form
-- function apply
eval (List []) = throwError $ Default "missing procedure expression"
eval (List (x : xs)) = do
  fn <- eval x
  case fn of
    Syntax _ -> apply fn xs -- special form 不对参数求值
    _ -> mapM eval xs >>= apply fn
eval val = return val

-- 用于尾部表达式的求值(尾调用优化)
evalTail :: LispVal -> InterpM LispVal
evalTail expr@(List (Symbol "lambda" : xs)) = eval expr
evalTail expr@(List (x : xs)) = do
  fn <- eval x
  case fn of
    Syntax _ -> apply fn xs -- special form 不对参数求值
    _ -> mapM eval xs >>= applyTail fn
evalTail expr = eval expr

-- apply
-- 函数应用前必须先对参数求值
-- 语法关键词（特殊形式）不对参数求值
apply :: LispVal -> [LispVal] -> InterpM LispVal
apply (Syntax s) args = s args
apply (HFunc func) args = func args
apply (IOFunc func) args = func args
apply (Func fn) args =
  case fn args of
    Left e -> throwError e -- 再次抛出,提升ThrowsError
    Right a -> return a
apply (Continuation cont) args =
  case args of
    [] -> cont Undefined
    [x] -> cont x
    _ -> cont $ Values args
apply (TailCall a b c d) args = apply (Closure a b c d) args
apply (Closure params varargs body closure) args = do
  let nargs = length params
  if length args /= nargs && isNothing varargs
    then throwError $ NumArgs nargs args
    else do
      locals <- liftIO $ localEnv nargs varargs
      let ctx = M.union locals closure
      val <- local (M.union ctx) $ evalBody body
      case val of
        List (TailCall a b c d : args) -> apply (Closure a b c d) args
        Values vals -> fmap last (mapM eval vals)
        _ -> return val
  where
    localEnv :: Int -> Maybe String -> IO Env
    localEnv _ Nothing = fmap (M.fromList . zip params) (mapM (liftIO . newIORef) args)
    localEnv nargs (Just argName) = do
      v0 <- liftIO $ mapM newIORef args
      v1 <- liftIO $ newIORef (List $ drop nargs args)
      return $ M.fromList $ zip params v0 ++ [(argName, v1)]
    evalBody :: [LispVal] -> InterpM LispVal
    evalBody [] = undefined
    evalBody [x] = evalTail x
    evalBody (x : xs) = eval x >> evalBody xs
apply given args =
  throwError $ Default $ "expected procedure, given: " ++ show given ++ "; arguments were: " ++ unwordsList args

applyTail :: LispVal -> [LispVal] -> InterpM LispVal
-- 尾部的函数应用只对参数在当前作用域内求值，然后返回尾调用对象
-- 只有自定义函数采用尾调用优化
applyTail (Closure a b c d) args = return $ List $ TailCall a b c d : args
applyTail proc args = apply proc args

-- eval quasiquote
evalqq :: Int -> LispVal -> InterpM LispVal
evalqq _ (List [Symbol "unquote-splicing", _]) = throwError $ Default "unquote-splicing: invalid context within quasiquote"
evalqq level v@(List [Symbol "quasiquote", expr]) = evalqq' level v
evalqq level v@(List [Symbol "unquote", expr]) = evalqq' level v
evalqq level (List lst) = do
  r <- mapM (evalqq' level) lst
  return $ List $ concatMap splicing r
  where
    splicing :: LispVal -> [LispVal]
    splicing (Slice s) = s
    splicing x = [x]
evalqq level expr = evalqq' level expr

evalqq' :: Int -> LispVal -> InterpM LispVal
evalqq' level v@(List [Symbol "quasiquote", expr]) = do
  v' <- evalqq' (level + 1) expr
  return $ List [Symbol "quasiquote", v']
evalqq' level v@(List [Symbol "unquote", expr])
  | level == 0 = eval expr
  | otherwise = do
      v' <- evalqq' (level - 1) expr
      case v' of
        Slice s -> return $ List $ Symbol "unquote" : s
        _ -> return $ List [Symbol "unquote", v']
evalqq' level v@(List [Symbol "unquote-splicing", expr])
  | level == 0 = eval expr >>= splice
  | otherwise = do
      v' <- evalqq' (level - 1) expr
      case v' of
        Slice s -> return $ List $ Symbol "unquote-splicing" : s
        _ -> return $ List [Symbol "unquote-splicing", v']
  where
    splice :: LispVal -> InterpM LispVal
    splice (List lst) = return $ Slice lst
    splice notList = throwError $ TypeMismatch "list?" notList
evalqq' level expr@(List _) = evalqq level expr
evalqq' _ expr = return expr
