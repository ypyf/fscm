module Scheme.Eval (eval, evalqq, evalTail, apply) where

import Scheme.Types

import Debug.Trace
import Data.IORef
import Data.List
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M


eval :: LispVal -> InterpM LispVal
eval (Symbol name) = do
  r <- ask
  case M.lookup name r of
    Nothing -> throwError $ UnboundName name
    Just x  -> liftIO $ readIORef x

-- 定义闭包(lambda abstract) call by value
-- 定义时检查重复的形式参数名
eval form@(List [Symbol "lambda", _]) = throwError $ BadSpecialForm "no expression in body" form

-- 固定参数
-- (lambda (x ...) ...)
eval (List (Symbol "lambda":List params:body)) =
  ask >>= return . Closure (map showVal params) Nothing body

-- 可变参数
-- (lambda x ...)
-- (lambda (. x) ...) => (lambda x ...) 这种形式在parse阶段已经被转换
eval (List (Symbol "lambda":Symbol vararg:body)) = ask >>= return . Closure [] (Just vararg) body


-- 可变参数
-- (lambda (x y . z) ...)
eval (List (Symbol "lambda":dl@(DotList params (Symbol vararg)):body)) =
  ask >>= return . Closure (map showVal params) (Just vararg) body


eval form@(DotList s0 s1) = throwError $ Default $ "illegal use of `.' in: " ++ show form

-- function apply
eval (List []) = throwError $ Default "missing procedure expression"
eval (List (x:xs)) = do
  fn <- eval x
  case fn of
    Syntax _ -> apply fn xs  -- special form 不对参数求值
    _        -> mapM eval xs >>= apply fn

eval val = return val


evalqq :: Int -> LispVal -> InterpM LispVal
evalqq _ (List [Symbol "unquote", expr]) = eval expr
evalqq _ (List [Symbol "unquote-splicing", _]) = throwError $ Default "unquote-splicing: invalid context within quasiquote"
evalqq level (List v) = do
  r <- mapM (evalqq' level) v
  return $ List $ concat $ map f r
  where
    f :: LispVal -> [LispVal]
    f x =
      case x of
        Slice s -> s
        _       -> [x]
evalqq _ expr = return expr

evalqq' :: Int -> LispVal -> InterpM LispVal
evalqq' level v@(List [Symbol "quasiquote", expr]) = evalqq (level+1) v
evalqq' level v@(List [Symbol "unquote-splicing", expr])
  | level == 0 = eval expr >>= splice
  | otherwise = return v
  where
    splice :: LispVal -> InterpM LispVal
    splice (List lst) = return $ Slice lst
    splice notList = throwError $ TypeMismatch "list?" notList
evalqq' level expr@(List _) = evalqq level expr
evalqq' _ expr = return expr

-- 用于尾部表达式的求值(尾调用优化)
evalTail :: LispVal -> InterpM LispVal
evalTail expr@(List (Symbol "lambda":xs)) = eval expr
evalTail expr@(List (x:xs)) = do
  fn <- eval x
  case fn of
    Syntax _ -> apply fn xs  -- special form 不对参数求值
    _        -> mapM eval xs >>= applyTail fn
evalTail expr = eval expr

-- apply
-- 函数应用前必须先对参数求值
-- 语法关键词（特殊形式）不对参数求值
apply :: LispVal -> [LispVal] -> InterpM LispVal
apply (Syntax s) args = s args
apply (HFunc func) args =  func args
apply (IOFunc func) args = func args

apply (Func fn) args = do
  case fn args of
    Left e -> throwError e  -- 再次抛出,提升ThrowsError
    Right a -> return a

apply (Continuation cont) args = do
  case args of
    []  -> cont Undefined
    [x] -> cont x
    --FIXME 多值
    xs  -> mapM_ (fmap cont . eval) xs >> return Undefined

apply (Closure params varargs body closure) args = do
  let nargs = length params
  if length args /= nargs && varargs == Nothing then throwError $ NumArgs nargs args
  else do
    locals <- liftIO $ localBindings nargs varargs
    val <- local (\r->M.union locals closure) $ evalBody body
    case val of
      List (TailCall a b c d:args) -> apply (Closure a b c d) args
      _                            -> return val
    where
      localBindings :: Int -> Maybe String -> IO Env
      localBindings _ Nothing = mapM (liftIO . newIORef) args >>= return . M.fromList . zip params
      localBindings nargs (Just argName) = do
        v0 <- liftIO $ mapM newIORef args
        v1 <- liftIO $ newIORef (List $ drop nargs args)
        return $ M.fromList $ zip params v0 ++ [(argName, v1)]
      evalBody :: [LispVal] -> InterpM LispVal
      evalBody [x] = evalTail x
      evalBody (x:xs) = eval x >> evalBody xs

apply given args =
  throwError $ Default $ "expected procedure, given: " ++ show given ++ "; arguments were: " ++ unwordsList args


applyTail :: LispVal -> [LispVal] -> InterpM LispVal
-- 尾部的函数应用只对参数在当前作用域内求值，然后返回尾调用对象
-- 只有自定义函数采用尾调用优化
applyTail (Closure a b c d) args = return $ List $ TailCall a b c d : args
applyTail proc args = apply proc args
