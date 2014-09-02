module Scheme.Eval (eval, eval_tail) where

import Scheme.Types
import Scheme.Variables

import Debug.Trace
import Data.IORef
import Data.List
import Control.Monad.Reader
import Control.Monad.Trans.Cont
import Control.Monad.Error
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M


eval :: Lisp -> InterpM Lisp
eval (Symbol name) = do
  r <- ask
  case getValue name r of
    Nothing -> throwError $ UnboundName name
    Just x  -> liftIO $ readIORef x

-- 定义闭包(lambda abstract) call by value
-- 定义时检查重复的形式参数名
eval form@(List [Symbol "lambda", _]) = throwError $ BadSpecialForm "no expression in body" form

-- 固定参数
-- (lambda (x ...) ...)
eval (List (Symbol "lambda":List s:body)) = do
  let nargs = length s -- 形参个数
  upvalues <- ask
  let params = map show s  -- 形参表
  return $ Lambda $ \v ->
    if length v /= nargs then throwError $ NumArgs nargs v
    else do
      v' <- mapM (liftIO . newIORef) v
      let locals = M.fromList $ zip params v'
      mkClosure locals upvalues body

-- 可变参数
-- (lambda x ...)
-- (lambda (. x) ...) => (lambda x ...) 这种形式在parse阶段已经被转换
eval (List (Symbol "lambda":(Symbol s):body)) = do
  upvalues <- ask
  return $ Lambda $ \v -> do
    v' <- liftIO $ newIORef (List v)
    let locals = M.fromList [(s, v')]
    mkClosure locals upvalues body


-- 可变参数
-- (lambda (x y . z) ...)
eval (List (Symbol "lambda":dl@(DotList s0 s1):body)) = do
  let nargs = length s0 -- 固定位置的形参个数
      params = map show s0 -- 固定形参名表
      vararg = show s1    -- 固定参数形参名
  upvalues <- ask
  return $ Lambda $ \v ->
      if length v < nargs then throwError $ NumArgs nargs v
      else do
        v0 <- liftIO $ sequence $ map newIORef v
        v1 <- liftIO $ newIORef (List $ drop nargs v)
        let locals = M.fromList $ (zip params v0) ++ [(vararg, v1)]
        mkClosure locals upvalues body


-- apply
-- 函数应用前必须先对参数求值
-- 语法关键词（特殊形式）不对参数求值
eval form@(DotList s0 s1) = throwError $ Default $ "illegal use of `.' in: " ++ show form

-- apply
eval (List []) = throwError $ Default "missing procedure expression."
eval (List (x:xs)) = eval x >>= \fn -> apply (fn:xs)
eval val = return val
--eval form = throwError $ BadSpecialForm "unrecognized special form" form

mkClosure :: M.Map String (IORef Lisp)
            -> Context
            -> [Lisp]
            -> InterpM Lisp
mkClosure locals upvalues body = do
  --let keys = M.keys upvalues
  --trace (show keys) $ do
  ret <- local f $ closure body
  case ret of
    List (TailCall func:v) -> func v
    _                      -> return ret
  where
    f (SC r) = TC (SC locals) upvalues (SC r)
    f r@(TC _ _ _) = TC (SC locals) upvalues r   -- 注意环境合并的顺序
    closure :: [Lisp] -> InterpM Lisp
    closure [x] = eval_tail x
    closure (x:xs) = eval x >> closure xs


-- 用于尾部表达式的求值(尾递归优化)
eval_tail :: Lisp -> InterpM Lisp
eval_tail arg@(List (Symbol "lambda":_)) = eval arg
eval_tail (List (x:xs)) = eval x >>= \fn -> apply_tail (fn:xs)
eval_tail x = eval x


apply :: [Lisp] -> InterpM Lisp
apply (IOFunc func:xs) = mapM eval xs >>= func
apply (Lambda func:xs) = mapM eval xs >>= func
apply (HFunc func:xs)  = mapM eval xs >>= func
apply (Syntax f:xs) = f xs
apply (Failure failure:xs) = mapM eval xs >>= \[String message, Continuation ok] -> failure message ok
apply (Continuation k:xs) = do
  v <- mapM eval xs
  case v of
    []  -> k Void
    [x] -> k x
    -- 多值 FIXME
    xs  -> mapM (liftM k . eval) xs >> return Void
apply (Func fn:xs) = do
  v <- mapM eval xs
  case fn v of
    Left e -> throwError e  -- 再次抛出,提升ThrowsError
    Right res -> return res
apply (x:xs) = do
  mapM eval xs
  throwError $ Default $ "expected procedure, given: " ++ show x ++ "; arguments were: " ++ unwordsList xs


apply_tail :: [Lisp] -> InterpM Lisp
--apply_tail (Lambda func:xs) = do
--    rx <- mapM eval xs
--    callCC $ \k -> func rx >>= k   --
apply_tail (Lambda func:xs) = mapM eval xs >>= \v -> return $ List $ TailCall func : v
apply_tail exps = apply exps

