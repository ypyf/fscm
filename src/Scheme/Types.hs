{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Scheme.Types where

import Data.Ratio
import Control.Monad.Except
import System.IO
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Functor.Identity
import Data.Monoid
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.Trans.Cont


-- | value list helpervalue list helper
unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

type ThrowsError = Either LispError

type Future = LispVal -- 续体的返回类型，但在对象语言中续体从不返回

-- 表达式求值的结果类型
data LispVal
    = Nil
    | Void  -- 有副作用的函数返回的未定义值
    | EOF
    | Symbol String
    | Fixnum Integer
    | String String
    | Char Char
    | LispTrue
    | LispFalse
    | DotList [LispVal] LispVal  -- 非严格表
    | List [LispVal]
    | Vector ![LispVal] -- TODO []换成Array
    | Func ([LispVal] -> ThrowsError LispVal) -- 纯的内置函数
    | IOFunc ([LispVal] -> InterpM LispVal)  -- 有IO副作用的内置函数
    | Lambda ([LispVal] -> InterpM LispVal)  -- 自定义函数 这些类型看似相同，但不应该合并，因为要处理尾递归
    | HFunc ([LispVal] -> InterpM LispVal) -- Haskell函数
    | TailCall ([LispVal] -> InterpM LispVal)  -- 尾调用
    | Continuation (LispVal -> InterpM Future)
    --   | Failure (String -> (LispVal -> InterpM Future) -> InterpM Future) -- 失败继续 参数：(message, error-cont)
    | Syntax ([LispVal] -> InterpM LispVal) -- 特殊语法形式，比如define,if等
    | HPort Handle -- IO函数使用的端口
    | Environment Env -- 环境
    | Module String -- 模块
    | Transformer ([LispVal] -> [LispVal])

instance Show LispVal where show = showVal
--showVal (Ratio x) = show (numerator x) ++ "%" ++ show (denominator x)
showVal :: LispVal -> String
showVal Nil = "nil"
showVal Void = []
showVal (Continuation _) = "#<continuation>"
-- showVal (Failure _) = "#<failure-continuation>"
showVal (Func _) = "#<procedure>"
showVal (IOFunc _) = "#<procedure>"
showVal (Lambda _) = "#<procedure>"
showVal (HFunc _) = "#<hs-procedure>"
showVal (HPort _) = "#<port>"
showVal (Syntax _) = "#<special-form>"
showVal (Transformer _) = "#<transformer>"
-- showVal (Func {name = name, params = args, vararg = varargs, body = body, closure = env}) =
    -- "(" ++ name ++ case argslist of
                     -- []        -> varargslist ++ ")"
                     -- otherwise -> case varargslist of
                                    -- []        -> " " ++ argslist ++ ")"
                                    -- otherwise -> " " ++ argslist ++ " " ++ varargslist ++ ")"
        -- where argslist = unwords (map (filter (/= '"') . show) args)
              -- varargslist = case varargs of
                              -- Just arg -> "[" ++ arg ++ "]"
                              -- Nothing -> []

showVal (Char '\x20') = "#\\space"
showVal (Char '\x0a') = "#\\newline"
--showVal (Char '\x08') = "#\\backspace"
--showVal (Char '\x09') = "#\\tab"
--showVal (Char '\x0c') = "#\\formfeed"
--showVal (Char '\x0d') = "#\\return"
showVal (Char c) = "#\\" ++ [c]
showVal (String s) = show s
showVal (Symbol a) = a
showVal (Fixnum n) = show n
showVal LispTrue = "#t"
showVal LispFalse = "#f"
showVal (List vals) = "(" ++ unwordsList vals ++ ")"
showVal (DotList s0 s1) = "(" ++ unwordsList s0 ++ " . " ++ show s1 ++ ")"
showVal (Module name) = "#<module:" ++ name ++ ">"
showVal x = "#<Lisp Type>"  -- debug only

-- instance Eq Lisp where x == y = eqv' x y

-- eqv' :: Lisp -> Lisp -> Bool
-- eqv' Nil Nil = LispTrue
-- eqv' (Bool arg1) (Bool arg2) = arg1 == arg2
-- eqv' (Fixnum arg1) (Fixnum arg2) = arg1 == arg2
-- eqv' (String arg1) (String arg2) = arg1 == arg2
-- eqv' (Symbol arg1) (Symbol arg2) = arg1 == arg2
-- eqv' (DotList xs x) (DotList ys y) = eqv' (List $ xs ++ [x]) (List $ ys ++ [y])
-- eqv' (List arg1) (List arg2) = (length arg1 == length arg2) && (and $ map eqvPair $ zip arg1 arg2)
    -- where eqvPair (x1, x2) = eqv' x1 x2
-- eqv' _ _ = LispFalse



-- 环境
type Env = M.Map String (IORef LispVal)

-- 环境 | 参数 闭包 环境
data Context = SC !Env | TC Context Context Context

-- 存储
type Store = M.Map String LispVal

instance MonadError e m => MonadError e (ContT r m) where
    throwError = lift . throwError
    m `catchError` h = ContT $ \c -> runContT m c `catchError` \e -> runContT (h e) c

-- 解释器单子
type InterpM = ContT LispVal (ReaderT Context (ExceptT LispError IO))


-- 解释器错误
data LispError
    = NumArgs Int [LispVal]
    | ZeroDivision
    | TypeMismatch String LispVal
    | ParseError String
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundName String
    | Default String
    | RTE !String !(LispVal -> InterpM Future)

showError :: LispError -> String
showError ZeroDivision = "division by zero"
showError (UnboundName name) = "cannot reference undefined identifier: "  ++ name
showError (BadSpecialForm message form) = message ++ ": bad syntax in: " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func

showError (NumArgs paramsNum args)
    | paramsNum <= 1 = "expects " ++ show paramsNum ++ " argument, given " ++ show (length args) ++ ": " ++ unwordsList args
    | otherwise =  "expects " ++ show paramsNum ++ " arguments, given " ++ show (length args) ++ ": " ++ unwordsList args

showError (TypeMismatch expected found) = "Invalid type: expected: " ++ expected ++ ", given: " ++ show found
showError (ParseError message) = "Parse error at " ++ message
showError (Default message) = "Error: " ++ message
showError (RTE message errorConkt) = message

instance Show LispError where show = showError

-- | 空表
emptyList :: LispVal
emptyList = List []

-- | 空串
emptyString :: LispVal
emptyString = String []
