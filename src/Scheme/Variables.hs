module Scheme.Variables where

import Scheme.Types

import Data.IORef
import qualified Data.Map.Strict as M

-- 在环境中查找绑定的符号
getValue :: String -> Context -> Maybe (IORef Lisp)
getValue name (SC r) = M.lookup name r
getValue name (TC locals upvalues r) =
  case getValue name locals of
    Nothing -> case getValue name upvalues of
                 Nothing -> getValue name r
                 x       -> x
    x      -> x
