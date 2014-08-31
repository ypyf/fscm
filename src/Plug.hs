{-# LANGUAGE BangPatterns #-}

module Plug where
import Scheme.Types

add :: [Lisp] -> InterpM Lisp
add [Number a, Number b] = return $ Number $ a + b

fib :: [Lisp] -> InterpM Lisp
fib [Number n] = return $ Number $ go n (0, 1)
  where
    go !n (!a, !b) | n == 0    = a
                   | otherwise = go (n-1) (b, a+b)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

foo :: Integer
foo = 12345
