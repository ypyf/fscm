-- fscm/core
module Scheme.Primitives
    (loadProc, readProc, evalProc,
     keywords, primitives, primitivesIo)
    where

import Scheme.Types
import Scheme.Parser
import Scheme.Eval
import Scheme.Variables

import Debug.Trace
import System.Exit
import System.Mem
import System.IO
import qualified System.Console.Readline as RL
import Data.Time
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Foldable (foldlM)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State.Lazy
import Control.Monad.Trans.Cont
import Control.Concurrent (threadDelay)
import qualified System.Plugins.Load as PL


--
-- 语法关键字
--

-- 一个简单的计时函数
time :: [Lisp] -> InterpM Lisp
time (x:xs) = do
  start <- liftIO $ getCurrentTime
  v <- eval x
  stop <- liftIO $ getCurrentTime
  liftIO $ print $ diffUTCTime stop start
  return v
time _ = throwError $ Default "define: bad syntax"

bench :: [Lisp] -> InterpM Lisp
bench _ = return Void

quote :: [Lisp] -> InterpM Lisp
quote [datum] = return datum
quote args = throwError $ NumArgs 1 args

-- 参见 r5rs 5.2.1
defineVar :: [Lisp] -> InterpM Lisp
defineVar [Symbol name, expr] = do
  r <- ask
  case getValue name r of
    Nothing -> do
      -- define类似于letrec，允许闭包的递归定义，所以先设定一个初始值
      -- 在修改后的环境中执行后续计算
      -- 第一次定义变量
      tmp <- liftIO $ newIORef Void
      callCC $ \k -> local (insert tmp) (def tmp >> k Void)
    -- 重定义变量
    Just x -> def x >> return Void
  where
    insert :: IORef Lisp -> Context -> Context
    insert v (SC r) = SC $ M.insert name v r
    insert v (TC locals upvalues r) = TC (insert v locals) upvalues r
    -- 有的实现似乎可以定义为void
    def :: IORef Lisp -> InterpM ()
    def ref = eval expr >>= liftIO . writeIORef ref

-- (define (name...) ...)
defineVar (List (Symbol name:xs):body) =
    eval $ List [Symbol "define", Symbol name, List (Symbol "lambda":List xs:body)]

defineVar (DotList (Symbol name:xs) varg:body) =
    eval $ List [Symbol "define", Symbol name, List (Symbol "lambda":DotList xs varg:body)]

defineVar _ = throwError $ Default "define: bad syntax"


setVar :: [Lisp] -> InterpM Lisp
setVar [Symbol name, expr] = do
  -- TODO 不可赋予Void(undefined)值
  val <- eval expr  -- 注意首先对expr求值
  r <- ask
  case getValue name r of
    Nothing -> throwError $ UnboundName name
    Just x -> do
      liftIO $ writeIORef x val
      return Void

ifExp :: [Lisp] -> InterpM Lisp
ifExp [pred, conseq] = do
  r <- eval pred
  case r of
    -- 只有#f是假值
    LispFalse -> return Void
    _         -> eval_tail conseq

ifExp [pred, conseq, alt] = do
  r <- eval pred
  case r of
    LispFalse -> eval_tail alt
    _         -> eval_tail conseq


letExp :: [Lisp] -> InterpM Lisp
letExp (List bindings:body) = do
  x <- unpack bindings
  let pairs = unzip x
      keys = fst pairs -- params
      values = snd pairs   -- args
  eval $ List (List (Symbol "lambda":List keys:body):values)
  where
    unpack :: [Lisp] -> InterpM [(Lisp, Lisp)]
    unpack [] = return []
    unpack (List [x, v]:xs) = do
      xs' <- unpack xs
      return $ (x, v) : xs'
    unpack (x:_) = throwError $ BadSpecialForm "let" x

letStarExp :: [Lisp] -> InterpM Lisp
letStarExp (List bindings:body) = do
    x <- unpack bindings
    let pairs = unzip x
        keys = fst pairs
        values = snd pairs
    eval $ List $ foo x
    where
        unpack :: [Lisp] -> InterpM [(Lisp, Lisp)]
        unpack [] = return []
        unpack (List [x, v]:xs) = do
            xs' <- unpack xs
            return $ (x, v) : xs'
        unpack (x:_) = throwError $ BadSpecialForm "let*" x
        foo :: [(Lisp, Lisp)] -> [Lisp]
        foo [] = List (Symbol "lambda":List []:body):[]
        foo ((k,v):xs) = List (Symbol "lambda":List [k]:List (foo xs):[]):[v]

-- (begin e1 e2 ...) => ((lambda () e1 e2 ...))
-- FIXME 顶层begin中的define应该绑定在顶层环境
beginExp :: [Lisp] -> InterpM Lisp
beginExp [] = return Void
beginExp lst =  eval_tail $ List [List $ Symbol "lambda":List []:lst]  -- 这里是尾调用而不是Lambda定义

keywords :: [(String, [Lisp] -> InterpM Lisp)]
keywords =
    [
     ("quote", quote),
     ("let", letExp),
     ("let*", letStarExp),
     ("begin", beginExp),
     ("if", ifExp),
     ("define", defineVar),
     ("set!", setVar),
     ("time", time),
     ("bench", bench)
    ]


--
-- 内置过程 Procedures
--

quitProc :: [Lisp] -> InterpM Lisp
quitProc _ = liftIO $ exitWith ExitSuccess

-- 载入Hs目标模块(*.o)
loadPlug :: [Lisp] -> InterpM Lisp
loadPlug [String file, String name] = do
    mv <- liftIO $ PL.load file ["."] [] name
    case mv of
        PL.LoadFailure errs -> throwError $ Default $ show errs
        PL.LoadSuccess _ v  -> return $ HFunc (v::[Lisp] -> InterpM Lisp)
loadPlug args = throwError $ NumArgs 1 args

-- 载入lisp源文件
loadFile :: String -> InterpM [Lisp]
loadFile file = (liftIO $ readFile file) >>= readLisp

-- 载入源文件并转换成Lisp并求值
loadProc :: [Lisp] -> InterpM Lisp
loadProc [String file] = loadFile file >>= mapM_ eval >> return Void
loadProc args = throwError $ NumArgs 1 args

-- 调用load并把结果转换成单一的LispVal
readAll :: [Lisp] -> InterpM Lisp
readAll [String file] = loadFile file >>= return . List

-- FIXME
applyProc :: [Lisp] -> InterpM Lisp
applyProc [] = throwError $ NumArgs 2 []
applyProc val@[_] = throwError $ NumArgs 2 val
applyProc (fn:xs) =
    case last xs of
        List args -> eval $ List $ fn : (init xs ++ args)
        notList   -> throwError $ TypeMismatch "List" notList


-- 参见 r5rs 6.5
-- TODO 环境参数
evalProc :: [Lisp] -> InterpM Lisp
evalProc [datum] = do
  ret <- eval datum
  case ret of  -- 处理顶层尾调用
    List (TailCall func:v) -> func v
    _                      -> return ret
evalProc args = throwError $ NumArgs 1 args

-- call-with-current-continuation
callcc :: [Lisp] -> InterpM Lisp
callcc [Lambda fn] = callCC (\k -> fn [Continuation k])
callcc [Continuation cont] = callCC (\k -> cont (Continuation k))
callcc [IOFunc fn] = callCC (\k -> fn [Continuation k])
callcc [Func fn] = callCC $ \k ->
                   case fn [Continuation k] of
                     Left e -> throwError e  -- 再次抛出以提升纯函数中的ThrowsError
                     Right res -> return res
callcc [arg] = throwError $ TypeMismatch "* -> *" arg
callcc args = throwError $ NumArgs 2 args


-- call-with-failure-continuation
-- http://sisc-scheme.org/sisc.pdf
-- thunk是一个无参的lambda
-- failure-handler有三个参数：第一个是错误消息，第二个是错误发生时的延续以及失败延续
callfc :: [Lisp] -> InterpM Lisp
callfc [Lambda thunk, Lambda handler] =
    (thunk []) `catchError` callHandler
    where
      callHandler (RTE message errorCont) = handler [String message, Continuation errorCont, Failure parentFK]
      callHandler e = throwError e

-- 内置错误延续
parentFK :: String -> (Lisp -> InterpM Lisp) -> InterpM Lisp
parentFK message errorCont = throwError $ RTE message errorCont


-- IO procedure

-- (flush-output)
flushOutputProc :: [Lisp] -> InterpM Lisp
flushOutputProc [] = liftIO $ hFlush stdout >> return Void
flushOutputProc _ = return Void -- TODO 加入端口参数

currentInputPort :: [Lisp] -> InterpM Lisp
currentInputPort [] = return $ HPort stdin
currentInputPort args = throwError $ NumArgs 0 args

currentOutputPort :: [Lisp] -> InterpM Lisp
currentOutputPort [] = return $ HPort stdout
currentOutputPort args = throwError $ NumArgs 0 args

-- r5rs 6.6.3
display :: [Lisp] -> InterpM Lisp

-- r5rs:6.6.3 字符串字面量不显示引号且不显示转义符号
display [String s] = liftIO $ putStr s >> return Void
display [String s, HPort port] = liftIO $ hPutStr port s >> return Void

-- r5rs:6.6.3 字符用write-char显示
display [Char c] = liftIO $ putChar c >> return Void
display [Char c, HPort port] = liftIO $ hPutChar port c >> return Void

display [x] = liftIO $ putStr (show x) >> return Void
display [x, HPort port] = liftIO $ hPutStr port (show x) >> return Void

display args = callCC $ \k -> throwError $ RTE "1 arg expected." k

-- 输出人类可读字符(不是外部表示)
writeChar :: [Lisp] -> InterpM Lisp
writeChar [] = throwError $ NumArgs 1 []
writeChar [Char c] = liftIO $ putChar c >> return Void
writeChar [Char c, HPort port] = liftIO $ hPutChar port c >> return Void
writeChar [notChar] = throwError $ TypeMismatch "char" notChar

-- makePort是对haskell中的openFile函数的包装
makePort :: IOMode -> [Lisp] -> InterpM Lisp
--makePort ReadMode [] = liftM HPort $ liftIO $ openFile "CONIN$" ReadMode
makePort mode [String file] = liftM HPort $ liftIO $ openFile file mode

closePort :: [Lisp] -> InterpM Lisp
closePort [HPort port] = liftIO $ hClose port >> (return LispTrue)
closePort _ = return LispFalse

-- read函数
-- read函数将Datum解析为内部对象(Lisp)
readProc :: [Lisp] -> InterpM Lisp
readProc [] = do --readProc [HPort stdin] -- 缺省端口
    line <- liftIO $ RL.readline "> "
    case line of
        Nothing -> return Void -- EOF
        Just s  ->
            if s /= [] then do
                liftIO $ RL.addHistory s
                r <- readLisp s
                case r of
                    [] -> return Void
                    x:xs -> return x
            else return Void
readProc [HPort port] = do
  s <- liftIO $ hGetLine port
  r <- readLisp s
  case r of
    [] -> return Void
    x:xs -> return x -- TODO rest部分应该缓存，下一次继续读

-- 将LispVal转换为字符串后写入端口
writeProc :: [Lisp] -> InterpM Lisp
writeProc [obj] = writeProc [obj, HPort stdout]  -- 缺省端口
writeProc [obj, HPort port] = liftIO $ hPrint port obj >> (return LispTrue)

-- 读取整个文件内容作为Lisp字符串
readContents :: [Lisp] -> InterpM Lisp
readContents [String file] = liftM String $ liftIO $ readFile file

-- FIXME
-- (+) => 0
-- (*) => 1
-- (- n) => -n
numericBinop :: (Integer -> Integer -> Integer) -> [Lisp] -> ThrowsError Lisp
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Fixnum . foldl1 op
  --control $ \run -> catch (run . (\e -> mapM unpackNum params >>= return . Fixnum . foldl1 op)) (run . (\e -> throwError $ Default "/: division by zero"))
  --mapM unpackNum params >>= return . Fixnum . foldl1 op

-- 与R5RS不同，我们的解释器暂时是一个弱类型的
unpackNum :: Lisp -> ThrowsError Integer
unpackNum (Fixnum n) = return n
unpackNum (String n) = let parsed = reads n in
                       if null parsed then throwError $ TypeMismatch "number" $ String n
                       else return $ fst $ parsed !! 0
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- doubleBinop :: (Double -> Double -> Double) -> [Lisp] -> ThrowsError Lisp
-- doubleBinop op [] = throwError $ NumArgs 2 []
-- doubleBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
-- doubleBinop op params = mapM unpackDouble params >>= return . Double . foldl1 op

-- unpackDouble :: Lisp -> ThrowsError Double
-- unpackDouble (Double n) = return n
-- unpackDouble (List [n]) = unpackDouble n
-- unpackDouble notDouble = throwError $ TypeMismatch "double" notDouble

-- 二元真值函数助手
boolBinop :: (Lisp -> ThrowsError a) -> (a -> a -> Bool) -> [Lisp] -> ThrowsError Lisp
boolBinop _ _ [] = throwError $ NumArgs 2 []
boolBinop _ _ args@[x] = throwError $ NumArgs 2 args
boolBinop unpacker op args = do
  r <- liftM loop $ mapM unpacker args
  return $ if r then LispTrue else LispFalse
  where
    loop []       = True
    loop [_]      = True
    loop (x:y:zs) = (x `op` y) && loop (y:zs)

numBoolBinop :: (Integer -> Integer -> Bool) -> [Lisp] -> ThrowsError Lisp
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [Lisp] -> ThrowsError Lisp
strBoolBinop = boolBinop unpackStr

--boolBoolBinop :: (Bool -> Bool -> Bool) -> [Lisp] -> ThrowsError Lisp
--boolBoolBinop op = foldr op True boolBinop unpackBool

--andProc :: [Lisp] -> ThrowsError Lisp
--andProc x = return $ if foldrM ((&&) . unpackBool) True x then LispTrue else LispFalse

-- orProce :: [Lisp] -> ThrowsError Lisp
-- orProce = foldr or True

unpackStr :: Lisp -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Fixnum s) = return $ show s
unpackStr LispTrue = return $ show LispTrue
unpackStr LispFalse = return $ show LispFalse
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: Lisp -> ThrowsError Bool
unpackBool LispTrue = return True
unpackBool LispFalse = return False
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

isPair :: [Lisp] -> ThrowsError Lisp
isPair [DotList _ _] = return LispTrue
isPair [_] = return LispFalse
isPair args = throwError $ NumArgs 1 args

isBoolean :: [Lisp] -> ThrowsError Lisp
isBoolean [LispTrue] = return LispTrue
isBoolean [LispFalse] = return LispTrue
isBoolean [_] = return LispFalse
isBoolean args = throwError $ NumArgs 1 args

isPort :: [Lisp] -> ThrowsError Lisp
isPort [HPort _] = return LispTrue
isPort [_] = return LispFalse
isPort args = throwError $ NumArgs 1 args

-- TODO
isSymbol :: [Lisp] -> ThrowsError Lisp
isSymbol [Symbol _] = return LispTrue
isSymbol [_] = return LispFalse
isSymbol args = throwError $ NumArgs 1 args

isString :: [Lisp] -> ThrowsError Lisp
isString [String _] =return LispTrue
isString [_] = return LispFalse
isString args = throwError $ NumArgs 1 args

isFixnum :: [Lisp] -> ThrowsError Lisp
isFixnum [Fixnum _] = return LispTrue
isFixnum [_] = return LispFalse
isFixnum args = throwError $ NumArgs 1 args

isList :: [Lisp] -> ThrowsError Lisp
isList [List _] = return LispTrue
isList [DotList _ rest] = isList [rest]
isList [_] = return LispFalse
isList args = throwError $ NumArgs 1 args

isChar :: [Lisp] -> ThrowsError Lisp
isChar [Char _] = return LispTrue
isChar [_] = return LispFalse
isChar args = throwError $ NumArgs 1 args

isProcedure :: [Lisp] -> ThrowsError Lisp
isProcedure [Func _] = return LispTrue
isProcedure [IOFunc _] = return LispTrue
isProcedure [Lambda _] = return LispTrue
isProcedure [_] = return LispFalse
isProcedure args = throwError $ NumArgs 1 args

car :: [Lisp] -> ThrowsError Lisp
car [List (x:xs)] = return x
car [DotList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [Lisp] -> ThrowsError Lisp
cdr [List (x:xs)] = return $ List xs
cdr [DotList [x] y] = return y
cdr [DotList (_:xs) y] = return $ DotList xs y
cdr [arg] = throwError $ TypeMismatch "pair" arg
cdr args = throwError $ NumArgs 1 args

-- x + List = List
-- x + DotList = DotList
cons :: [Lisp] -> ThrowsError Lisp
cons [a, List b] = return $ List ([a] ++ b)
cons [a, DotList b c] = return $ DotList ([a] ++ b) c
cons [a, b] = return $ DotList [a] b
cons args = throwError $ NumArgs 2 args

eqv :: [Lisp] -> ThrowsError Lisp
eqv [LispTrue, LispTrue] = return LispTrue
eqv [LispFalse, LispFalse] = return LispTrue
eqv [LispTrue, LispFalse] = return LispFalse
eqv [LispFalse, LispTrue] = return LispFalse
eqv [Char c1, Char c2] = return $ if c1 == c2 then LispTrue else LispFalse
eqv [Fixnum arg1, Fixnum arg2] = return $ if arg1 == arg2 then LispTrue else LispFalse
eqv [String arg1, String arg2] = return $ if arg1 == arg2 then LispTrue else LispFalse
eqv [Symbol arg1, Symbol arg2] = return $ if arg1 == arg2 then LispTrue else LispFalse
eqv [DotList xs x, DotList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return $ if (length arg1 == length arg2) && (and $ map eqvPair $ zip arg1 arg2) then LispTrue else LispFalse
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right LispTrue -> True
                               Right _ -> False

eqv [_, _] = return LispFalse
eqv badArgList = throwError $ NumArgs 2 badArgList


equal :: [Lisp] -> ThrowsError Lisp
equal [LispTrue, LispTrue] = return LispTrue
equal [LispFalse, LispFalse] = return LispTrue
equal [LispTrue, LispFalse] = return LispFalse
equal [LispFalse, LispTrue] = return LispFalse
equal [Char c1, Char c2] = return $ if c1 == c2 then LispTrue else LispFalse
equal [Fixnum arg1, Fixnum arg2] = return $ if arg1 == arg2 then LispTrue else LispFalse
equal [String arg1, String arg2] = return $ if arg1 == arg2 then LispTrue else LispFalse
equal [Symbol arg1, Symbol arg2] = return $ if arg1 == arg2 then LispTrue else LispFalse
equal [DotList xs x, DotList ys y] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [List arg1, List arg2] = return $ if (length arg1 == length arg2) && (and $ map equalPair $ zip arg1 arg2) then LispTrue else LispFalse
    where equalPair (x1, x2) = case equal [x1, x2] of
                               Left err -> False
                               Right LispTrue -> True
                               Right _ -> False

equal [_, _] = return LispFalse
equal badArgList = throwError $ NumArgs 2 badArgList
{-
eqv' :: Lisp -> Lisp -> Bool
eqv' (Bool arg1) (Bool arg2) = arg1 == arg2
eqv' (Fixnum arg1) (Fixnum arg2) = arg1 == arg2
eqv' (String arg1) (String arg2) = arg1 == arg2
eqv' (Symbol arg1) (Symbol arg2) = arg1 == arg2
eqv' (DotList xs x) (DotList ys y) = eqv' (List $ xs ++ [x]) (List $ ys ++ [y])
eqv' (List arg1) (List arg2) = (length arg1 == length arg2) && (and $ map eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = eqv' x1 x2
eqv' _ _ = LispFalse
-}

makeString :: [Lisp] -> ThrowsError Lisp
makeString [Fixnum k] = makeString' (fromInteger k) '\0'
makeString [Fixnum k, Char c] = makeString' (fromInteger k) c
makeString _ = throwError $ Default "Argument Types Error"

makeString' :: Int -> Char -> ThrowsError Lisp
makeString' k c = let x = c : x in
                  if k >= 0 then return $ String $ take k x
                  else throwError $ Default "expects argument of type <non-negative exact integer>"

stringFromCharList :: [Lisp] -> ThrowsError Lisp
stringFromCharList [Char arg] = return $ String [arg]
stringFromCharList (Char arg : xs) = do
  String rest <- stringFromCharList xs
  return $ String $ [arg] ++ rest

stringLength :: [Lisp] -> ThrowsError Lisp
stringLength [String arg] = return $ Fixnum $ toInteger $ length arg
stringLength [badArg] = throwError $ TypeMismatch "String" badArg
stringLength badArgs = throwError $ NumArgs 1 badArgs

stringAppend :: [Lisp] -> ThrowsError Lisp
stringAppend args = do
    r <- foldlM append "" args
    return $ String r
  where
    append :: String -> Lisp -> ThrowsError String
    append a (String b) = return $ a ++ b
    append a b = throwError $ TypeMismatch "string" b


stringRef :: [Lisp] -> ThrowsError Lisp
stringRef [(String arg0), (Fixnum arg1)] =
    if arg1 < 0 || arg1 >= toInteger (length arg0)
    then throwError $ Default "String index out of range"
    else return $ Char $ arg0 !! fromInteger(arg1)


--
-- 进程控制
--

sleepProc :: [Lisp] -> InterpM Lisp
sleepProc [Fixnum n] = liftIO $ threadDelay (fromInteger n * 1000000) >> return Void
sleepProc args = throwError $ NumArgs 1 args


--- 错误处理
errorProc :: [Lisp] -> InterpM Lisp
errorProc [String message] = callCC $ \k -> throwError $ RTE message k
errorProc args = throwError $ NumArgs 1 args

------ 内存管理
collectGarbage :: [Lisp] -> InterpM Lisp
collectGarbage [] = liftIO performGC >> return Void
collectGarbage args = throwError $ NumArgs 0 args

idiv :: Integer -> Integer -> InterpM Lisp
idiv a 0 = throwError ZeroDivision
idiv a b = return Void

-- 内置的纯函数查询表
primitives :: [(String, [Lisp] -> ThrowsError Lisp)]
primitives =
    [
      -- 数值算符
      ("+", numericBinop (+)),
      ("-", numericBinop (-)),
      ("*", numericBinop (*)),
      ("/", numericBinop div),
      ("mod", numericBinop mod),
      ("quot", numericBinop quot),
      ("rem", numericBinop rem),

      ("=", numBoolBinop (==)),
      ("<", numBoolBinop (<)),
      (">", numBoolBinop (>)),
      ("/=", numBoolBinop (/=)),
      (">=", numBoolBinop (>=)),
      ("<=", numBoolBinop (<=)),

      ("string=?", strBoolBinop (==)),
      ("string<?", strBoolBinop (<)),
      ("string>?", strBoolBinop (>)),
      ("string<=?", strBoolBinop (<=)),
      ("string>=?", strBoolBinop (>=)),

      -- 类型信息
      ("symbol?", isSymbol),
      ("boolean?", isBoolean),
      ("pair?", isPair),
      ("number?", isFixnum),  -- FIXME
      ("string?", isString),
      ("list?", isList),
      ("char?", isChar),
      ("port?", isPort),
      ("procedure?", isProcedure),

      -- 字符串操作
      ("string", stringFromCharList),
      ("make-string", makeString),
      ("string-length", stringLength),
      ("string-append", stringAppend),
      ("string-ref", stringRef),

      -- 列表操作
      ("car", car), ("cdr", cdr), ("cons", cons),

      ("eq?", eqv), ("eqv?", eqv), ("equal?", equal)
    ]

-- 内置IO函数查询表
primitivesIo :: [(String, [Lisp] -> InterpM Lisp)]
primitivesIo =
    [
     ("load", loadProc),
     ("load-hs-proc", loadPlug),
     ("eval", evalProc),
     ("apply", applyProc),
     ("call-with-current-continuation", callcc),
     ("call-with-failure-continuation", callfc),
     ("display", display),
     ("write-char", writeChar),
     ("open-input-file", makePort ReadMode),
     ("open-output-file", makePort WriteMode),
     ("close-input-port", closePort),
     ("close-output-port", closePort),
     ("flush-output", flushOutputProc),
     ("current-input-port", currentInputPort),
     ("current-output-port", currentOutputPort),
     ("read", readProc),
     ("write", writeProc),
     ("read-contents", readContents),
     ("read-all", readAll),

     -- 控制
     ("sleep", sleepProc),
     ("collect-garbage", collectGarbage),
     ("quit", quitProc),
     ("error", errorProc)

     -- 互操作
     --("load-ffi", loadHaskellFunction)
    ]
