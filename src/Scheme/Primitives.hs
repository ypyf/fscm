-- fscm/core
module Scheme.Primitives
    (loadProc, readProc, evalProc, readString,
     keywords, primitives, primitivesIo)
    where

import Scheme.Types
import Scheme.Parser
import Scheme.Eval

import System.Exit
import System.Mem
import System.IO
import Data.Time
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Foldable (foldlM)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad.Trans.Cont
import Control.Concurrent (threadDelay)


--
-- 语法关键字
--

-- 一个简单的计时函数
time :: [LispVal] -> InterpM LispVal
time (x:xs) = do
  start <- liftIO getCurrentTime
  v <- eval x
  stop <- liftIO getCurrentTime
  liftIO $ print $ diffUTCTime stop start
  return v
time _ = throwError $ Default "time: bad syntax"

bench :: [LispVal] -> InterpM LispVal
bench _ = return Undefined

quote :: [LispVal] -> InterpM LispVal
quote [datum] = return datum
quote args = throwError $ NumArgs 1 args

quasiquote :: [LispVal] -> InterpM LispVal
quasiquote [datum] = evalqq 0 datum
quasiquote args = throwError $ NumArgs 1 args

unquote :: [LispVal] -> InterpM LispVal
unquote args = throwError $ Default "unquote: not in quasiquote"

unquoteSplicing :: [LispVal] -> InterpM LispVal
unquoteSplicing args = throwError $ Default "unquote-splicing: not in quasiquote"

-- 顶层定义 r5rs 5.2.1
-- FIXME 只能出现在顶层或<body>的开始
-- TODO 出现在<body>中的属于内部定义,可以变换为等价的letrec绑定
-- define类似于letrec，允许闭包的递归定义，所以先设定一个临时的初值
-- 在修改后的环境中执行后续计算
defineVar :: [LispVal] -> InterpM LispVal
defineVar [Symbol name, expr] = do
  env <- ask   -- FIXME get TopEnv
  case M.lookup name env of
      Nothing -> do
        tmp <- liftIO $ newIORef Undefined
        callCC $ \k -> local (M.insert name tmp) (def tmp >>= k)
      -- 重定义变量
      Just var -> def var
  where
      def :: IORef LispVal -> InterpM LispVal
      def var = eval expr >>= liftIO . writeIORef var >> return Undefined

-- (define (name...) ...)
defineVar (List (Symbol name:xs):body) =
    eval $ List [Symbol "define", Symbol name, List (Symbol "lambda":List xs:body)]

defineVar (DotList (Symbol name:xs) varg:body) =
    eval $ List [Symbol "define", Symbol name, List (Symbol "lambda":DotList xs varg:body)]

defineVar _ = throwError $ Default "define: bad syntax"


-- (set! name expr)
setVar :: [LispVal] -> InterpM LispVal
setVar [Symbol name, expr] = do
  -- TODO 不可赋予Void(undefined)值
  val <- eval expr  -- 注意首先对expr求值
  env <- ask
  case M.lookup name env of
    Nothing -> throwError $ UnboundName name
    Just x -> do
      liftIO $ writeIORef x val
      return Undefined

-- 只有#f是假值
ifExp :: [LispVal] -> InterpM LispVal
ifExp [pred, conseq] = do
  r <- eval pred
  case r of
    LispFalse -> return Undefined
    _         -> evalTail conseq

ifExp [pred, conseq, alt] = do
  r <- eval pred
  case r of
    LispFalse -> evalTail alt
    _         -> evalTail conseq

-- 局部绑定
-- 变换为等价的函数应用
letExp :: [LispVal] -> InterpM LispVal
letExp (List bindings:bodys) = do
  x <- unpack bindings
  let pairs = unzip x
      keys = fst pairs -- params
      values = snd pairs   -- args
  eval $ List (List (Symbol "lambda":List keys:bodys):values)
  where
    unpack :: [LispVal] -> InterpM [(LispVal, LispVal)]
    unpack [] = return []
    unpack (List [x, v]:xs) = do
      xs' <- unpack xs
      return $ (x, v) : xs'
    unpack (x:_) = throwError $ BadSpecialForm "let" x

-- let* 可以变换为等价的let形式
letStar :: [LispVal] -> InterpM LispVal
letStar (List []:bodys) = eval $ List $ Symbol "let":List []:bodys
letStar (List (binding:rest):bodys) = eval $ List $ Symbol "let":List [binding]:List (Symbol "let*":List rest:bodys):[]
letStar [] = throwError $ BadSpecialForm "let*" $ String "(let*)"
letStar [args] = throwError $ BadSpecialForm "let*" $ args

-- (begin e1 e2 ...) => ((lambda () e1 e2 ...))
-- FIXME 顶层begin中的define应该绑定在顶层环境
beginExp :: [LispVal] -> InterpM LispVal
beginExp [] = return Undefined
beginExp lst = evalTail $ List [List $ Symbol "lambda":List []:lst]  -- 这里是尾调用而不是Lambda定义


-- (define-syntax ...)
-- 参见 r5rs 5.3
-- FIXME只能出现在程序的顶层
-- 转换器的输入是一组规则和S表达式
-- 输出是转换后的S表达式
defineSyntax :: [LispVal] -> InterpM LispVal
defineSyntax [Symbol name, syntax] = return Undefined
--  let List (Symbol "syntax-rules":List ids:rules) = syntax
--  return $ Transformer $ t rules
--  where
--    -- 返回空表示无法匹配
--    t :: [LispVal] -> [LispVal] -> [LispVal]
--    t [] _ = []
--    t (List [pattern, template]:rs) exprs =
--      let Symbol key = head pattern -- 模式中出现的关键字
--          Symbol key' = head exprs  -- 表达式中出现的关键字
--      in
--        if (key == name || key `elem` ids) then
--            if length pattern == length exprs && key == key' then
--
--            else t rs exprs
--        else []



defineModule :: [LispVal] -> InterpM LispVal
defineModule [List [file]] = return Undefined

--defineModule (List [dir file]) =
keywords :: [(String, [LispVal] -> InterpM LispVal)]
keywords =
    [
     ("define-syntax", defineSyntax),
     ("quote", quote),
     ("quasiquote", quasiquote),
     ("unquote", unquote),
     ("unquote-splicing", unquoteSplicing),
     ("let", letExp),
     ("let*", letStar),
     ("begin", beginExp),
     ("if", ifExp),
     ("define", defineVar),
     ("set!", setVar),
     ("time", time),
     ("bench", bench),
     ("define-module", defineModule)
    ]


--
-- 内置过程 Procedures
--


quitProc :: [LispVal] -> InterpM LispVal
quitProc _ = liftIO $ putStrLn "Bye!" >> liftIO exitSuccess


-- 载入lisp源文件
loadFile :: String -> InterpM [LispVal]
loadFile file = liftIO (readFile file) >>= readLisp

--readLines n f = withFile f ReadMode $ replicateM n . hGetLine

-- 载入源文件并转换成Lisp并求值
loadProc :: [LispVal] -> InterpM LispVal
loadProc [String file] = loadFile file >>= mapM_ eval >> return Undefined
loadProc args = throwError $ NumArgs 1 args

-- 调用load并把结果转换成单一的LispVal
readAll :: [LispVal] -> InterpM LispVal
readAll [String file] = List <$> loadFile file

-- FIXME
applyProc :: [LispVal] -> InterpM LispVal
applyProc [] = throwError $ NumArgs 2 []
applyProc val@[_] = throwError $ NumArgs 2 val
applyProc (fn:xs) =
    case last xs of
        List args -> eval $ List $ fn : (init xs ++ args)
        notList   -> throwError $ TypeMismatch "List" notList


-- 参见 r5rs 6.5
-- TODO 环境参数
evalProc :: [LispVal] -> InterpM LispVal
evalProc [datum] = do
  ret <- eval datum
  case ret of  -- 处理顶层尾调用
    List (func@(TailCall {}):args) -> apply func args
    _                              -> return ret
evalProc args = throwError $ NumArgs 1 args

-- call-with-current-continuation
callcc :: [LispVal] -> InterpM LispVal
callcc [fn] = callCC $ \k -> apply fn [Continuation k]
callcc args = throwError $ NumArgs 2 args


-- call-with-failure-continuation
-- http://sisc-scheme.org/sisc.pdf
-- thunk是一个无参的lambda
-- failure-handler有三个参数：第一个是错误消息，第二个是错误发生时的延续以及失败延续
-- callfc :: [LispVal] -> InterpM LispVal
-- callfc [Lambda thunk, Lambda handler] =
--     thunk [] `catchError` callHandler
--     where
--       callHandler (RTE message errorCont) = handler [String message, Continuation errorCont, Failure parentFK]
--       callHandler e = throwError e

-- -- 内置错误延续
-- parentFK :: String -> (LispVal -> InterpM LispVal) -> InterpM LispVal
-- parentFK message errorCont = throwError $ RTE message errorCont


-- IO procedure

-- (flush-output)
flushOutputProc :: [LispVal] -> InterpM LispVal
flushOutputProc [] = liftIO $ hFlush stdout >> return Undefined
flushOutputProc _ = return Undefined -- TODO 加入端口参数

currentInputPort :: [LispVal] -> InterpM LispVal
currentInputPort [] = return $ HPort stdin
currentInputPort args = throwError $ NumArgs 0 args

currentOutputPort :: [LispVal] -> InterpM LispVal
currentOutputPort [] = return $ HPort stdout
currentOutputPort args = throwError $ NumArgs 0 args

-- r5rs 6.6.3
display :: [LispVal] -> InterpM LispVal
-- TODO 端口没有指定时应该获取当前端口
display [val] = display [val, HPort stdout]
display [val, HPort port] = display' val port >> return Undefined
display args = throwError $ NumArgs 1 args

display' :: LispVal -> Handle -> InterpM ()
display' (String s) port = liftIO $ hPutStr port s
display' (Char c) port = liftIO $ hPutChar port c
display' val port = liftIO $ hPutStr port $ show val

writeChar :: [LispVal] -> InterpM LispVal
writeChar [] = throwError $ NumArgs 1 []
writeChar val@[Char _] = display val
writeChar val@[Char _, HPort _] = display val
writeChar [notChar] = throwError $ TypeMismatch "char" notChar


-- makePort是对haskell中的openFile函数的包装
makePort :: IOMode -> [LispVal] -> InterpM LispVal
--makePort ReadMode [] = liftM HPort $ liftIO $ openFile "CONIN$" ReadMode
makePort mode [String file] = fmap HPort $ liftIO $ openFile file mode

closePort :: [LispVal] -> InterpM LispVal
closePort [HPort port] = liftIO $ hClose port >> return LispTrue
closePort _ = return LispFalse

-- from Clojure :-)
readString :: [LispVal] -> InterpM LispVal
readString [] = throwError $ NumArgs 1 []
readString [String str] = do
  r <- readLisp str
  case r of
    [] -> return Undefined
    x:xs -> return x -- TODO rest部分应该缓存，下一次继续读

-- read函数
-- read函数将Datum解析为内部对象(Lisp)
readProc :: [LispVal] -> InterpM LispVal
readProc [] = readProc [HPort stdin] -- 缺省端口
readProc [HPort port] = liftIO (hGetLine port) >>= \s -> readString [String s]

-- 将Lisp转换为字符串形式的外部表示后写入端口
writeProc :: [LispVal] -> InterpM LispVal
writeProc [obj] = writeProc [obj, HPort stdout]  -- 缺省端口
writeProc [obj, HPort port] = liftIO $ hPrint port obj >> return LispTrue

-- 读取整个文件内容作为Lisp字符串
readContents :: [LispVal] -> InterpM LispVal
readContents [String file] = fmap String $ liftIO $ readFile file

-- FIXME
-- (+) => 0
-- (*) => 1
-- (+ 1) => 1
-- (- 1) => -1
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Fixnum . foldl1 op
  --control $ \run -> catch (run . (\e -> mapM unpackNum params >>= return . Fixnum . foldl1 op)) (run . (\e -> throwError $ Default "/: division by zero"))
  --mapM unpackNum params >>= return . Fixnum . foldl1 op

-- 与R5RS不同，我们的解释器暂时是一个弱类型的
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Fixnum n) = return n
unpackNum (String n) = let parsed = reads n in
                       if null parsed then throwError $ TypeMismatch "number" $ String n
                       else return $ fst $ head parsed
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- doubleBinop :: (Double -> Double -> Double) -> [LispVal] -> ThrowsError LispVal
-- doubleBinop op [] = throwError $ NumArgs 2 []
-- doubleBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
-- doubleBinop op params = mapM unpackDouble params >>= return . Double . foldl1 op

-- unpackDouble :: LispVal -> ThrowsError Double
-- unpackDouble (Double n) = return n
-- unpackDouble (List [n]) = unpackDouble n
-- unpackDouble notDouble = throwError $ TypeMismatch "double" notDouble

-- 二元真值函数助手
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop _ _ [] = throwError $ NumArgs 2 []
boolBinop _ _ args@[x] = throwError $ NumArgs 2 args
boolBinop unpacker op args = do
  r <- loop <$> mapM unpacker args
  return $ if r then LispTrue else LispFalse
  where
    loop []       = True
    loop [_]      = True
    loop (x:y:zs) = (x `op` y) && loop (y:zs)

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

--boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
--boolBoolBinop op = foldr op True boolBinop unpackBool

--andProc :: [LispVal] -> ThrowsError LispVal
--andProc x = return $ if foldrM ((&&) . unpackBool) True x then LispTrue else LispFalse

-- orProce :: [LispVal] -> ThrowsError LispVal
-- orProce = foldr or True

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Fixnum s) = return $ show s
unpackStr LispTrue = return $ show LispTrue
unpackStr LispFalse = return $ show LispFalse
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool LispTrue = return True
unpackBool LispFalse = return False
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

isPair :: [LispVal] -> ThrowsError LispVal
isPair [DotList _ _] = return LispTrue
isPair [_] = return LispFalse
isPair args = throwError $ NumArgs 1 args

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean [LispTrue] = return LispTrue
isBoolean [LispFalse] = return LispTrue
isBoolean [_] = return LispFalse
isBoolean args = throwError $ NumArgs 1 args

isPort :: [LispVal] -> ThrowsError LispVal
isPort [HPort _] = return LispTrue
isPort [_] = return LispFalse
isPort args = throwError $ NumArgs 1 args

-- TODO
isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol [Symbol _] = return LispTrue
isSymbol [_] = return LispFalse
isSymbol args = throwError $ NumArgs 1 args

isString :: [LispVal] -> ThrowsError LispVal
isString [String _] =return LispTrue
isString [_] = return LispFalse
isString args = throwError $ NumArgs 1 args

isFixnum :: [LispVal] -> ThrowsError LispVal
isFixnum [Fixnum _] = return LispTrue
isFixnum [_] = return LispFalse
isFixnum args = throwError $ NumArgs 1 args

isList :: [LispVal] -> ThrowsError LispVal
isList [List _] = return LispTrue
isList [DotList _ rest] = isList [rest]
isList [_] = return LispFalse
isList args = throwError $ NumArgs 1 args

isChar :: [LispVal] -> ThrowsError LispVal
isChar [Char _] = return LispTrue
isChar [_] = return LispFalse
isChar args = throwError $ NumArgs 1 args

isProcedure :: [LispVal] -> ThrowsError LispVal
isProcedure [Func _] = return LispTrue
isProcedure [IOFunc _] = return LispTrue
isProcedure [Closure {}] = return LispTrue
isProcedure [_] = return LispFalse
isProcedure args = throwError $ NumArgs 1 args

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DotList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DotList [x] y] = return y
cdr [DotList (_:xs) y] = return $ DotList xs y
cdr [arg] = throwError $ TypeMismatch "pair" arg
cdr args = throwError $ NumArgs 1 args

-- x + List = List
-- x + DotList = DotList
cons :: [LispVal] -> ThrowsError LispVal
cons [a, List b] = return $ List (a:b)
cons [a, DotList b c] = return $ DotList (a:b) c
cons [a, b] = return $ DotList [a] b
cons args = throwError $ NumArgs 2 args

eqv :: [LispVal] -> ThrowsError LispVal
eqv [LispTrue, LispTrue] = return LispTrue
eqv [LispFalse, LispFalse] = return LispTrue
eqv [LispTrue, LispFalse] = return LispFalse
eqv [LispFalse, LispTrue] = return LispFalse
eqv [Char c1, Char c2] = return $ if c1 == c2 then LispTrue else LispFalse
eqv [Fixnum arg1, Fixnum arg2] = return $ if arg1 == arg2 then LispTrue else LispFalse
eqv [String arg1, String arg2] = return $ if arg1 == arg2 then LispTrue else LispFalse
eqv [Symbol arg1, Symbol arg2] = return $ if arg1 == arg2 then LispTrue else LispFalse
eqv [DotList xs x, DotList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = return $ if (length arg1 == length arg2) && all eqvPair (zip arg1 arg2) then LispTrue else LispFalse
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right LispTrue -> True
                               Right _ -> False

eqv [_, _] = return LispFalse
eqv badArgList = throwError $ NumArgs 2 badArgList


equal :: [LispVal] -> ThrowsError LispVal
equal [LispTrue, LispTrue] = return LispTrue
equal [LispFalse, LispFalse] = return LispTrue
equal [LispTrue, LispFalse] = return LispFalse
equal [LispFalse, LispTrue] = return LispFalse
equal [Char c1, Char c2] = return $ if c1 == c2 then LispTrue else LispFalse
equal [Fixnum arg1, Fixnum arg2] = return $ if arg1 == arg2 then LispTrue else LispFalse
equal [String arg1, String arg2] = return $ if arg1 == arg2 then LispTrue else LispFalse
equal [Symbol arg1, Symbol arg2] = return $ if arg1 == arg2 then LispTrue else LispFalse
equal [DotList xs x, DotList ys y] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [List arg1, List arg2] = return $ if (length arg1 == length arg2) && all equalPair (zip arg1 arg2) then LispTrue else LispFalse
    where equalPair (x1, x2) = case equal [x1, x2] of
                               Left err -> False
                               Right LispTrue -> True
                               Right _ -> False

equal [_, _] = return LispFalse
equal badArgList = throwError $ NumArgs 2 badArgList
{-
eqv' :: LispVal -> LispVal -> Bool
eqv' (Bool arg1) (Bool arg2) = arg1 == arg2
eqv' (Fixnum arg1) (Fixnum arg2) = arg1 == arg2
eqv' (String arg1) (String arg2) = arg1 == arg2
eqv' (Symbol arg1) (Symbol arg2) = arg1 == arg2
eqv' (DotList xs x) (DotList ys y) = eqv' (List $ xs ++ [x]) (List $ ys ++ [y])
eqv' (List arg1) (List arg2) = (length arg1 == length arg2) && (and $ map eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = eqv' x1 x2
eqv' _ _ = LispFalse
-}

makeString :: [LispVal] -> ThrowsError LispVal
makeString [Fixnum k] = makeString' (fromInteger k) '\0'
makeString [Fixnum k, Char c] = makeString' (fromInteger k) c
makeString _ = throwError $ Default "Argument Types Error"

makeString' :: Int -> Char -> ThrowsError LispVal
makeString' k c = let x = c : x in
                  if k >= 0 then return $ String $ take k x
                  else throwError $ Default "expects argument of type <non-negative exact integer>"

-- (string) => ""
-- (string #\a) => "a"
-- (string #\a #\b) => "ab"
stringFromCharList :: [LispVal] -> ThrowsError LispVal
stringFromCharList [] = return $ String []
stringFromCharList [Char arg] = return $ String [arg]
stringFromCharList (Char arg : xs) = do
  String rest <- stringFromCharList xs
  return $ String $ arg:rest
stringFromCharList [args] = throwError $ TypeMismatch "Char" args

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String arg] = return $ Fixnum $ toInteger $ length arg
stringLength [badArg] = throwError $ TypeMismatch "String" badArg
stringLength badArgs = throwError $ NumArgs 1 badArgs

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend args = do
    r <- foldlM append "" args
    return $ String r
  where
    append :: String -> LispVal -> ThrowsError String
    append a (String b) = return $ a ++ b
    append a b = throwError $ TypeMismatch "string" b


stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [String arg0, Fixnum arg1] =
    if arg1 < 0 || arg1 >= toInteger (length arg0)
    then throwError $ Default "String index out of range"
    else return $ Char $ arg0 !! fromInteger arg1


--
-- 进程控制
--

sleepProc :: [LispVal] -> InterpM LispVal
sleepProc [Fixnum n] = liftIO $ threadDelay (fromInteger n * 1000000) >> return Undefined
sleepProc args = throwError $ NumArgs 1 args


-- 内存管理
collectGarbage :: [LispVal] -> InterpM LispVal
collectGarbage [] = liftIO performGC >> return Undefined
collectGarbage args = throwError $ NumArgs 0 args

idiv :: Integer -> Integer -> InterpM LispVal
idiv a 0 = throwError ZeroDivision
idiv a b = return Undefined

type Opcode = Int
type DispatchFunc = Opcode -> [LispVal] -> ThrowsError LispVal

opIAdd = 0

-- 运算符映射表
opcodes :: [(String, DispatchFunc, Int, Int, Opcode)]
opcodes =
    [
      ("+", numericOp, 0, 0xfff, opIAdd)
    ]

numericOp :: DispatchFunc
numericOp opcode args = do
    let (name, dis, min, max, op) = opcodes !! opcode
        arity = length args
    -- FIXME 定义新的错误类型，表示超出最大最小arity的情况
    -- 检查参数数量
    if arity < min then throwError $ NumArgs min args
    else if arity >= max then throwError $ NumArgs max args
    else numericOp' opcode args

numericOp' :: DispatchFunc
numericOp' opIAdd [] = return $ Fixnum 0
numericOp' opIAdd args = (Fixnum . sum) <$> mapM unpackNum args

-- 内置的纯函数查询表
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
    [
      -- 数值算符
      ("+", numericOp opIAdd),
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
primitivesIo :: [(String, [LispVal] -> InterpM LispVal)]
primitivesIo =
    [
     ("load", loadProc),
     ("eval", evalProc),
     ("apply", applyProc),
     ("call-with-current-continuation", callcc),
    --  ("call-with-failure-continuation", callfc),
     ("display", display),
     ("write-char", writeChar),
     ("open-input-file", makePort ReadMode),
     ("open-output-file", makePort WriteMode),
     ("close-input-port", closePort),
     ("close-output-port", closePort),
     ("flush-output", flushOutputProc),  -- 非r5rs
     ("current-input-port", currentInputPort),
     ("current-output-port", currentOutputPort),
     ("read", readProc),
     ("read-string", readString),  -- 非r5rs
     ("write", writeProc),
     ("read-contents", readContents),
     ("read-all", readAll),

     -- 控制
     ("sleep", sleepProc),
     ("collect-garbage", collectGarbage),
     ("quit", quitProc)

     -- 互操作
     --("load-ffi", loadHaskellFunction)
    ]
