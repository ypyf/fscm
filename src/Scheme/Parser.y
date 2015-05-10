{
module Scheme.Parser (parseLisp, readLisp) where

import Control.Monad.Error
import Data.Char

import Scheme.Scan
import Scheme.Types

type ParseError = Either String
}

-- Parser
%partial parseLisp Lisp

%tokentype { Token }
%error { parseError }
%monad { ParseError }
--%lexer { <lexer> } { EOFT }

-- terminal
%token
eof             { T p EOFT $$ }
boolean 	{ T p BoolT $$ }
character 	{ T p CharT $$ }
string 		{ T p StringT $$ }
ident 		{ T p IdentT $$ }
number		{ T p NumberT $$ }
'#' 		{ T $$ PunctuT "#" }
'(' 		{ T $$ PunctuT "(" }
')' 		{ T $$ PunctuT ")" }
'[' 		{ T $$ PunctuT "[" }
']' 		{ T $$ PunctuT "]" }
'.' 		{ T $$ PunctuT "." }
'\'' 		{ T $$ PunctuT "'" } 		-- Quote
'`' 		{ T $$ PunctuT "`" }  		-- Quasiquote
',' 		{ T $$ PunctuT "," }  		-- Unquote
",@"		{ T $$ SplicingT ",@" } 	-- UnquoteSplicing
"#(" 		{ T $$ VectorT "#(" }  		-- Vector

%%

Lisp : {- empty -} { [] }  --| Datum { [$1] }
     | Datum Lisp  { $1:$2 }

Datum : eof          { EOF }
      | boolean      { if $1 == "#t" || $1 == "#T" then LispTrue else LispFalse }
      | number       { Fixnum (read $1 ::Integer) }
      | character    { Char (head $1) }
      | string       { String $1 }
      | ident        { Symbol $1 }
      | List         { $1 }
      | Vector       { $1 }

Lisp1 : Datum { [$1] }
      | Datum Lisp1 { $1 : $2 }

-- 注意虽然根据DotList类型的定义可以表示严格表
-- 但Parser保证返回的DotList不是List（严格表），即它的cdr部分不是List
List : '(' Lisp1 '.' Datum ')' { case $4 of { DotList s0 s1 -> DotList ($2++s0) s1; List vals -> List ($2++vals); _ -> DotList $2 $4} }  -- 有必要优先解析点对吗??
     | '[' Lisp1 '.' Datum ']' { case $4 of { DotList s0 s1 -> DotList ($2++s0) s1; List vals -> List ($2++vals); _ -> DotList $2 $4} }  -- 有必要优先解析点对吗??
  -- | '(' Lisp1 '.' Datum error {% Left $ showPosn $1 ++ " expected a ')' to close '('" }
     | '(' '.' Datum ')' { $3 }
     | '[' '.' Datum ']' { $3 }
  -- | '(' Lisp1 '.' error {% Left $ showPosn $1 ++ " expected a ')' to close '('" }
  -- | '(' '.' Datum error {% Left $ showPosn $1 ++ " expected a ')' to close '('" }
     | '(' Lisp1 ')' { List $2 }
     | '(' ')'  { List [] }
     | '[' Lisp1 ']' { List $2 }
     | '[' ']'  { List [] }
  -- | '(' Datum error {% Left $ showPosn $1 ++ " expected a ')' to close '('" }
  -- | '(' Lisp1 error {% Left $ showPosn $1 ++ " expected a ')' to close '('" }
     | '\'' Datum { List [Symbol "quote", $2] }
     | '`' Datum { List [Symbol "quasiquote", $2] }
     | ',' Datum { List [Symbol "unquote", $2] }
     | ",@" Datum { List [Symbol "unquote-splicing", $2] }

Vector : "#(" Lisp ')' { Vector $2 }
       | "#(" Lisp error {% Left $ showPosn $1 ++ " error: expected a ')' to close '#('" }

-- 注意:没有精确性前缀的数只要有出现至少一个#就是非精确数
{-
Number : Int10	{ $1 }
	   | Frac10 { $1 }

-- 有符号10进制整数
SInt10 : uint10					{ $1 }
       | '+' uint10				{ $2 }
       | '-' uint10				{ '-':$2 }
-- 精确数
Int10 : SInt10					{ VInt (read $1 ::Integer) }
	  | radix10 SInt10			{ VInt (read $2 ::Integer) }
	  | prefix10_e SInt10 ManyS	{ VInt (read $2 * 10^$3) }
	  
ManyS : {- empty -} 			{ 0 }
      |	'#' ManyS				{ 1 + $2 }

Frac10 : SInt10 '/' uint10			{ VRatio (read $1 % read $3) }
	   | radix10 SInt10 '/' uint10	{ VRatio (read $2 % read $4) }
	   | prefix10_e SInt10 ManyS '/' uint10 ManyS { VRatio $ (read $2 * 10^$3) % (read $5 * 10^$6) }
-}

{
-- parseError与top level parser有着相同的签名
parseError :: [Token] -> ParseError a
parseError (T p tkn lexeme:xs) = Left $ "syntax error: " ++ showPosn p ++ ": " ++ lexeme
parseError _                   = Left "syntax error"

readLisp :: String -> InterpM [Lisp]
readLisp input =
    case scanner input of
        Left x -> throwError $ ParseError x
        Right toks ->
            case parseLisp toks of
                Left x -> throwError $ ParseError x
                Right x -> return x

{- main = do
  s <- getContents
  case scanner s of
    Left s -> error s
    Right tks -> case parseLisp tks of
                  Ok x -> print x
                  Failed x -> putStrLn x
-}

}
