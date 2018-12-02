{
module Scheme.Parser (parseLisp, readLisp) where

import Control.Monad.Error
import Data.Char

import Scheme.Scan
import Scheme.Types
}

-- Parser
%partial parseLisp Program

%tokentype { Token }
%error     { parseError }
%monad     { ParseResult }
--%lexer   { <lexer> } { EOFT }

-- terminal
%token
eof         { T p EOFT $$ }
boolean 	{ T p BoolT $$ }
character 	{ T p CharT $$ }
string 		{ T p StringT $$ }
ident 		{ T p IdentT $$ }
number		{ T p NumberT $$ }
fixnum		{ T p FixnumT $$ }
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

Program : {- empty -} { [] }
        | Datum Program { $1 : $2 }

Datum : eof          { EOF }
      | boolean      { if $1 == "#t" || $1 == "#T" then LispTrue else LispFalse }
      | fixnum       { Fixnum (read $1 ::Integer) }
      | character    { Char (head $1) }
      | string       { String $1 }
      | ident        { Symbol $1 }
      | List         { $1 }
      | Vector       { $1 }

DatumList : {- empty -} { [] }
          | Datum DatumList { $1 : $2 }

-- ËäÈ»¸ù¾ÝDotListÀàÐÍµÄ¶¨Òå¿ÉÒÔ±íÊ¾ÑÏ¸ñ±í
-- µ«Parser±£Ö¤·µ»ØµÄDotList²»ÊÇList£¨ÑÏ¸ñ±í£©£¬¼´ËüµÄcdr²¿·Ö²»ÊÇList
List : '(' DatumList '.' Datum ')' { case $4 of { DotList s0 s1 -> DotList ($2++s0) s1; List vals -> List ($2++vals); _ -> DotList $2 $4} }  -- ÓÐ±ØÒªÓÅÏÈ½âÎöµã¶ÔÂð??
     | '[' DatumList '.' Datum ']' { case $4 of { DotList s0 s1 -> DotList ($2++s0) s1; List vals -> List ($2++vals); _ -> DotList $2 $4} }  -- ÓÐ±ØÒªÓÅÏÈ½âÎöµã¶ÔÂð??
     --| '(' DatumList '.' Datum error {% Left $ showPosn $1 ++ " expected a ')' to close '('" }
     --| '(' '.' Datum ')' { $3 } -- ±ê×¼ÖÐËÆºõÃ»ÓÐÕâÖÖÐ´·¨: (.x) => x
     --| '[' '.' Datum ']' { $3 }
  -- | '(' DatumList '.' error {% Left $ showPosn $1 ++ " expected a ')' to close '('" }
  -- | '(' '.' Datum error {% Left $ showPosn $1 ++ " expected a ')' to close '('" }
     | '(' DatumList ')' { List $2 }
     | '[' DatumList ']' { List $2 }
  -- | '(' Datum error {% Left $ showPosn $1 ++ " expected a ')' to close '('" }
  -- | '(' DatumList error {% Left $ showPosn $1 ++ " expected a ')' to close '('" }
     | '\'' Datum { List [Symbol "quote", $2] }
     | '`' Datum { List [Symbol "quasiquote", $2] }
     | ',' Datum { List [Symbol "unquote", $2] }
     | ",@" Datum { List [Symbol "unquote-splicing", $2] }

Vector : "#(" DatumList ')' { Vector $2 }
       | "#(" DatumList error {% Left $ showPosn $1 ++ " error: expected a ')' to close '#('" }

-- Ã»ÓÐ¾«È·ÐÔÇ°×ºµÄÊýÖ»ÒªÓÐ³öÏÖÖÁÉÙÒ»¸ö#¾ÍÊÇ·Ç¾«È·Êý
{-
Number : Int10	{ $1 }
	   | Frac10 { $1 }

-- ÓÐ·ûºÅ10½øÖÆÕûÊý
SInt10 : uint10					{ $1 }
       | '+' uint10				{ $2 }
       | '-' uint10				{ '-':$2 }
-- ¾«È·Êý
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

type ParseResult = Either String

-- parseErrorÓëparseLisp(top level parser)ÓÐ×ÅÏàÍ¬µÄÇ©Ãû
parseError :: [Token] -> ParseResult a
parseError []                    = Left $ "syntax error"
parseError (T pos tkn lexeme:xs) = Left $ "syntax error: " ++ showPosn pos ++ ": " ++ lexeme

-- ¶ÁÈ¡Lisp±í´ïÊ½
readLisp :: String -> InterpM LispVal
readLisp input =
    case scanner input of
        Left x -> throwError $ ParseError x
        Right toks ->
            case parseLisp toks of
                Left x -> throwError $ ParseError x
                Right x -> return $ Values x

{- main = do
  s <- getContents
  case scanner s of
    Left s -> error s
    Right tks -> case parseLisp tks of
                  Ok x -> print x
                  Failed x -> putStrLn x
-}

}
