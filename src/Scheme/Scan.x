-- 注意:Alex只能识别无BOM的UTF-8格式
-- code
{             
module Scheme.Scan
	(
	  scanner
	, showPosn
	, Token(..)
	, Tkn(..)
	, AlexPosn(..)
	) where

import qualified Data.Char as Char
}

-- wrapper
%wrapper "monad"

-- macro definition
$digit    	= [0-9]
$hexdig   	= [0-9 A-F a-f]
$octal    	= 0-7
$letter 	= [a-zA-Z] -- alphabetic characters

$special_initial = [\!\$\/\%\&\*\:\<\=\>\?\^\_\~]
$special_subsequent = [\+\-\.\@]

@initial = $letter | $special_initial
@subsequent = @initial | $digit | $special_subsequent
@ident = @initial@subsequent* | "+" | "-" | "..." | "->"
@bool = "#t" | "#T" | "#f" | "#F"
-- Case is significant in #\<character>, but not in #\<charactername> 
@char = \#\\. | \#\\[Ss][Pp][Aa][Cc][Ee] | \#\\[Nn][Ee][Ww][Ll][Ii][Nn][Ee]
--@string = \"(\.|[^\"\\])*\"
-- TODO 数字和字符串解析放到Parser.y
@string = \"(\.|[^\"\\]|\\[\'\"\\abfnrtv]|\\[0-7]{1,3}|\\[UuXx][0-9 a-f A-F]+)*\"
@comment = ";".*
@ws      = $white+ | @comment

----------------------------------------------------
-- 数值字面量语法示例
----------------------------------------------------

-- 指数符号
$exp_marker = [EeSsFfDdLl]
-- 虚数符号
--$imag_marker = [Ii]

-- 可空非终结符,使用时需要在后面加上问号(?)
@exact = "#"[Ee]
@inexact = "#"[Ii]
@exactness = @exact | @inexact
@sign = "+" | "-"
@suffix = $exp_marker @sign? $digit+

-- 我们暂时之定义了10进制数

-- 进制前缀
@radix10 = "#"[Dd]
-- 前缀
@prefix10 = @radix10? @exactness? | @exactness? @radix10?
-- 精确前缀
@prefix10_e = @radix10? @exact | @exact @radix10?
-- 非精确前缀
@prefix10_i = @radix10? @inexact? | @inexact? @radix10?

@uint10 = $digit+ "#"*

@decimal10 = @uint10 @suffix?
		   | "." $digit+ "#"* @suffix?
		   | $digit+ "." $digit* "#"* @suffix?
		   | $digit+ "#" "." "#"* @suffix?

@ureal10 = @uint10 
	     | @uint10 "/" @uint10
		 | @decimal10

@real10 = @sign? @ureal10

@complex10 = @real10 | @real10 "@" @real10
           | @real10 "+" @ureal10 [Ii] | @real10 "-" @ureal10 [Ii]
		   | @real10 "+"  [Ii] | @real10 "-" [Ii]
		   | "+" @ureal10 [Ii] | "-" @ureal10 [Ii] | "+" [Ii] | "-" [Ii]

@num10 = @prefix10 @complex10

@number = @num10

@uint10 = $digit+
@int10 = @sign? $digit+
@frac10e = @int10 "/" @uint10

---- 精确数
-- 没有精确性前缀的数只要有至少一个#就是非精确数
-- @int10 = @radix10? @sign? $digit+ 
--        | @prefix10_e @sign? $digit+ "#"*
-- 	   
-- @frac10 = @radix10? @sign? $digit+ "/" $digit+
-- 		| @prefix10_e @sign? $digit+ "#"* "/" $digit+ "#"*
--         
-- -- 非精确数
-- @ieint10 = @radix10? @sign? $digit+ "#"+
--          | @prefix10_i @sign? $digit+ "#"*
-- 		 
-- @iefrac10 = @radix10? @sign? $digit+ "#"+ "/" $digit+
-- 		  | @radix10? @sign? $digit+ "/" $digit+ "#"+
-- 		  | @radix10? @sign? $digit+ "#"+ "/" $digit+ "#"+
-- 		  | @prefix10_i @sign? $digit+ "#"* "/" $digit+ "#"*
-- 		  
-- @real10 = @prefix10 @sign? @decimal10
--        | @prefix10_inexact @sign? @decimal10

-- rule Note the order!
scheme :-
@ws					{ skip }
<0>   	@bool 				{ mkToken BoolT }
<0>		@int10				{ number }
<0>   	@char          		{ char }
<0>   	@string        		{ string }
<0>		@ident 				{ ident }
<0>		[\(\)\#\@\'\`\,\.] 	{ mkToken PunctuT }
<0>    	"#("           		{ mkToken VectorT }
<0>    	",@"           		{ mkToken SplicingT }
        .                       { badToken }

-- code
{
data Token = T AlexPosn Tkn String
    deriving (Show)

data Tkn = EOFT
         | IdentT
         | BoolT
         | NumberT
         | CharT
         | StringT
         | PunctuT 			-- Punctuation
         | VectorT 			-- #(
         | SplicingT       	-- ,@
    deriving (Eq,Show)

mkToken :: Tkn -> AlexInput -> Int -> Alex Token
mkToken tkn (p, _, _, str) len = return $ T p tkn (take len str)

-- read函数无法识别数字前的加号
-- 因此需要去掉这个符号
dropSign ('+':xs) = xs
dropSign x        = x

number (p, _, _, str) len = return $ T p NumberT $ map Char.toLower (dropSign (take len str))

char (p, _, _, str) len = return $ T p CharT (extract len str)
	where extract len str =
			let '#':'\\':x = take len str
	        in case map Char.toLower x of
				"space" -> ['\x20']
				"newline" -> ['\x0a']
				_ -> x

string (p, _, _, str) len = return $ T p StringT (extractString len str)

-- extract string and remove double quotes
extractString len str = escape $ take (len-2) (tail str)
  where
    escape [] = []
    -- 转义字符
    escape str@('\\':'a':xs) = '\a' : escape xs
    escape str@('\\':'b':xs) = '\b' : escape xs
    escape str@('\\':'f':xs) = '\f' : escape xs
    escape str@('\\':'n':xs) = '\n' : escape xs
    escape str@('\\':'r':xs) = '\r' : escape xs
    escape str@('\\':'t':xs) = '\t' : escape xs
    escape str@('\\':'v':xs) = '\v' : escape xs
    --escape str@('\\':'X':xs) = -- 16进制
    --escape str@('\\':'x':xs) = -- 16进制
    --escape str@('\\':x:xs) = -- 8进制
    --escape str@('\\':'U':xs) = -- 16进制Unicode
    --escape str@('\\':'u':xs) = -- 16进制Unicode
    escape str@(x:xs) = x : escape xs

ident (p, _, _, str) len = return $ T p IdentT (extract len str)
  where extract = (map Char.toLower .) . take

alexEOF :: Alex Token
alexEOF = return $ T undefined EOFT ""

badToken :: AlexInput -> Int -> Alex Token
badToken (_, _, _, input) len = alexError $ "illegal character " ++ show (head input)

{-
alexError s = do
  (p, _, _, input) <- alexGetInput
  alexError (showPosn p ++ ": " ++ s ++
		   (if (not (null input))
		     then " before " ++ show (head input)
		     else " at end of file"))
-}

showPosn (AlexPn _ line col) = show line ++ ':': show col

scanner :: String -> Either String [Token]
scanner str = runAlex str loop
  where
    loop :: Alex [Token] -- AlexState -> Either String (AlexState, [Token])
    loop = do
        tok <- alexMonadScan
        case tok of
            T _ EOFT _ -> return []
            t -> do
                tks <- loop
                return $ t : tks
}
