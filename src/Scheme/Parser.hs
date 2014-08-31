{-# OPTIONS_GHC -w #-}
module Scheme.Parser (parseLisp, readLisp) where

import Control.Monad.Error
import Data.Char

import Scheme.Scan
import Scheme.Types

type ParseError = Either String

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn t4 t5 t6 t7 t8
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8

action_0 (9) = happyShift action_6
action_0 (10) = happyShift action_7
action_0 (11) = happyShift action_8
action_0 (12) = happyShift action_9
action_0 (13) = happyShift action_10
action_0 (14) = happyShift action_11
action_0 (16) = happyShift action_12
action_0 (19) = happyShift action_13
action_0 (20) = happyShift action_14
action_0 (21) = happyShift action_15
action_0 (22) = happyShift action_16
action_0 (23) = happyShift action_17
action_0 (4) = happyGoto action_2
action_0 (5) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 _ = happyReduce_1

action_1 _ = happyFail

action_2 (24) = happyAccept
action_2 _ = happyFail

action_3 (9) = happyShift action_6
action_3 (10) = happyShift action_7
action_3 (11) = happyShift action_8
action_3 (12) = happyShift action_9
action_3 (13) = happyShift action_10
action_3 (14) = happyShift action_11
action_3 (16) = happyShift action_12
action_3 (19) = happyShift action_13
action_3 (20) = happyShift action_14
action_3 (21) = happyShift action_15
action_3 (22) = happyShift action_16
action_3 (23) = happyShift action_17
action_3 (4) = happyGoto action_27
action_3 (5) = happyGoto action_3
action_3 (7) = happyGoto action_4
action_3 (8) = happyGoto action_5
action_3 _ = happyReduce_1

action_4 _ = happyReduce_9

action_5 _ = happyReduce_10

action_6 _ = happyReduce_3

action_7 _ = happyReduce_4

action_8 _ = happyReduce_6

action_9 _ = happyReduce_7

action_10 _ = happyReduce_8

action_11 _ = happyReduce_5

action_12 (9) = happyShift action_6
action_12 (10) = happyShift action_7
action_12 (11) = happyShift action_8
action_12 (12) = happyShift action_9
action_12 (13) = happyShift action_10
action_12 (14) = happyShift action_11
action_12 (16) = happyShift action_12
action_12 (18) = happyShift action_26
action_12 (19) = happyShift action_13
action_12 (20) = happyShift action_14
action_12 (21) = happyShift action_15
action_12 (22) = happyShift action_16
action_12 (23) = happyShift action_17
action_12 (4) = happyGoto action_23
action_12 (5) = happyGoto action_24
action_12 (6) = happyGoto action_25
action_12 (7) = happyGoto action_4
action_12 (8) = happyGoto action_5
action_12 _ = happyReduce_1

action_13 (9) = happyShift action_6
action_13 (10) = happyShift action_7
action_13 (11) = happyShift action_8
action_13 (12) = happyShift action_9
action_13 (13) = happyShift action_10
action_13 (14) = happyShift action_11
action_13 (16) = happyShift action_12
action_13 (19) = happyShift action_13
action_13 (20) = happyShift action_14
action_13 (21) = happyShift action_15
action_13 (22) = happyShift action_16
action_13 (23) = happyShift action_17
action_13 (5) = happyGoto action_22
action_13 (7) = happyGoto action_4
action_13 (8) = happyGoto action_5
action_13 _ = happyFail

action_14 (9) = happyShift action_6
action_14 (10) = happyShift action_7
action_14 (11) = happyShift action_8
action_14 (12) = happyShift action_9
action_14 (13) = happyShift action_10
action_14 (14) = happyShift action_11
action_14 (16) = happyShift action_12
action_14 (19) = happyShift action_13
action_14 (20) = happyShift action_14
action_14 (21) = happyShift action_15
action_14 (22) = happyShift action_16
action_14 (23) = happyShift action_17
action_14 (5) = happyGoto action_21
action_14 (7) = happyGoto action_4
action_14 (8) = happyGoto action_5
action_14 _ = happyFail

action_15 (9) = happyShift action_6
action_15 (10) = happyShift action_7
action_15 (11) = happyShift action_8
action_15 (12) = happyShift action_9
action_15 (13) = happyShift action_10
action_15 (14) = happyShift action_11
action_15 (16) = happyShift action_12
action_15 (19) = happyShift action_13
action_15 (20) = happyShift action_14
action_15 (21) = happyShift action_15
action_15 (22) = happyShift action_16
action_15 (23) = happyShift action_17
action_15 (5) = happyGoto action_20
action_15 (7) = happyGoto action_4
action_15 (8) = happyGoto action_5
action_15 _ = happyFail

action_16 (9) = happyShift action_6
action_16 (10) = happyShift action_7
action_16 (11) = happyShift action_8
action_16 (12) = happyShift action_9
action_16 (13) = happyShift action_10
action_16 (14) = happyShift action_11
action_16 (16) = happyShift action_12
action_16 (19) = happyShift action_13
action_16 (20) = happyShift action_14
action_16 (21) = happyShift action_15
action_16 (22) = happyShift action_16
action_16 (23) = happyShift action_17
action_16 (5) = happyGoto action_19
action_16 (7) = happyGoto action_4
action_16 (8) = happyGoto action_5
action_16 _ = happyFail

action_17 (9) = happyShift action_6
action_17 (10) = happyShift action_7
action_17 (11) = happyShift action_8
action_17 (12) = happyShift action_9
action_17 (13) = happyShift action_10
action_17 (14) = happyShift action_11
action_17 (16) = happyShift action_12
action_17 (19) = happyShift action_13
action_17 (20) = happyShift action_14
action_17 (21) = happyShift action_15
action_17 (22) = happyShift action_16
action_17 (23) = happyShift action_17
action_17 (4) = happyGoto action_18
action_17 (5) = happyGoto action_3
action_17 (7) = happyGoto action_4
action_17 (8) = happyGoto action_5
action_17 _ = happyReduce_1

action_18 (1) = happyShift action_32
action_18 (17) = happyShift action_33
action_18 _ = happyFail

action_19 _ = happyReduce_19

action_20 _ = happyReduce_18

action_21 _ = happyReduce_17

action_22 _ = happyReduce_16

action_23 (17) = happyShift action_31
action_23 _ = happyFail

action_24 (9) = happyShift action_6
action_24 (10) = happyShift action_7
action_24 (11) = happyShift action_8
action_24 (12) = happyShift action_9
action_24 (13) = happyShift action_10
action_24 (14) = happyShift action_11
action_24 (16) = happyShift action_12
action_24 (18) = happyReduce_11
action_24 (19) = happyShift action_13
action_24 (20) = happyShift action_14
action_24 (21) = happyShift action_15
action_24 (22) = happyShift action_16
action_24 (23) = happyShift action_17
action_24 (4) = happyGoto action_27
action_24 (5) = happyGoto action_24
action_24 (6) = happyGoto action_30
action_24 (7) = happyGoto action_4
action_24 (8) = happyGoto action_5
action_24 _ = happyReduce_1

action_25 (18) = happyShift action_29
action_25 _ = happyFail

action_26 (9) = happyShift action_6
action_26 (10) = happyShift action_7
action_26 (11) = happyShift action_8
action_26 (12) = happyShift action_9
action_26 (13) = happyShift action_10
action_26 (14) = happyShift action_11
action_26 (16) = happyShift action_12
action_26 (19) = happyShift action_13
action_26 (20) = happyShift action_14
action_26 (21) = happyShift action_15
action_26 (22) = happyShift action_16
action_26 (23) = happyShift action_17
action_26 (5) = happyGoto action_28
action_26 (7) = happyGoto action_4
action_26 (8) = happyGoto action_5
action_26 _ = happyFail

action_27 _ = happyReduce_2

action_28 (17) = happyShift action_35
action_28 _ = happyFail

action_29 (9) = happyShift action_6
action_29 (10) = happyShift action_7
action_29 (11) = happyShift action_8
action_29 (12) = happyShift action_9
action_29 (13) = happyShift action_10
action_29 (14) = happyShift action_11
action_29 (16) = happyShift action_12
action_29 (19) = happyShift action_13
action_29 (20) = happyShift action_14
action_29 (21) = happyShift action_15
action_29 (22) = happyShift action_16
action_29 (23) = happyShift action_17
action_29 (5) = happyGoto action_34
action_29 (7) = happyGoto action_4
action_29 (8) = happyGoto action_5
action_29 _ = happyFail

action_30 _ = happyReduce_12

action_31 _ = happyReduce_15

action_32 _ = happyReduce_21

action_33 _ = happyReduce_20

action_34 (17) = happyShift action_36
action_34 _ = happyFail

action_35 _ = happyReduce_14

action_36 _ = happyReduce_13

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 ([]
	)

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1:happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 _
	 =  HappyAbsSyn5
		 (EOF
	)

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyTerminal (T p BoolT happy_var_1))
	 =  HappyAbsSyn5
		 (if happy_var_1 == "#t" || happy_var_1 == "#T" then LispTrue else LispFalse
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyTerminal (T p NumberT happy_var_1))
	 =  HappyAbsSyn5
		 (Number (read happy_var_1 ::Integer)
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyTerminal (T p CharT happy_var_1))
	 =  HappyAbsSyn5
		 (Char (head happy_var_1)
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 (HappyTerminal (T p StringT happy_var_1))
	 =  HappyAbsSyn5
		 (String happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  5 happyReduction_8
happyReduction_8 (HappyTerminal (T p IdentT happy_var_1))
	 =  HappyAbsSyn5
		 (Symbol happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_1  5 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_1  6 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  6 happyReduction_12
happyReduction_12 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 : happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 5 7 happyReduction_13
happyReduction_13 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (case happy_var_4 of { DotList s0 s1 -> DotList (happy_var_2++s0) s1; List vals -> List (happy_var_2++vals); _ -> DotList happy_var_2 happy_var_4}
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 4 7 happyReduction_14
happyReduction_14 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (happy_var_3
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_3  7 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (List happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  7 happyReduction_16
happyReduction_16 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (List [Symbol "quote", happy_var_2]
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  7 happyReduction_17
happyReduction_17 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (List [Symbol "quasiquote", happy_var_2]
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_2  7 happyReduction_18
happyReduction_18 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (List [Symbol "unquote", happy_var_2]
	)
happyReduction_18 _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_2  7 happyReduction_19
happyReduction_19 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (List [Symbol "unquote-splicing", happy_var_2]
	)
happyReduction_19 _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  8 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Vector happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyMonadReduce 3 8 happyReduction_21
happyReduction_21 (_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (T happy_var_1 VectorT "#(")) `HappyStk`
	happyRest) tk
	 = happyThen (( Left $ showPosn happy_var_1 ++ " error: expected a ')' to close '#('")
	) (\r -> happyReturn (HappyAbsSyn8 r))

happyNewToken action sts stk [] =
	action 24 24 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	T p EOFT happy_dollar_dollar -> cont 9;
	T p BoolT happy_dollar_dollar -> cont 10;
	T p CharT happy_dollar_dollar -> cont 11;
	T p StringT happy_dollar_dollar -> cont 12;
	T p IdentT happy_dollar_dollar -> cont 13;
	T p NumberT happy_dollar_dollar -> cont 14;
	T happy_dollar_dollar PunctuT "#" -> cont 15;
	T happy_dollar_dollar PunctuT "(" -> cont 16;
	T happy_dollar_dollar PunctuT ")" -> cont 17;
	T happy_dollar_dollar PunctuT "." -> cont 18;
	T happy_dollar_dollar PunctuT "'" -> cont 19;
	T happy_dollar_dollar PunctuT "`" -> cont 20;
	T happy_dollar_dollar PunctuT "," -> cont 21;
	T happy_dollar_dollar SplicingT ",@" -> cont 22;
	T happy_dollar_dollar VectorT "#(" -> cont 23;
	_ -> happyError' (tk:tks)
	}

happyError_ 24 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => ParseError a -> (a -> ParseError b) -> ParseError b
happyThen = (>>=)
happyReturn :: () => a -> ParseError a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> ParseError a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> ParseError a
happyError' = parseError

parseLisp tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDoSeq


-- parseError与top level parser有相同的签名
parseError :: [Token] -> ParseError a
parseError (T p tkn lexeme:xs) = Left $ "syntax error: " ++ showPosn p ++ ": " ++ lexeme
parseError _                   = Left "syntax error"

readLisp :: String -> InterpM [Lisp]
readLisp input = case scanner input of
                   Left x -> throwError $ ParseError x
                   Right toks -> case parseLisp toks of
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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































# 5 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
