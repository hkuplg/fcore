{-# OPTIONS_GHC -w #-}
module SystemFParser where

import SystemFTokens
import SystemFLexer
import SystemF

import Data.Maybe       (fromJust)

-- parser produced by Happy Version 1.19.0

data HappyAbsSyn t4 t5 t6
	= HappyTerminal (SystemFToken)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6

action_0 (7) = happyShift action_4
action_0 (8) = happyShift action_5
action_0 (9) = happyShift action_6
action_0 (14) = happyShift action_7
action_0 (17) = happyShift action_8
action_0 (21) = happyShift action_2
action_0 (23) = happyShift action_9
action_0 (24) = happyShift action_10
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (21) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (7) = happyShift action_4
action_3 (8) = happyShift action_5
action_3 (9) = happyShift action_6
action_3 (11) = happyShift action_20
action_3 (14) = happyShift action_7
action_3 (17) = happyShift action_21
action_3 (19) = happyShift action_22
action_3 (20) = happyShift action_23
action_3 (21) = happyShift action_2
action_3 (22) = happyShift action_24
action_3 (23) = happyShift action_9
action_3 (24) = happyShift action_10
action_3 (27) = happyShift action_25
action_3 (29) = happyAccept
action_3 (4) = happyGoto action_18
action_3 (6) = happyGoto action_19
action_3 _ = happyFail

action_4 (22) = happyShift action_17
action_4 _ = happyFail

action_5 (17) = happyShift action_16
action_5 _ = happyFail

action_6 (21) = happyShift action_15
action_6 _ = happyFail

action_7 (21) = happyShift action_14
action_7 _ = happyFail

action_8 (7) = happyShift action_4
action_8 (8) = happyShift action_5
action_8 (9) = happyShift action_6
action_8 (14) = happyShift action_7
action_8 (17) = happyShift action_8
action_8 (21) = happyShift action_2
action_8 (23) = happyShift action_9
action_8 (24) = happyShift action_10
action_8 (4) = happyGoto action_12
action_8 (5) = happyGoto action_13
action_8 _ = happyFail

action_9 _ = happyReduce_8

action_10 (7) = happyShift action_4
action_10 (8) = happyShift action_5
action_10 (9) = happyShift action_6
action_10 (14) = happyShift action_7
action_10 (17) = happyShift action_8
action_10 (21) = happyShift action_2
action_10 (23) = happyShift action_9
action_10 (24) = happyShift action_10
action_10 (4) = happyGoto action_11
action_10 _ = happyFail

action_11 (7) = happyShift action_4
action_11 (8) = happyShift action_5
action_11 (9) = happyShift action_6
action_11 (11) = happyShift action_20
action_11 (14) = happyShift action_7
action_11 (17) = happyShift action_21
action_11 (19) = happyShift action_22
action_11 (20) = happyShift action_23
action_11 (21) = happyShift action_2
action_11 (22) = happyShift action_24
action_11 (23) = happyShift action_9
action_11 (24) = happyShift action_10
action_11 (25) = happyShift action_38
action_11 (27) = happyShift action_25
action_11 (4) = happyGoto action_18
action_11 (6) = happyGoto action_19
action_11 _ = happyFail

action_12 (7) = happyShift action_4
action_12 (8) = happyShift action_5
action_12 (9) = happyShift action_6
action_12 (10) = happyShift action_36
action_12 (11) = happyShift action_20
action_12 (14) = happyShift action_7
action_12 (17) = happyShift action_21
action_12 (18) = happyShift action_37
action_12 (19) = happyShift action_22
action_12 (20) = happyShift action_23
action_12 (21) = happyShift action_2
action_12 (22) = happyShift action_24
action_12 (23) = happyShift action_9
action_12 (24) = happyShift action_10
action_12 (27) = happyShift action_25
action_12 (4) = happyGoto action_18
action_12 (6) = happyGoto action_19
action_12 _ = happyFail

action_13 (18) = happyShift action_35
action_13 _ = happyFail

action_14 (15) = happyShift action_34
action_14 _ = happyFail

action_15 (11) = happyShift action_33
action_15 _ = happyFail

action_16 (21) = happyShift action_32
action_16 _ = happyFail

action_17 (11) = happyShift action_31
action_17 _ = happyFail

action_18 (7) = happyShift action_4
action_18 (8) = happyShift action_5
action_18 (9) = happyShift action_6
action_18 (11) = happyShift action_20
action_18 (14) = happyShift action_7
action_18 (17) = happyShift action_21
action_18 (19) = happyShift action_22
action_18 (20) = happyShift action_23
action_18 (21) = happyShift action_2
action_18 (22) = happyShift action_24
action_18 (23) = happyShift action_9
action_18 (24) = happyShift action_10
action_18 (27) = happyShift action_25
action_18 (4) = happyGoto action_18
action_18 (6) = happyGoto action_19
action_18 _ = happyReduce_4

action_19 (12) = happyShift action_30
action_19 _ = happyReduce_6

action_20 (28) = happyShift action_29
action_20 _ = happyFail

action_21 (7) = happyShift action_4
action_21 (8) = happyShift action_5
action_21 (9) = happyShift action_6
action_21 (14) = happyShift action_7
action_21 (17) = happyShift action_21
action_21 (19) = happyShift action_22
action_21 (20) = happyShift action_23
action_21 (21) = happyShift action_2
action_21 (22) = happyShift action_24
action_21 (23) = happyShift action_9
action_21 (24) = happyShift action_10
action_21 (4) = happyGoto action_12
action_21 (5) = happyGoto action_13
action_21 (6) = happyGoto action_28
action_21 _ = happyFail

action_22 (22) = happyShift action_27
action_22 _ = happyFail

action_23 _ = happyReduce_19

action_24 _ = happyReduce_16

action_25 (7) = happyShift action_4
action_25 (8) = happyShift action_5
action_25 (9) = happyShift action_6
action_25 (14) = happyShift action_7
action_25 (17) = happyShift action_8
action_25 (21) = happyShift action_2
action_25 (23) = happyShift action_9
action_25 (24) = happyShift action_10
action_25 (4) = happyGoto action_26
action_25 _ = happyFail

action_26 (7) = happyShift action_4
action_26 (8) = happyShift action_5
action_26 (9) = happyShift action_6
action_26 (11) = happyShift action_20
action_26 (14) = happyShift action_7
action_26 (17) = happyShift action_21
action_26 (19) = happyShift action_22
action_26 (20) = happyShift action_23
action_26 (21) = happyShift action_2
action_26 (22) = happyShift action_24
action_26 (23) = happyShift action_9
action_26 (24) = happyShift action_10
action_26 (27) = happyShift action_25
action_26 (4) = happyGoto action_18
action_26 (6) = happyGoto action_19
action_26 _ = happyReduce_7

action_27 (11) = happyShift action_49
action_27 _ = happyFail

action_28 (12) = happyShift action_30
action_28 (18) = happyShift action_48
action_28 _ = happyFail

action_29 _ = happyReduce_11

action_30 (17) = happyShift action_47
action_30 (19) = happyShift action_22
action_30 (20) = happyShift action_23
action_30 (22) = happyShift action_24
action_30 (6) = happyGoto action_46
action_30 _ = happyFail

action_31 (7) = happyShift action_4
action_31 (8) = happyShift action_5
action_31 (9) = happyShift action_6
action_31 (14) = happyShift action_7
action_31 (17) = happyShift action_8
action_31 (21) = happyShift action_2
action_31 (23) = happyShift action_9
action_31 (24) = happyShift action_10
action_31 (4) = happyGoto action_45
action_31 _ = happyFail

action_32 (13) = happyShift action_44
action_32 _ = happyFail

action_33 (8) = happyShift action_43
action_33 _ = happyFail

action_34 (7) = happyShift action_4
action_34 (8) = happyShift action_5
action_34 (9) = happyShift action_6
action_34 (14) = happyShift action_7
action_34 (17) = happyShift action_8
action_34 (21) = happyShift action_2
action_34 (23) = happyShift action_9
action_34 (24) = happyShift action_10
action_34 (4) = happyGoto action_42
action_34 _ = happyFail

action_35 _ = happyReduce_10

action_36 (7) = happyShift action_4
action_36 (8) = happyShift action_5
action_36 (9) = happyShift action_6
action_36 (14) = happyShift action_7
action_36 (17) = happyShift action_8
action_36 (21) = happyShift action_2
action_36 (23) = happyShift action_9
action_36 (24) = happyShift action_10
action_36 (4) = happyGoto action_40
action_36 (5) = happyGoto action_41
action_36 _ = happyFail

action_37 _ = happyReduce_13

action_38 (7) = happyShift action_4
action_38 (8) = happyShift action_5
action_38 (9) = happyShift action_6
action_38 (14) = happyShift action_7
action_38 (17) = happyShift action_8
action_38 (21) = happyShift action_2
action_38 (23) = happyShift action_9
action_38 (24) = happyShift action_10
action_38 (4) = happyGoto action_39
action_38 _ = happyFail

action_39 (7) = happyShift action_4
action_39 (8) = happyShift action_5
action_39 (9) = happyShift action_6
action_39 (11) = happyShift action_20
action_39 (14) = happyShift action_7
action_39 (17) = happyShift action_21
action_39 (19) = happyShift action_22
action_39 (20) = happyShift action_23
action_39 (21) = happyShift action_2
action_39 (22) = happyShift action_24
action_39 (23) = happyShift action_9
action_39 (24) = happyShift action_10
action_39 (26) = happyShift action_54
action_39 (27) = happyShift action_25
action_39 (4) = happyGoto action_18
action_39 (6) = happyGoto action_19
action_39 _ = happyFail

action_40 (7) = happyShift action_4
action_40 (8) = happyShift action_5
action_40 (9) = happyShift action_6
action_40 (10) = happyShift action_36
action_40 (11) = happyShift action_20
action_40 (14) = happyShift action_7
action_40 (17) = happyShift action_21
action_40 (19) = happyShift action_22
action_40 (20) = happyShift action_23
action_40 (21) = happyShift action_2
action_40 (22) = happyShift action_24
action_40 (23) = happyShift action_9
action_40 (24) = happyShift action_10
action_40 (27) = happyShift action_25
action_40 (4) = happyGoto action_18
action_40 (6) = happyGoto action_19
action_40 _ = happyReduce_14

action_41 _ = happyReduce_15

action_42 (7) = happyShift action_4
action_42 (8) = happyShift action_5
action_42 (9) = happyShift action_6
action_42 (11) = happyShift action_20
action_42 (13) = happyShift action_53
action_42 (14) = happyShift action_7
action_42 (17) = happyShift action_21
action_42 (19) = happyShift action_22
action_42 (20) = happyShift action_23
action_42 (21) = happyShift action_2
action_42 (22) = happyShift action_24
action_42 (23) = happyShift action_9
action_42 (24) = happyShift action_10
action_42 (27) = happyShift action_25
action_42 (4) = happyGoto action_18
action_42 (6) = happyGoto action_19
action_42 _ = happyFail

action_43 (17) = happyShift action_52
action_43 _ = happyFail

action_44 (17) = happyShift action_47
action_44 (19) = happyShift action_22
action_44 (20) = happyShift action_23
action_44 (22) = happyShift action_24
action_44 (6) = happyGoto action_51
action_44 _ = happyFail

action_45 (7) = happyShift action_4
action_45 (8) = happyShift action_5
action_45 (9) = happyShift action_6
action_45 (11) = happyShift action_20
action_45 (14) = happyShift action_7
action_45 (17) = happyShift action_21
action_45 (19) = happyShift action_22
action_45 (20) = happyShift action_23
action_45 (21) = happyShift action_2
action_45 (22) = happyShift action_24
action_45 (23) = happyShift action_9
action_45 (24) = happyShift action_10
action_45 (27) = happyShift action_25
action_45 (4) = happyGoto action_18
action_45 (6) = happyGoto action_19
action_45 _ = happyReduce_2

action_46 _ = happyReduce_18

action_47 (17) = happyShift action_47
action_47 (19) = happyShift action_22
action_47 (20) = happyShift action_23
action_47 (22) = happyShift action_24
action_47 (6) = happyGoto action_28
action_47 _ = happyFail

action_48 _ = happyReduce_20

action_49 (17) = happyShift action_47
action_49 (19) = happyShift action_22
action_49 (20) = happyShift action_23
action_49 (22) = happyShift action_24
action_49 (6) = happyGoto action_50
action_49 _ = happyFail

action_50 (12) = happyShift action_30
action_50 _ = happyReduce_17

action_51 (12) = happyShift action_30
action_51 (18) = happyShift action_58
action_51 _ = happyFail

action_52 (21) = happyShift action_57
action_52 _ = happyFail

action_53 (17) = happyShift action_47
action_53 (19) = happyShift action_22
action_53 (20) = happyShift action_23
action_53 (22) = happyShift action_24
action_53 (6) = happyGoto action_56
action_53 _ = happyFail

action_54 (7) = happyShift action_4
action_54 (8) = happyShift action_5
action_54 (9) = happyShift action_6
action_54 (14) = happyShift action_7
action_54 (17) = happyShift action_8
action_54 (21) = happyShift action_2
action_54 (23) = happyShift action_9
action_54 (24) = happyShift action_10
action_54 (4) = happyGoto action_55
action_54 _ = happyFail

action_55 (7) = happyShift action_4
action_55 (8) = happyShift action_5
action_55 (9) = happyShift action_6
action_55 (11) = happyShift action_20
action_55 (14) = happyShift action_7
action_55 (17) = happyShift action_21
action_55 (19) = happyShift action_22
action_55 (20) = happyShift action_23
action_55 (21) = happyShift action_2
action_55 (22) = happyShift action_24
action_55 (23) = happyShift action_9
action_55 (24) = happyShift action_10
action_55 (27) = happyShift action_25
action_55 (4) = happyGoto action_18
action_55 (6) = happyGoto action_19
action_55 _ = happyReduce_9

action_56 (12) = happyShift action_30
action_56 (16) = happyShift action_61
action_56 _ = happyFail

action_57 (13) = happyShift action_60
action_57 _ = happyFail

action_58 (11) = happyShift action_59
action_58 _ = happyFail

action_59 (7) = happyShift action_4
action_59 (8) = happyShift action_5
action_59 (9) = happyShift action_6
action_59 (14) = happyShift action_7
action_59 (17) = happyShift action_8
action_59 (21) = happyShift action_2
action_59 (23) = happyShift action_9
action_59 (24) = happyShift action_10
action_59 (4) = happyGoto action_64
action_59 _ = happyFail

action_60 (17) = happyShift action_47
action_60 (19) = happyShift action_22
action_60 (20) = happyShift action_23
action_60 (22) = happyShift action_24
action_60 (6) = happyGoto action_63
action_60 _ = happyFail

action_61 (7) = happyShift action_4
action_61 (8) = happyShift action_5
action_61 (9) = happyShift action_6
action_61 (14) = happyShift action_7
action_61 (17) = happyShift action_8
action_61 (21) = happyShift action_2
action_61 (23) = happyShift action_9
action_61 (24) = happyShift action_10
action_61 (4) = happyGoto action_62
action_61 _ = happyFail

action_62 (7) = happyShift action_4
action_62 (8) = happyShift action_5
action_62 (9) = happyShift action_6
action_62 (11) = happyShift action_20
action_62 (14) = happyShift action_7
action_62 (17) = happyShift action_21
action_62 (19) = happyShift action_22
action_62 (20) = happyShift action_23
action_62 (21) = happyShift action_2
action_62 (22) = happyShift action_24
action_62 (23) = happyShift action_9
action_62 (24) = happyShift action_10
action_62 (27) = happyShift action_25
action_62 (4) = happyGoto action_18
action_62 (6) = happyGoto action_19
action_62 _ = happyReduce_5

action_63 (12) = happyShift action_30
action_63 (18) = happyShift action_65
action_63 _ = happyFail

action_64 (7) = happyShift action_4
action_64 (8) = happyShift action_5
action_64 (9) = happyShift action_6
action_64 (11) = happyShift action_20
action_64 (14) = happyShift action_7
action_64 (17) = happyShift action_21
action_64 (19) = happyShift action_22
action_64 (20) = happyShift action_23
action_64 (21) = happyShift action_2
action_64 (22) = happyShift action_24
action_64 (23) = happyShift action_9
action_64 (24) = happyShift action_10
action_64 (27) = happyShift action_25
action_64 (4) = happyGoto action_18
action_64 (6) = happyGoto action_19
action_64 _ = happyReduce_3

action_65 (11) = happyShift action_66
action_65 _ = happyFail

action_66 (7) = happyShift action_4
action_66 (8) = happyShift action_5
action_66 (9) = happyShift action_6
action_66 (14) = happyShift action_7
action_66 (17) = happyShift action_8
action_66 (21) = happyShift action_2
action_66 (23) = happyShift action_9
action_66 (24) = happyShift action_10
action_66 (4) = happyGoto action_67
action_66 _ = happyFail

action_67 (7) = happyShift action_4
action_67 (8) = happyShift action_5
action_67 (9) = happyShift action_6
action_67 (11) = happyShift action_20
action_67 (13) = happyShift action_68
action_67 (14) = happyShift action_7
action_67 (17) = happyShift action_21
action_67 (19) = happyShift action_22
action_67 (20) = happyShift action_23
action_67 (21) = happyShift action_2
action_67 (22) = happyShift action_24
action_67 (23) = happyShift action_9
action_67 (24) = happyShift action_10
action_67 (27) = happyShift action_25
action_67 (4) = happyGoto action_18
action_67 (6) = happyGoto action_19
action_67 _ = happyFail

action_68 (17) = happyShift action_47
action_68 (19) = happyShift action_22
action_68 (20) = happyShift action_23
action_68 (22) = happyShift action_24
action_68 (6) = happyGoto action_69
action_68 _ = happyFail

action_69 (12) = happyShift action_30
action_69 _ = happyReduce_12

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (TokenLowId happy_var_1))
	 =  HappyAbsSyn4
		 (\(tenv, env) -> FVar (fromJust (lookup happy_var_1 env))
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happyReduce 4 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenUpId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (\(tenv, env) -> FBLam (\a -> happy_var_4 ((happy_var_2, a):tenv, env))
	) `HappyStk` happyRest

happyReduce_3 = happyReduce 8 4 happyReduction_3
happyReduction_3 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenLowId happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (\(tenv, env) -> FLam (happy_var_5 tenv) (\x -> happy_var_8 (tenv, (happy_var_3, x):env))
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_2  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (\(tenv, env) -> FApp  (happy_var_1 (tenv, env)) (happy_var_2 (tenv, env))
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 8 4 happyReduction_5
happyReduction_5 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenLowId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (\(tenv, env) -> FApp (FLam (happy_var_6 tenv) (\x -> happy_var_8 (tenv, (happy_var_2, x):env))) (happy_var_4 (tenv, env))
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_2  4 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (\(tenv, env) -> FTApp (happy_var_1 (tenv, env)) (happy_var_2 tenv)
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_3)
	(HappyTerminal (TokenPrimOp happy_var_2))
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (\e -> FPrimOp (happy_var_1 e) happy_var_2 (happy_var_3 e)
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  4 happyReduction_8
happyReduction_8 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn4
		 (\_e -> FLit happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happyReduce 6 4 happyReduction_9
happyReduction_9 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (\e -> Fif0 (happy_var_2 e) (happy_var_4 e) (happy_var_6 e)
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (\(tenv, env) -> FTuple (happy_var_2 (tenv, env))
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  4 happyReduction_11
happyReduction_11 (HappyTerminal (TokenTupleField happy_var_3))
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (\e -> FProj happy_var_3 (happy_var_1 e)
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happyReduce 13 4 happyReduction_12
happyReduction_12 ((HappyAbsSyn6  happy_var_13) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_11) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenLowId happy_var_6)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenLowId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (\(tenv, env) -> 
            FFix (happy_var_8 tenv) (\y -> \x -> happy_var_11 (tenv, (happy_var_6, x):(happy_var_2, y):env)) (happy_var_13 tenv)
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_3  4 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 (\(tenv, env) -> (happy_var_1 (tenv, env):[happy_var_3 (tenv, env)])
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn5
		 (\(tenv, env) -> (happy_var_1 (tenv, env):happy_var_3 (tenv, env))
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  6 happyReduction_16
happyReduction_16 (HappyTerminal (TokenUpId happy_var_1))
	 =  HappyAbsSyn6
		 (\tenv -> FTVar (fromJust (lookup happy_var_1 tenv))
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happyReduce 4 6 happyReduction_17
happyReduction_17 ((HappyAbsSyn6  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenUpId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (\tenv -> FForall (\a -> happy_var_4 ((happy_var_2, a):tenv))
	) `HappyStk` happyRest

happyReduce_18 = happySpecReduce_3  6 happyReduction_18
happyReduction_18 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (\tenv -> FFun (happy_var_1 tenv) (happy_var_3 tenv)
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  6 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn6
		 (\_    -> PFInt
	)

happyReduce_20 = happySpecReduce_3  6 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 29 29 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenTLambda -> cont 7;
	TokenLambda -> cont 8;
	TokenFix -> cont 9;
	TokenComma -> cont 10;
	TokenDot -> cont 11;
	TokenArrow -> cont 12;
	TokenColon -> cont 13;
	TokenLet -> cont 14;
	TokenEQ -> cont 15;
	TokenIn -> cont 16;
	TokenOParen -> cont 17;
	TokenCParen -> cont 18;
	TokenForall -> cont 19;
	TokenIntType -> cont 20;
	TokenLowId happy_dollar_dollar -> cont 21;
	TokenUpId happy_dollar_dollar -> cont 22;
	TokenInt happy_dollar_dollar -> cont 23;
	TokenIf0 -> cont 24;
	TokenThen -> cont 25;
	TokenElse -> cont 26;
	TokenPrimOp happy_dollar_dollar -> cont 27;
	TokenTupleField happy_dollar_dollar -> cont 28;
	_ -> happyError' (tk:tks)
	}

happyError_ 29 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(SystemFToken)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseSF tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [SystemFToken] -> a
parseError tokens = error $ "Parse error before tokens:\n\t" ++ show tokens

readSF :: String -> PFExp t e
readSF = (\parser -> parser emptyEnvs) . parseSF . lexSF
    where emptyEnvs = ([], [])
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4














# 1 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 1 3 4

# 18 "/usr/include/x86_64-linux-gnu/bits/predefs.h" 3 4












# 31 "/usr/include/stdc-predef.h" 2 3 4








# 5 "<command-line>" 2
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 45 "templates/GenericTemplate.hs" #-}








{-# LINE 66 "templates/GenericTemplate.hs" #-}

{-# LINE 76 "templates/GenericTemplate.hs" #-}

{-# LINE 85 "templates/GenericTemplate.hs" #-}

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

{-# LINE 154 "templates/GenericTemplate.hs" #-}

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
     let i = (case x of { HappyErrorToken (i) -> i }) in
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
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 255 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
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

{-# LINE 321 "templates/GenericTemplate.hs" #-}
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
