{-# OPTIONS_GHC -w #-}
module HMParser where

import HMSyntax
import HMTokens
import HMLexer

-- parser produced by Happy Version 1.19.0

data HappyAbsSyn t4 t5 t6 t7
	= HappyTerminal (HMToken)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7

action_0 (8) = happyShift action_5
action_0 (13) = happyShift action_6
action_0 (15) = happyShift action_2
action_0 (17) = happyShift action_7
action_0 (31) = happyShift action_8
action_0 (34) = happyShift action_9
action_0 (35) = happyShift action_10
action_0 (4) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 _ = happyFail

action_1 (15) = happyShift action_2
action_1 _ = happyFail

action_2 (8) = happyShift action_5
action_2 (13) = happyShift action_6
action_2 (15) = happyShift action_2
action_2 (17) = happyShift action_7
action_2 (31) = happyShift action_8
action_2 (34) = happyShift action_9
action_2 (35) = happyShift action_10
action_2 (4) = happyGoto action_30
action_2 (7) = happyGoto action_4
action_2 _ = happyFail

action_3 (8) = happyShift action_5
action_3 (13) = happyShift action_6
action_3 (15) = happyShift action_2
action_3 (17) = happyShift action_7
action_3 (18) = happyShift action_17
action_3 (19) = happyShift action_18
action_3 (20) = happyShift action_19
action_3 (21) = happyShift action_20
action_3 (22) = happyShift action_21
action_3 (23) = happyShift action_22
action_3 (24) = happyShift action_23
action_3 (25) = happyShift action_24
action_3 (26) = happyShift action_25
action_3 (27) = happyShift action_26
action_3 (28) = happyShift action_27
action_3 (29) = happyShift action_28
action_3 (30) = happyShift action_29
action_3 (31) = happyShift action_8
action_3 (34) = happyShift action_9
action_3 (35) = happyShift action_10
action_3 (36) = happyAccept
action_3 (4) = happyGoto action_16
action_3 (7) = happyGoto action_4
action_3 _ = happyFail

action_4 _ = happyReduce_2

action_5 (9) = happyShift action_15
action_5 (35) = happyShift action_10
action_5 (7) = happyGoto action_14
action_5 _ = happyFail

action_6 (35) = happyShift action_10
action_6 (7) = happyGoto action_13
action_6 _ = happyFail

action_7 (8) = happyShift action_5
action_7 (13) = happyShift action_6
action_7 (15) = happyShift action_2
action_7 (17) = happyShift action_7
action_7 (31) = happyShift action_8
action_7 (34) = happyShift action_9
action_7 (35) = happyShift action_10
action_7 (4) = happyGoto action_12
action_7 (7) = happyGoto action_4
action_7 _ = happyFail

action_8 (8) = happyShift action_5
action_8 (13) = happyShift action_6
action_8 (15) = happyShift action_2
action_8 (17) = happyShift action_7
action_8 (31) = happyShift action_8
action_8 (34) = happyShift action_9
action_8 (35) = happyShift action_10
action_8 (4) = happyGoto action_11
action_8 (7) = happyGoto action_4
action_8 _ = happyFail

action_9 _ = happyReduce_22

action_10 _ = happyReduce_26

action_11 (8) = happyShift action_5
action_11 (13) = happyShift action_6
action_11 (15) = happyShift action_2
action_11 (17) = happyShift action_7
action_11 (18) = happyShift action_17
action_11 (19) = happyShift action_18
action_11 (20) = happyShift action_19
action_11 (21) = happyShift action_20
action_11 (22) = happyShift action_21
action_11 (23) = happyShift action_22
action_11 (24) = happyShift action_23
action_11 (25) = happyShift action_24
action_11 (26) = happyShift action_25
action_11 (27) = happyShift action_26
action_11 (28) = happyShift action_27
action_11 (29) = happyShift action_28
action_11 (30) = happyShift action_29
action_11 (31) = happyShift action_8
action_11 (32) = happyShift action_50
action_11 (34) = happyShift action_9
action_11 (35) = happyShift action_10
action_11 (4) = happyGoto action_16
action_11 (7) = happyGoto action_4
action_11 _ = happyFail

action_12 (8) = happyShift action_5
action_12 (13) = happyShift action_6
action_12 (15) = happyShift action_2
action_12 (17) = happyShift action_7
action_12 (18) = happyShift action_17
action_12 (19) = happyShift action_18
action_12 (20) = happyShift action_19
action_12 (21) = happyShift action_20
action_12 (22) = happyShift action_21
action_12 (23) = happyShift action_22
action_12 (24) = happyShift action_23
action_12 (25) = happyShift action_24
action_12 (26) = happyShift action_25
action_12 (27) = happyShift action_26
action_12 (28) = happyShift action_27
action_12 (29) = happyShift action_28
action_12 (30) = happyShift action_29
action_12 (31) = happyShift action_8
action_12 (34) = happyShift action_9
action_12 (35) = happyShift action_10
action_12 (4) = happyGoto action_16
action_12 (7) = happyGoto action_4
action_12 _ = happyReduce_7

action_13 (14) = happyShift action_49
action_13 _ = happyFail

action_14 (11) = happyShift action_48
action_14 _ = happyFail

action_15 (35) = happyShift action_10
action_15 (5) = happyGoto action_45
action_15 (6) = happyGoto action_46
action_15 (7) = happyGoto action_47
action_15 _ = happyFail

action_16 (8) = happyShift action_5
action_16 (13) = happyShift action_6
action_16 (15) = happyShift action_2
action_16 (17) = happyShift action_7
action_16 (18) = happyShift action_17
action_16 (19) = happyShift action_18
action_16 (20) = happyShift action_19
action_16 (21) = happyShift action_20
action_16 (22) = happyShift action_21
action_16 (23) = happyShift action_22
action_16 (24) = happyShift action_23
action_16 (25) = happyShift action_24
action_16 (26) = happyShift action_25
action_16 (27) = happyShift action_26
action_16 (28) = happyShift action_27
action_16 (29) = happyShift action_28
action_16 (30) = happyShift action_29
action_16 (31) = happyShift action_8
action_16 (34) = happyShift action_9
action_16 (35) = happyShift action_10
action_16 (4) = happyGoto action_16
action_16 (7) = happyGoto action_4
action_16 _ = happyReduce_3

action_17 (8) = happyShift action_5
action_17 (13) = happyShift action_6
action_17 (15) = happyShift action_2
action_17 (17) = happyShift action_7
action_17 (31) = happyShift action_8
action_17 (34) = happyShift action_9
action_17 (35) = happyShift action_10
action_17 (4) = happyGoto action_44
action_17 (7) = happyGoto action_4
action_17 _ = happyFail

action_18 (8) = happyShift action_5
action_18 (13) = happyShift action_6
action_18 (15) = happyShift action_2
action_18 (17) = happyShift action_7
action_18 (31) = happyShift action_8
action_18 (34) = happyShift action_9
action_18 (35) = happyShift action_10
action_18 (4) = happyGoto action_43
action_18 (7) = happyGoto action_4
action_18 _ = happyFail

action_19 (8) = happyShift action_5
action_19 (13) = happyShift action_6
action_19 (15) = happyShift action_2
action_19 (17) = happyShift action_7
action_19 (31) = happyShift action_8
action_19 (34) = happyShift action_9
action_19 (35) = happyShift action_10
action_19 (4) = happyGoto action_42
action_19 (7) = happyGoto action_4
action_19 _ = happyFail

action_20 (8) = happyShift action_5
action_20 (13) = happyShift action_6
action_20 (15) = happyShift action_2
action_20 (17) = happyShift action_7
action_20 (31) = happyShift action_8
action_20 (34) = happyShift action_9
action_20 (35) = happyShift action_10
action_20 (4) = happyGoto action_41
action_20 (7) = happyGoto action_4
action_20 _ = happyFail

action_21 (8) = happyShift action_5
action_21 (13) = happyShift action_6
action_21 (15) = happyShift action_2
action_21 (17) = happyShift action_7
action_21 (31) = happyShift action_8
action_21 (34) = happyShift action_9
action_21 (35) = happyShift action_10
action_21 (4) = happyGoto action_40
action_21 (7) = happyGoto action_4
action_21 _ = happyFail

action_22 (8) = happyShift action_5
action_22 (13) = happyShift action_6
action_22 (15) = happyShift action_2
action_22 (17) = happyShift action_7
action_22 (31) = happyShift action_8
action_22 (34) = happyShift action_9
action_22 (35) = happyShift action_10
action_22 (4) = happyGoto action_39
action_22 (7) = happyGoto action_4
action_22 _ = happyFail

action_23 (8) = happyShift action_5
action_23 (13) = happyShift action_6
action_23 (15) = happyShift action_2
action_23 (17) = happyShift action_7
action_23 (31) = happyShift action_8
action_23 (34) = happyShift action_9
action_23 (35) = happyShift action_10
action_23 (4) = happyGoto action_38
action_23 (7) = happyGoto action_4
action_23 _ = happyFail

action_24 (8) = happyShift action_5
action_24 (13) = happyShift action_6
action_24 (15) = happyShift action_2
action_24 (17) = happyShift action_7
action_24 (31) = happyShift action_8
action_24 (34) = happyShift action_9
action_24 (35) = happyShift action_10
action_24 (4) = happyGoto action_37
action_24 (7) = happyGoto action_4
action_24 _ = happyFail

action_25 (8) = happyShift action_5
action_25 (13) = happyShift action_6
action_25 (15) = happyShift action_2
action_25 (17) = happyShift action_7
action_25 (31) = happyShift action_8
action_25 (34) = happyShift action_9
action_25 (35) = happyShift action_10
action_25 (4) = happyGoto action_36
action_25 (7) = happyGoto action_4
action_25 _ = happyFail

action_26 (8) = happyShift action_5
action_26 (13) = happyShift action_6
action_26 (15) = happyShift action_2
action_26 (17) = happyShift action_7
action_26 (31) = happyShift action_8
action_26 (34) = happyShift action_9
action_26 (35) = happyShift action_10
action_26 (4) = happyGoto action_35
action_26 (7) = happyGoto action_4
action_26 _ = happyFail

action_27 (8) = happyShift action_5
action_27 (13) = happyShift action_6
action_27 (15) = happyShift action_2
action_27 (17) = happyShift action_7
action_27 (31) = happyShift action_8
action_27 (34) = happyShift action_9
action_27 (35) = happyShift action_10
action_27 (4) = happyGoto action_34
action_27 (7) = happyGoto action_4
action_27 _ = happyFail

action_28 (8) = happyShift action_5
action_28 (13) = happyShift action_6
action_28 (15) = happyShift action_2
action_28 (17) = happyShift action_7
action_28 (31) = happyShift action_8
action_28 (34) = happyShift action_9
action_28 (35) = happyShift action_10
action_28 (4) = happyGoto action_33
action_28 (7) = happyGoto action_4
action_28 _ = happyFail

action_29 (8) = happyShift action_5
action_29 (13) = happyShift action_6
action_29 (15) = happyShift action_2
action_29 (17) = happyShift action_7
action_29 (31) = happyShift action_8
action_29 (34) = happyShift action_9
action_29 (35) = happyShift action_10
action_29 (4) = happyGoto action_32
action_29 (7) = happyGoto action_4
action_29 _ = happyFail

action_30 (8) = happyShift action_5
action_30 (13) = happyShift action_6
action_30 (15) = happyShift action_2
action_30 (16) = happyShift action_31
action_30 (17) = happyShift action_7
action_30 (18) = happyShift action_17
action_30 (19) = happyShift action_18
action_30 (20) = happyShift action_19
action_30 (21) = happyShift action_20
action_30 (22) = happyShift action_21
action_30 (23) = happyShift action_22
action_30 (24) = happyShift action_23
action_30 (25) = happyShift action_24
action_30 (26) = happyShift action_25
action_30 (27) = happyShift action_26
action_30 (28) = happyShift action_27
action_30 (29) = happyShift action_28
action_30 (30) = happyShift action_29
action_30 (31) = happyShift action_8
action_30 (34) = happyShift action_9
action_30 (35) = happyShift action_10
action_30 (4) = happyGoto action_16
action_30 (7) = happyGoto action_4
action_30 _ = happyFail

action_31 _ = happyReduce_1

action_32 (8) = happyShift action_5
action_32 (13) = happyShift action_6
action_32 (15) = happyShift action_2
action_32 (17) = happyShift action_7
action_32 (18) = happyShift action_17
action_32 (19) = happyShift action_18
action_32 (20) = happyShift action_19
action_32 (21) = happyShift action_20
action_32 (22) = happyShift action_21
action_32 (23) = happyShift action_22
action_32 (24) = happyShift action_23
action_32 (25) = happyShift action_24
action_32 (26) = happyShift action_25
action_32 (27) = happyShift action_26
action_32 (28) = happyShift action_27
action_32 (29) = happyShift action_28
action_32 (31) = happyShift action_8
action_32 (34) = happyShift action_9
action_32 (35) = happyShift action_10
action_32 (4) = happyGoto action_16
action_32 (7) = happyGoto action_4
action_32 _ = happyReduce_20

action_33 (8) = happyShift action_5
action_33 (13) = happyShift action_6
action_33 (15) = happyShift action_2
action_33 (17) = happyShift action_7
action_33 (18) = happyShift action_17
action_33 (19) = happyShift action_18
action_33 (20) = happyShift action_19
action_33 (21) = happyShift action_20
action_33 (22) = happyShift action_21
action_33 (23) = happyShift action_22
action_33 (24) = happyShift action_23
action_33 (25) = happyShift action_24
action_33 (26) = happyShift action_25
action_33 (27) = happyShift action_26
action_33 (28) = happyShift action_27
action_33 (31) = happyShift action_8
action_33 (34) = happyShift action_9
action_33 (35) = happyShift action_10
action_33 (4) = happyGoto action_16
action_33 (7) = happyGoto action_4
action_33 _ = happyReduce_19

action_34 (8) = happyShift action_5
action_34 (13) = happyShift action_6
action_34 (15) = happyShift action_2
action_34 (17) = happyShift action_7
action_34 (18) = happyShift action_17
action_34 (19) = happyShift action_18
action_34 (20) = happyShift action_19
action_34 (21) = happyShift action_20
action_34 (22) = happyShift action_21
action_34 (24) = happyShift action_23
action_34 (31) = happyShift action_8
action_34 (34) = happyShift action_9
action_34 (35) = happyShift action_10
action_34 (4) = happyGoto action_16
action_34 (7) = happyGoto action_4
action_34 _ = happyReduce_18

action_35 (8) = happyShift action_5
action_35 (13) = happyShift action_6
action_35 (15) = happyShift action_2
action_35 (17) = happyShift action_7
action_35 (18) = happyShift action_17
action_35 (19) = happyShift action_18
action_35 (20) = happyShift action_19
action_35 (21) = happyShift action_20
action_35 (22) = happyShift action_21
action_35 (24) = happyShift action_23
action_35 (31) = happyShift action_8
action_35 (34) = happyShift action_9
action_35 (35) = happyShift action_10
action_35 (4) = happyGoto action_16
action_35 (7) = happyGoto action_4
action_35 _ = happyReduce_17

action_36 (8) = happyShift action_5
action_36 (13) = happyShift action_6
action_36 (15) = happyShift action_2
action_36 (17) = happyShift action_7
action_36 (18) = happyShift action_17
action_36 (19) = happyShift action_18
action_36 (20) = happyShift action_19
action_36 (21) = happyShift action_20
action_36 (22) = happyShift action_21
action_36 (24) = happyShift action_23
action_36 (31) = happyShift action_8
action_36 (34) = happyShift action_9
action_36 (35) = happyShift action_10
action_36 (4) = happyGoto action_16
action_36 (7) = happyGoto action_4
action_36 _ = happyReduce_16

action_37 (8) = happyShift action_5
action_37 (13) = happyShift action_6
action_37 (15) = happyShift action_2
action_37 (17) = happyShift action_7
action_37 (18) = happyShift action_17
action_37 (19) = happyShift action_18
action_37 (20) = happyShift action_19
action_37 (21) = happyShift action_20
action_37 (22) = happyShift action_21
action_37 (24) = happyShift action_23
action_37 (31) = happyShift action_8
action_37 (34) = happyShift action_9
action_37 (35) = happyShift action_10
action_37 (4) = happyGoto action_16
action_37 (7) = happyGoto action_4
action_37 _ = happyReduce_15

action_38 (8) = happyShift action_5
action_38 (13) = happyShift action_6
action_38 (15) = happyShift action_2
action_38 (17) = happyShift action_7
action_38 (18) = happyShift action_17
action_38 (19) = happyShift action_18
action_38 (20) = happyShift action_19
action_38 (21) = happyShift action_20
action_38 (22) = happyShift action_21
action_38 (23) = happyShift action_22
action_38 (24) = happyShift action_23
action_38 (25) = happyShift action_24
action_38 (26) = happyShift action_25
action_38 (27) = happyShift action_26
action_38 (28) = happyShift action_27
action_38 (29) = happyShift action_28
action_38 (30) = happyShift action_29
action_38 (31) = happyShift action_8
action_38 (34) = happyShift action_9
action_38 (35) = happyShift action_10
action_38 (4) = happyGoto action_16
action_38 (7) = happyGoto action_4
action_38 _ = happyReduce_14

action_39 (8) = happyShift action_5
action_39 (13) = happyShift action_6
action_39 (15) = happyShift action_2
action_39 (17) = happyShift action_7
action_39 (18) = happyShift action_17
action_39 (19) = happyShift action_18
action_39 (20) = happyShift action_19
action_39 (21) = happyShift action_20
action_39 (22) = happyShift action_21
action_39 (24) = happyShift action_23
action_39 (31) = happyShift action_8
action_39 (34) = happyShift action_9
action_39 (35) = happyShift action_10
action_39 (4) = happyGoto action_16
action_39 (7) = happyGoto action_4
action_39 _ = happyReduce_13

action_40 (8) = happyShift action_5
action_40 (13) = happyShift action_6
action_40 (15) = happyShift action_2
action_40 (17) = happyShift action_7
action_40 (24) = happyShift action_23
action_40 (31) = happyShift action_8
action_40 (34) = happyShift action_9
action_40 (35) = happyShift action_10
action_40 (4) = happyGoto action_16
action_40 (7) = happyGoto action_4
action_40 _ = happyReduce_12

action_41 (8) = happyShift action_5
action_41 (13) = happyShift action_6
action_41 (15) = happyShift action_2
action_41 (17) = happyShift action_7
action_41 (24) = happyShift action_23
action_41 (31) = happyShift action_8
action_41 (34) = happyShift action_9
action_41 (35) = happyShift action_10
action_41 (4) = happyGoto action_16
action_41 (7) = happyGoto action_4
action_41 _ = happyReduce_11

action_42 (8) = happyShift action_5
action_42 (13) = happyShift action_6
action_42 (15) = happyShift action_2
action_42 (17) = happyShift action_7
action_42 (24) = happyShift action_23
action_42 (31) = happyShift action_8
action_42 (34) = happyShift action_9
action_42 (35) = happyShift action_10
action_42 (4) = happyGoto action_16
action_42 (7) = happyGoto action_4
action_42 _ = happyReduce_10

action_43 (8) = happyShift action_5
action_43 (13) = happyShift action_6
action_43 (15) = happyShift action_2
action_43 (17) = happyShift action_7
action_43 (20) = happyShift action_19
action_43 (21) = happyShift action_20
action_43 (22) = happyShift action_21
action_43 (24) = happyShift action_23
action_43 (31) = happyShift action_8
action_43 (34) = happyShift action_9
action_43 (35) = happyShift action_10
action_43 (4) = happyGoto action_16
action_43 (7) = happyGoto action_4
action_43 _ = happyReduce_9

action_44 (8) = happyShift action_5
action_44 (13) = happyShift action_6
action_44 (15) = happyShift action_2
action_44 (17) = happyShift action_7
action_44 (20) = happyShift action_19
action_44 (21) = happyShift action_20
action_44 (22) = happyShift action_21
action_44 (24) = happyShift action_23
action_44 (31) = happyShift action_8
action_44 (34) = happyShift action_9
action_44 (35) = happyShift action_10
action_44 (4) = happyGoto action_16
action_44 (7) = happyGoto action_4
action_44 _ = happyReduce_8

action_45 _ = happyReduce_24

action_46 (10) = happyShift action_55
action_46 (12) = happyShift action_56
action_46 _ = happyFail

action_47 (11) = happyShift action_54
action_47 _ = happyFail

action_48 (8) = happyShift action_5
action_48 (13) = happyShift action_6
action_48 (15) = happyShift action_2
action_48 (17) = happyShift action_7
action_48 (31) = happyShift action_8
action_48 (34) = happyShift action_9
action_48 (35) = happyShift action_10
action_48 (4) = happyGoto action_53
action_48 (7) = happyGoto action_4
action_48 _ = happyFail

action_49 (8) = happyShift action_5
action_49 (13) = happyShift action_6
action_49 (15) = happyShift action_2
action_49 (17) = happyShift action_7
action_49 (31) = happyShift action_8
action_49 (34) = happyShift action_9
action_49 (35) = happyShift action_10
action_49 (4) = happyGoto action_52
action_49 (7) = happyGoto action_4
action_49 _ = happyFail

action_50 (8) = happyShift action_5
action_50 (13) = happyShift action_6
action_50 (15) = happyShift action_2
action_50 (17) = happyShift action_7
action_50 (31) = happyShift action_8
action_50 (34) = happyShift action_9
action_50 (35) = happyShift action_10
action_50 (4) = happyGoto action_51
action_50 (7) = happyGoto action_4
action_50 _ = happyFail

action_51 (8) = happyShift action_5
action_51 (13) = happyShift action_6
action_51 (15) = happyShift action_2
action_51 (17) = happyShift action_7
action_51 (18) = happyShift action_17
action_51 (19) = happyShift action_18
action_51 (20) = happyShift action_19
action_51 (21) = happyShift action_20
action_51 (22) = happyShift action_21
action_51 (23) = happyShift action_22
action_51 (24) = happyShift action_23
action_51 (25) = happyShift action_24
action_51 (26) = happyShift action_25
action_51 (27) = happyShift action_26
action_51 (28) = happyShift action_27
action_51 (29) = happyShift action_28
action_51 (30) = happyShift action_29
action_51 (31) = happyShift action_8
action_51 (33) = happyShift action_61
action_51 (34) = happyShift action_9
action_51 (35) = happyShift action_10
action_51 (4) = happyGoto action_16
action_51 (7) = happyGoto action_4
action_51 _ = happyFail

action_52 (8) = happyShift action_5
action_52 (13) = happyShift action_6
action_52 (15) = happyShift action_2
action_52 (17) = happyShift action_7
action_52 (18) = happyShift action_17
action_52 (19) = happyShift action_18
action_52 (20) = happyShift action_19
action_52 (21) = happyShift action_20
action_52 (22) = happyShift action_21
action_52 (23) = happyShift action_22
action_52 (24) = happyShift action_23
action_52 (25) = happyShift action_24
action_52 (26) = happyShift action_25
action_52 (27) = happyShift action_26
action_52 (28) = happyShift action_27
action_52 (29) = happyShift action_28
action_52 (30) = happyShift action_29
action_52 (31) = happyShift action_8
action_52 (34) = happyShift action_9
action_52 (35) = happyShift action_10
action_52 (4) = happyGoto action_16
action_52 (7) = happyGoto action_4
action_52 _ = happyReduce_4

action_53 (8) = happyShift action_5
action_53 (12) = happyShift action_60
action_53 (13) = happyShift action_6
action_53 (15) = happyShift action_2
action_53 (17) = happyShift action_7
action_53 (18) = happyShift action_17
action_53 (19) = happyShift action_18
action_53 (20) = happyShift action_19
action_53 (21) = happyShift action_20
action_53 (22) = happyShift action_21
action_53 (23) = happyShift action_22
action_53 (24) = happyShift action_23
action_53 (25) = happyShift action_24
action_53 (26) = happyShift action_25
action_53 (27) = happyShift action_26
action_53 (28) = happyShift action_27
action_53 (29) = happyShift action_28
action_53 (30) = happyShift action_29
action_53 (31) = happyShift action_8
action_53 (34) = happyShift action_9
action_53 (35) = happyShift action_10
action_53 (4) = happyGoto action_16
action_53 (7) = happyGoto action_4
action_53 _ = happyFail

action_54 (8) = happyShift action_5
action_54 (13) = happyShift action_6
action_54 (15) = happyShift action_2
action_54 (17) = happyShift action_7
action_54 (31) = happyShift action_8
action_54 (34) = happyShift action_9
action_54 (35) = happyShift action_10
action_54 (4) = happyGoto action_59
action_54 (7) = happyGoto action_4
action_54 _ = happyFail

action_55 (35) = happyShift action_10
action_55 (5) = happyGoto action_58
action_55 (7) = happyGoto action_47
action_55 _ = happyFail

action_56 (8) = happyShift action_5
action_56 (13) = happyShift action_6
action_56 (15) = happyShift action_2
action_56 (17) = happyShift action_7
action_56 (31) = happyShift action_8
action_56 (34) = happyShift action_9
action_56 (35) = happyShift action_10
action_56 (4) = happyGoto action_57
action_56 (7) = happyGoto action_4
action_56 _ = happyFail

action_57 (8) = happyShift action_5
action_57 (13) = happyShift action_6
action_57 (15) = happyShift action_2
action_57 (17) = happyShift action_7
action_57 (18) = happyShift action_17
action_57 (19) = happyShift action_18
action_57 (20) = happyShift action_19
action_57 (21) = happyShift action_20
action_57 (22) = happyShift action_21
action_57 (23) = happyShift action_22
action_57 (24) = happyShift action_23
action_57 (25) = happyShift action_24
action_57 (26) = happyShift action_25
action_57 (27) = happyShift action_26
action_57 (28) = happyShift action_27
action_57 (29) = happyShift action_28
action_57 (30) = happyShift action_29
action_57 (31) = happyShift action_8
action_57 (34) = happyShift action_9
action_57 (35) = happyShift action_10
action_57 (4) = happyGoto action_16
action_57 (7) = happyGoto action_4
action_57 _ = happyReduce_6

action_58 _ = happyReduce_25

action_59 (8) = happyShift action_5
action_59 (13) = happyShift action_6
action_59 (15) = happyShift action_2
action_59 (17) = happyShift action_7
action_59 (18) = happyShift action_17
action_59 (19) = happyShift action_18
action_59 (20) = happyShift action_19
action_59 (21) = happyShift action_20
action_59 (22) = happyShift action_21
action_59 (23) = happyShift action_22
action_59 (24) = happyShift action_23
action_59 (25) = happyShift action_24
action_59 (26) = happyShift action_25
action_59 (27) = happyShift action_26
action_59 (28) = happyShift action_27
action_59 (29) = happyShift action_28
action_59 (30) = happyShift action_29
action_59 (31) = happyShift action_8
action_59 (34) = happyShift action_9
action_59 (35) = happyShift action_10
action_59 (4) = happyGoto action_16
action_59 (7) = happyGoto action_4
action_59 _ = happyReduce_23

action_60 (8) = happyShift action_5
action_60 (13) = happyShift action_6
action_60 (15) = happyShift action_2
action_60 (17) = happyShift action_7
action_60 (31) = happyShift action_8
action_60 (34) = happyShift action_9
action_60 (35) = happyShift action_10
action_60 (4) = happyGoto action_63
action_60 (7) = happyGoto action_4
action_60 _ = happyFail

action_61 (8) = happyShift action_5
action_61 (13) = happyShift action_6
action_61 (15) = happyShift action_2
action_61 (17) = happyShift action_7
action_61 (31) = happyShift action_8
action_61 (34) = happyShift action_9
action_61 (35) = happyShift action_10
action_61 (4) = happyGoto action_62
action_61 (7) = happyGoto action_4
action_61 _ = happyFail

action_62 (8) = happyShift action_5
action_62 (13) = happyShift action_6
action_62 (15) = happyShift action_2
action_62 (17) = happyShift action_7
action_62 (18) = happyShift action_17
action_62 (19) = happyShift action_18
action_62 (20) = happyShift action_19
action_62 (21) = happyShift action_20
action_62 (22) = happyShift action_21
action_62 (23) = happyShift action_22
action_62 (24) = happyShift action_23
action_62 (25) = happyShift action_24
action_62 (26) = happyShift action_25
action_62 (27) = happyShift action_26
action_62 (28) = happyShift action_27
action_62 (29) = happyShift action_28
action_62 (30) = happyShift action_29
action_62 (31) = happyShift action_8
action_62 (34) = happyShift action_9
action_62 (35) = happyShift action_10
action_62 (4) = happyGoto action_16
action_62 (7) = happyGoto action_4
action_62 _ = happyReduce_21

action_63 (8) = happyShift action_5
action_63 (13) = happyShift action_6
action_63 (15) = happyShift action_2
action_63 (17) = happyShift action_7
action_63 (18) = happyShift action_17
action_63 (19) = happyShift action_18
action_63 (20) = happyShift action_19
action_63 (21) = happyShift action_20
action_63 (22) = happyShift action_21
action_63 (23) = happyShift action_22
action_63 (24) = happyShift action_23
action_63 (25) = happyShift action_24
action_63 (26) = happyShift action_25
action_63 (27) = happyShift action_26
action_63 (28) = happyShift action_27
action_63 (29) = happyShift action_28
action_63 (30) = happyShift action_29
action_63 (31) = happyShift action_8
action_63 (34) = happyShift action_9
action_63 (35) = happyShift action_10
action_63 (4) = happyGoto action_16
action_63 (7) = happyGoto action_4
action_63 _ = happyReduce_5

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (EVar happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (EApp happy_var_1 happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happyReduce 4 4 happyReduction_4
happyReduction_4 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (ELam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 6 4 happyReduction_5
happyReduction_5 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (ELet    happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 5 4 happyReduction_6
happyReduction_6 ((HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (ELetRec happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_2  4 happyReduction_7
happyReduction_7 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (EUn Not happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  4 happyReduction_8
happyReduction_8 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (EBin Add happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  4 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (EBin Sub happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  4 happyReduction_10
happyReduction_10 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (EBin Mul happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  4 happyReduction_11
happyReduction_11 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (EBin Div happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  4 happyReduction_12
happyReduction_12 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (EBin Mod happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  4 happyReduction_13
happyReduction_13 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (EBin Eq happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  4 happyReduction_14
happyReduction_14 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (EBin Ne happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  4 happyReduction_15
happyReduction_15 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (EBin Lt happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  4 happyReduction_16
happyReduction_16 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (EBin Gt happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  4 happyReduction_17
happyReduction_17 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (EBin Le happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  4 happyReduction_18
happyReduction_18 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (EBin Ge happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  4 happyReduction_19
happyReduction_19 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (EBin And happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  4 happyReduction_20
happyReduction_20 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (EBin Or  happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happyReduce 6 4 happyReduction_21
happyReduction_21 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (EIf0 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_22 = happySpecReduce_1  4 happyReduction_22
happyReduction_22 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn4
		 (ELit happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  5 happyReduction_23
happyReduction_23 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 ((happy_var_1, happy_var_3)
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  6 happyReduction_24
happyReduction_24 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  6 happyReduction_25
happyReduction_25 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1 ++ [happy_var_3]
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  7 happyReduction_26
happyReduction_26 (HappyTerminal (TokenLowId happy_var_1))
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 36 36 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLet -> cont 8;
	TokenRec -> cont 9;
	TokenAnd -> cont 10;
	TokenEQ -> cont 11;
	TokenIn -> cont 12;
	TokenLambda -> cont 13;
	TokenArrow -> cont 14;
	TokenOParen -> cont 15;
	TokenCParen -> cont 16;
	TokenUn Not -> cont 17;
	TokenBin Add -> cont 18;
	TokenBin Sub -> cont 19;
	TokenBin Mul -> cont 20;
	TokenBin Div -> cont 21;
	TokenBin Mod -> cont 22;
	TokenBin Eq -> cont 23;
	TokenBin Ne -> cont 24;
	TokenBin Lt -> cont 25;
	TokenBin Gt -> cont 26;
	TokenBin Le -> cont 27;
	TokenBin Ge -> cont 28;
	TokenBin And -> cont 29;
	TokenBin Or -> cont 30;
	TokenIf0 -> cont 31;
	TokenThen -> cont 32;
	TokenElse -> cont 33;
	TokenInt happy_dollar_dollar -> cont 34;
	TokenLowId happy_dollar_dollar -> cont 35;
	_ -> happyError' (tk:tks)
	}

happyError_ 36 tk tks = happyError' tks
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
happyError' :: () => [(HMToken)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseHM tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [HMToken] -> a
parseError tokens = error $ "Parse error before tokens:\n\t" ++ show tokens

readHM :: String -> Exp
readHM = parseHM . lexHM
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
