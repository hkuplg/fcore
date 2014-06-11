{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module SystemF.Pretty where

import SystemF.Syntax

import Data.List        (intercalate)
import Language.Java.Pretty as JP

prettyPFTyp :: PFTyp Int -> Int -> Bool -> String
prettyPFTyp (FTVar i) _ _    = "A" ++ show i
prettyPFTyp (FForall f) i p  = "forall A" ++ show i ++ ". " ++ prettyPFTyp (f i) (i+1) False
prettyPFTyp (FFun t1 t2) i p = if p then "(" ++ content ++ ")" else content
                                    where content = prettyPFTyp t1 i True ++ " -> " ++ prettyPFTyp t2 i False 
prettyPFTyp PFInt _ _        = "Int"

prettyPFExp :: PFExp Int Int -> Int -> String     
prettyPFExp (FVar n)   _         = "x" ++ show n
prettyPFExp (FBLam f)  i         = "(" ++ "/\\A" ++ show i ++ ". " ++ prettyPFExp (f i) (i+1) ++ ")" 
prettyPFExp (FLam t f) i         = "(" ++ "\\(x" ++ show i ++ " : " ++ prettyPFTyp t (i+1) False ++ "). " ++ prettyPFExp (f i) (i+1) ++ ")" 
prettyPFExp (FApp e1 e2) i       = "(" ++ prettyPFExp e1 i ++ ") (" ++ prettyPFExp e2 i ++ ")"
prettyPFExp (FTApp e t) i        = "(" ++ prettyPFExp e i ++ ") (" ++ prettyPFTyp t i False ++ ")"
prettyPFExp (FPrimOp e1 op e2) i = "(" ++ prettyPFExp e1 i ++ " " ++ JP.prettyPrint op ++ " " ++ prettyPFExp e2 i ++ ")"
prettyPFExp (FLit n) _           = show n
prettyPFExp (Fif0 e1 e2 e3) i    = "(if " ++ prettyPFExp e1 i ++ " then " ++ prettyPFExp e2 i ++ " else " ++ prettyPFExp e3 i ++ ")"
prettyPFExp (FTuple es) i        = "(" ++ intercalate "," (map (\e -> prettyPFExp e i) es)
prettyPFExp (FProj pos e) i      = "(" ++ prettyPFExp e i ++ "._" ++ show pos ++ ")"
prettyPFExp (FFix t1 f t2) i     = "(fix x" ++ show i ++ ". \\(x" ++ show (i+1) ++ " : " ++ prettyPFTyp t1 i False ++ "). " ++ 
                                    prettyPFExp (f i (i+1)) (i+2) ++ " : " ++ prettyPFTyp t2 i False ++ ")"
