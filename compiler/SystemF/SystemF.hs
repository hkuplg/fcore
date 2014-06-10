{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module SystemF where

import Language.Java.Syntax as J
import Language.Java.Pretty as JPretty
import Data.List        (intercalate)

-- System F syntax

data PFTyp t = FTVar t 
             | FForall (t -> PFTyp t) 
             | FFun (PFTyp t) (PFTyp t) 
             | PFInt

type PrimLit = Integer -- later maybe Bool | Char

data PFExp t e = 
     FVar e 
   | FBLam (t -> PFExp t e) 
   | FLam (PFTyp t) (e -> PFExp t e) 
   | FApp (PFExp t e) (PFExp t e)
   | FTApp (PFExp t e) (PFTyp t)
   | FPrimOp (PFExp t e) (J.Op) (PFExp t e) -- SystemF extension from: https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf (no int restriction)
   | FLit PrimLit
   | Fif0 (PFExp t e) (PFExp t e) (PFExp t e)
   | FTuple [PFExp t e]
   | FProj Int (PFExp t e)
   -- fixpoints
   | FFix (PFTyp t) (e -> e -> PFExp t e) (PFTyp t)  -- fix y. \(x : t1). e : t2
     
-- forall A. forall B. (A -> B) -> A -> B
doubleTyp :: PFTyp a
doubleTyp = FForall (\a -> FForall (\b -> FFun (FFun (FTVar a) (FTVar b)) (FFun (FTVar a) (FTVar b))))

-- forall A. forall B. forall C. (B -> C) -> (A -> B) -> A -> C
composeTyp :: PFTyp a
composeTyp = FForall (\a -> FForall (\b -> FForall (\c -> FFun (FFun (FTVar b) (FTVar c)) (FFun (FFun (FTVar a) (FTVar b)) (FFun (FTVar a) (FTVar c))))))

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
prettyPFExp (FPrimOp e1 op e2) i = "(" ++ prettyPFExp e1 i ++ " " ++ JPretty.prettyPrint op ++ " " ++ prettyPFExp e2 i ++ ")"
prettyPFExp (FLit n) _           = show n
prettyPFExp (Fif0 e1 e2 e3) i    = "(if " ++ prettyPFExp e1 i ++ " then " ++ prettyPFExp e2 i ++ " else " ++ prettyPFExp e3 i ++ ")"
prettyPFExp (FTuple es) i        = "(" ++ intercalate "," (map (\e -> prettyPFExp e i) es)
prettyPFExp (FProj pos e) i      = "(" ++ prettyPFExp e i ++ "._" ++ show pos ++ ")"
prettyPFExp (FFix t1 f t2) i     = "(fix x" ++ show i ++ ". \\(x" ++ show (i+1) ++ " : " ++ prettyPFTyp t1 i False ++ "). " ++ 
                                    prettyPFExp (f i (i+1)) (i+2) ++ " : " ++ prettyPFTyp t2 i False ++ ")"
   
-- idF = FLam PFInt (\x -> FVar x)


-- fact n = if (n == 0) then 1 else n * fact (n-1)

-- fact = \n -> if (n == 0) then 1 else n * fact (n-1)

-- fact = fix fact . \(n : Int) . if (n == 0) then 1 else n * fact (n-1) : Int
