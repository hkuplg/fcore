{-# OPTIONS -XRankNTypes -XFlexibleInstances #-}

module ClosureF where

import Language.Java.Syntax as J
import SystemF

-- Closure F syntax

data Scope b t e = Body b | Kind (t -> Scope b t e) | Typ (PCTyp t) (e -> Scope b t e) 

type TScope t = Scope (PCTyp t) t ()

type EScope t e = Scope (PCExp t e) t e

data PCTyp t = CTVar t | CForall (TScope t) | CInt | CTupleType [PCTyp t]

data PCExp t e = 
     CVar e 
   | CFVar Int
   | CLam (EScope t e)  
   | CApp (PCExp t e) (PCExp t e)
   | CTApp (PCExp t e) (PCTyp t)
   | CFPrimOp (PCExp t e) (J.Op) (PCExp t e)
   | CFLit PrimLit
   | CFif0 (PCExp t e) (PCExp t e) (PCExp t e)
   | CFTuple [PCExp t e]
   | CFProj Int (PCExp t e)
   -- fixpoints
   | CFix (PCTyp t) (EScope t e) 

-- System F to Closure F

ftyp2scope :: PFTyp t -> TScope t
ftyp2scope (FForall f)   = Kind (\a -> ftyp2scope (f a))
ftyp2scope (FFun t1 t2)  = Typ (ftyp2ctyp t1) (\x -> ftyp2scope t2)
ftyp2scope t             = Body (ftyp2ctyp t)
-- ftyp2scope PFInt         = Body CInt
-- ftyp2scope (FTVar x)     = Body (CTVar x)

ftyp2ctyp :: PFTyp t -> PCTyp t
ftyp2ctyp (FTVar x) = CTVar x
ftyp2ctyp (PFInt)     = CInt
ftyp2ctyp t         = CForall (ftyp2scope t)

fexp2cexp :: PFExp t e -> PCExp t e
fexp2cexp (FVar x)      = CVar x
fexp2cexp (FApp e1 e2)  = CApp (fexp2cexp e1) (fexp2cexp e2)
fexp2cexp (FTApp e t)   = CTApp (fexp2cexp e) (ftyp2ctyp t)
fexp2cexp (FPrimOp e1 op e2) = CFPrimOp (fexp2cexp e1) op (fexp2cexp e2)
fexp2cexp (FLit e) = CFLit e
fexp2cexp (Fif0 e1 e2 e3) = CFif0 (fexp2cexp e1) (fexp2cexp e2) (fexp2cexp e3)
fexp2cexp (FTuple tuple) = CFTuple (map fexp2cexp tuple)
fexp2cexp (FProj i e) = CFProj i (fexp2cexp e)
fexp2cexp (FFix t1 f t2) = 
  let  t = (Typ (ftyp2ctyp t1) (\e -> groupLambda (FLam t1 (f e)))) -- is this right???? (BUG)
  in   CFix (CForall (adjust (FFun t1 t2) t)) t
fexp2cexp e             = CLam (groupLambda e)

adjust :: PFTyp t -> EScope t e -> TScope t
adjust (FFun t1 t2) (Typ t1' g) = Typ t1' (\_ -> adjust t2 (g undefined)) -- not very nice!
adjust (FForall f) (Kind g)     = Kind (\t -> adjust (f t) (g t))
adjust t (Body b)               = Body (ftyp2ctyp t)

groupLambda :: PFExp t e -> EScope t e
groupLambda (FBLam f)  = Kind (\a -> groupLambda (f a))
groupLambda (FLam t f) = Typ (ftyp2ctyp t) (\x -> groupLambda (f x))
groupLambda e          = Body (fexp2cexp e)

-- join

scope2ctyp :: TScope t -> PCTyp t
scope2ctyp (Body t)  = t
scope2ctyp s         = CForall s

joinPCTyp :: PCTyp (PCTyp t) -> PCTyp t
joinPCTyp (CTVar t)   = t
joinPCTyp (CForall s) = CForall (joinTScope s)
joinPCTyp CInt = CInt
joinPCTyp (CTupleType ts) = CTupleType (map joinPCTyp ts)

joinTScope :: TScope (PCTyp t) -> TScope t
joinTScope (Body b)   = Body (joinPCTyp b)
joinTScope (Kind f)   = Kind (joinTScope . f . CTVar)
joinTScope (Typ t f)  = let t' = (joinPCTyp t) in Typ t' (\x -> joinTScope (f x))

-- Free variable substitution

substScope n t (Body t1) = Body (substType n t t1)
substScope n t (Kind f)  = Kind (\a -> substScope n t (f a))
substScope n t (Typ t1 f) = Typ (substType n t t1) (\x -> substScope n t (f x))

substType n t (CTVar x) = subst n t x
substType n t (CForall s) = CForall (substScope n t s)
substType n t x = x

class Subst t where
   subst :: Int -> PCTyp Int -> t -> PCTyp t

instance Subst Int where
   subst n t x 
      | n == x = t
      | otherwise = CTVar x

instance Subst t => Subst (PCTyp t) where
   subst n t x = CTVar (substType n t x)
   
-- Pretty Printing

showPCTyp :: PCTyp Int -> Int -> String
showPCTyp (CTVar i) n = "a" ++ show i
showPCTyp (CForall s) n = "(forall " ++ showTScope s n ++ ")"
showPCTyp CInt n = "Int"

showTScope (Body t) n = ". " ++ showPCTyp t n
showTScope (Kind f) n = "a" ++ show n ++ " " ++ showTScope (f n) (n+1)
showTScope (Typ t f) n = "(_ : " ++ showPCTyp t n ++ ") " ++ showTScope (f ()) (n)

showEScope (Body t) n = ". " ++ showPCExp t n
showEScope (Kind f) n = "a" ++ show n ++ " " ++ showEScope (f n) (n+1)
showEScope (Typ t f) n = "(x" ++ show n ++ " : " ++ showPCTyp t n ++ ") " ++ showEScope (f n) (n+1)

showPCExp (CVar x) n      = "x" ++ show x
showPCExp (CLam s) n      = "(\\" ++ showEScope s n ++ ")"
showPCExp (CApp e1 e2) n  = showPCExp e1 n ++ " " ++ showPCExp e2 n
showPCExp (CTApp e t) n   = showPCExp e n ++ " " ++ showPCTyp t n
showPCExp (CFix t f) n    = "TODO!"

instance Show (PCTyp Int) where
   show e = showPCTyp e 0

instance Show (TScope Int) where
   show e = showTScope e 0

instance Show (EScope Int Int) where
   show e = showEScope e 0

instance Show (PCExp Int Int) where
   show e = showPCExp e 0