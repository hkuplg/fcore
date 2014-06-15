{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Language.SystemF.TypeCheck 
    ( -- inferWithEnv
     infer, gen
    ) where

import Data.Maybe (fromMaybe)

import Language.SystemF.Syntax
import Unsafe.Coerce

{- Generalizing from a concrete type to a polymorphic type:

This is just the identity function (assumming that the expression 
does not contain free variables). 

Thus a quick hack to create this function is to use unsafeCoerce.

Alternatively we could create a function generalize that traverses
the structure and converts the tree.

-}

gen :: PFExp Int (Int, PFTyp Int) -> PFExp t e
gen = unsafeCoerce

{- Alternative to gen: not all cases completed -}

generalizeTyp :: PFTyp Int -> [t] -> PFTyp t
generalizeTyp (FTVar i) tenv = FTVar (tenv !! i)
-- generalizeTyp ()

generalize :: PFExp Int (Int,PFTyp Int) -> [t] -> [e] -> PFExp t e
generalize (FVar x t) tenv env  = FVar x (env !! fst t)
generalize (FBLam f) tenv env   = FBLam (\a -> generalize (f (length tenv)) (a:tenv) env)
generalize (FLam t f) tenv env  = 
  FLam (generalizeTyp t tenv) (\x -> generalize (f (length env,t)) tenv env)

eqTyp :: PFTyp Int -> PFTyp Int -> Int -> Bool
eqTyp (FTVar x) (FTVar y)      _  = x == y
eqTyp (FForall f) (FForall g)  n  = eqTyp (f n) (g n) (n+1)

infer :: PFExp Int (Int,PFTyp Int) -> Int -> PFTyp Int -- Maybe (PFTyp Int)
infer (FVar _ t) _  = snd t
infer (FBLam f) i   = FForall (\a -> infer (f a) i)
infer (FLam t g) i  = FFun t (infer (g (i,t)) (i+1)) 
infer (FTApp e t) i = 
  case infer e i of
    FForall g -> substFree i t (g i)
infer (FApp e1 e2) i = 
  let t3 = infer e2 i in
  case infer e1 i of
    FFun t1 t2 | eqTyp t1 t3 i -> t2
    
substFree :: Int -> PFTyp Int -> PFTyp Int -> PFTyp Int
substFree = undefined

{-
inferWithEnv :: (t1, t2, [(String, PFTyp t)]) -> PFExp t e -> PFTyp t
inferWithEnv (tenv, env, venv) e = case e of
    FVar s _        -> fromMaybe (error $ "Unbound variable: `" ++ s ++ "'") (lookup s venv)
    FBLam f         -> FForall (inferWithEnv (tenv, env, venv) . f)
    FLam t f        -> error "FLam" -- FFun t (inferWithEnv (tenv, env, venv) $ f 0) -- TODO
    FApp e1 e2      -> let te1 = inferWithEnv (tenv, env, venv) e1
                           te2 = inferWithEnv (tenv, env, venv) e2
                       in
                       case te1 of 
                            FFun t1 t2 {- | te2 == t1 -} -> error "Function application OK" -- TODO
                            _ -> error "Function application not OK" -- TODO
    FTApp e t        -> error "FTApp" -- TODO
    FPrimOp e1 op e2 -> let te1 = inferWithEnv (tenv, env, venv) e1
                            te2 = inferWithEnv (tenv, env, venv) e2
                        in
                        error "FPrimOp" -- TODO
    FLit n           -> FInt
    FIf0 e1 e2 e3    -> let te1 = inferWithEnv (tenv, env, venv) e1
                            te2 = inferWithEnv (tenv, env, venv) e2 
                            te3 = inferWithEnv (tenv, env, venv) e3
                        in
                        error "Fif0" -- TODO
    FTuple es -> FProduct $ map (inferWithEnv (tenv, env, venv)) es
    FProj i e1 -> case inferWithEnv (tenv, env, venv) e1 of 
                    FProduct ts | i <= length ts -> ts !! (i - 1)
                    FProduct _                   -> error "Index out of bounds in projection"
                    _                         -> error "Projection on a non-tuple" 
    FFix t1 f t2 -> FFun t1 t2
-}
