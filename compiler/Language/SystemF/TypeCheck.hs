
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Language.SystemF.TypeCheck
    ( unsafeGeneralize
    , infer
    ) where

import qualified Unsafe.Coerce

import Language.SystemF.Syntax

{- Generalizing from a concrete type to a polymorphic type:

This is just the identity function (assumming that the expression
does not contain free variables).

Thus a quick hack to create this function is to use unsafeCoerce.

Alternatively we could create a function generalize that traverses
the structure and converts the tree.

-}

unsafeGeneralize :: PFExp Int (Int, PFTyp Int) -> PFExp t e
unsafeGeneralize = Unsafe.Coerce.unsafeCoerce

{- Alternative to gen: not all cases completed -}

{-
generalizeTyp :: PFTyp Int -> [t] -> PFTyp t
generalizeTyp (FTVar i) tenv = FTVar (tenv !! i)
-- generalizeTyp ()

generalize :: PFExp Int (Int,PFTyp Int) -> [t] -> [e] -> PFExp t e
generalize (FVar x t) tenv env  = FVar x (env !! fst t)
generalize (FBLam f) tenv env   = FBLam (\a -> generalize (f (length tenv)) (a:tenv) env)
generalize (FLam t f) tenv env  =
  FLam (generalizeTyp t tenv) (\x -> generalize (f (length env,t)) tenv env)
-}

infer :: Int -> PFExp Int (Int, PFTyp Int) -> PFTyp Int
infer _ (FVar _ t) = snd t
infer i (FBLam f)  = FForall (\a -> infer i (f a))
infer i (FLam t g) = FFun t (infer (i+1) (g (i, t)))
infer i (FTApp e t) =
    case infer i e of
        FForall g -> substFree (i, t) (g i)
        _         -> error "Cannot apply type to a type that is a non-forall type"
infer i (FApp e1 e2) =
    let t3 = infer i e2 in
    case infer i e1 of
        FFun t1 t2 | t1 == t3 -> t2
        _                     -> error "Type mismatch in function application"
infer i (FPrimOp e1 op e2) =
    let t1 = infer i e1
        t2 = infer i e2
    in
    if t1 == t2 then t1
                else error "Type mismatch of two operands in primitive operation"
infer i (FLit _) = FInt
infer i (FIf0 e1 e2 e3) =
    case infer i e1 of
        FInt -> let t2 = infer i e2
                    t3 = infer i e3
                in
                if t2 == t3 then t2
                            else error "Type mismatch in two branches of if expression"
        _    -> error "The predicate of an if expression should be of literal type"
infer i (FTuple es) = FProduct $ map (infer i) es
infer i (FProj idx e) = case infer i e of
                        FProduct ts | idx <= length ts -> ts !! (i-1)
                        FProduct _                     -> error "Index out of bounds in projection"
                        _                              -> error "Projection on a non-tuple"
infer _ (FFix _ t1 t2) = FFun t1 t2

substFree :: (Int, PFTyp Int) -> PFTyp Int -> PFTyp Int
substFree (i, t) (FTVar j)
    | j == i    = t
    | otherwise = FTVar j
substFree (i, t) (FForall f)   = FForall (\a -> substFree (i, t) (f a))
substFree (i, t) (FFun t1 t2)  = FFun (substFree (i, t) t1) (substFree (i, t) t2)
substFree (i, t) FInt          = FInt
substFree (i, t) (FProduct ts) = FProduct (map (substFree (i, t)) ts)

{-
inferWithEnv :: (t1, t2, [(String, PFTyp t)]) -> PFExp t e -> PFTyp t
inferWithEnv (tenv, env, venv) e = case e of
    FVar s _        -> fromMaybe (error $ "Not in scope: variable: `" ++ s ++ "'") (lookup s venv)
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
    FFix f t1 t2 -> FFun t1 t2
-}
