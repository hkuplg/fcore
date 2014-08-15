{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module SystemFI.TypeCheck
  ( subtype
  , infer
  ) where

import SystemFI.Syntax hiding (fsubstEE, fsubstTE)
import SystemF.Syntax  hiding (fsubstTT)

import qualified Data.Map as Map

subtype :: Type Int -> Type Int -> Bool
subtype = subtype' 0

subtype' :: Int -> Type Int -> Type Int -> Bool
subtype' i  Int         Int        = True                                 -- Sub-Int
subtype' i (TVar x)    (TVar y)    = x == y                               -- Sub-TVar
subtype' i (Forall f)  (Forall g)  = subtype' (i + 1) (f i) (g i)         -- Sub-Forall
subtype' i (Fun t1 t2) (Fun t3 t4) = subtype' i t3 t1 && subtype' i t2 t4 -- Sub-Arrow
subtype' i  t          (And t1 t2) = subtype' i t  t1 && subtype' i t  t2 -- Sub-&1
subtype' i (And t1 t2)  t          = subtype' i t1 t  || subtype' i t2 t  -- Sub-&2, Sub-&3
subtype' i  _           _          = False

transType :: Int -> Type Int -> PFTyp Int
transType i (TVar a)      = FTVar a
transType i Int           = FInt
transType i (a1 `Fun` a2) = transType i a1 `FFun` transType i a2
transType i (a1 `And` a2) = FProduct [transType i a1, transType i a2]
transType i (Forall f)    = FForall (\a -> transType (i + 1) (f i))

infer :: Expr Int Int -> Maybe (Type Int, PFExp Int Int)
infer = infer' (0,0) Map.empty

infer' :: (Int, Int) -> Map.Map Int (Type Int) -> Expr Int Int -> Maybe (Type Int, PFExp Int Int)
infer' (i,j) env (Var x)       = do t <- Map.lookup x env
                                    return (t, FVar "" x)
infer' (i,j) env (Lit n)       = return (Int, FLit n)
infer' (i,j) env (BLam f)      = do (t, m) <- infer' (succ i, j) env (f i)
                                    return ( Forall (\a -> fsubstTT (i, TVar a) t)
                                           , FBLam (\a -> fsubstEE (i, FVar "" a) m))
infer' (i,j) env (Lam t f)     = do (tbody, m) <- infer' (i, succ j) (Map.insert j t env) (f j)
                                    return ( Fun t tbody
                                           , FLam (transType i t) (\x -> fsubstEE (j, FVar "" x) m))
infer' (i,j) env (Merge e1 e2) = do (t1, m1) <- infer' (i,j) env e1
                                    (t2, m2) <- infer' (i,j) env e2
                                    return (And t1 t2, FTuple [m1, m2])
infer' (i,j) env (TApp e t)    = do (t1, m) <- infer' (i,j) env e
                                    case (t1, m) of
                                      (Forall f, FBLam g) -> return ( fsubstTT (i,t) (f i)
                                                                    , fsubstTE (i, transType i t) (g i))
                                      _                   -> Nothing
infer' (i,j) env (App e1 e2)   = do (t1, m1) <- infer' (i,j) env e1
                                    (t2, m2) <- infer' (i,j) env e2
                                    case t1 of
                                      Fun t t'| t2 `subtype` t -> undefined
                                      _ -> Nothing