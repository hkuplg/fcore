{-# LANGUAGE RecordWildCards #-}

module Desugar (desugarTcExpr) where

import ESF.Syntax
-- import ESF.TypeCheck

import SystemF.Syntax

import Data.Maybe       (fromJust)

import qualified Data.Map as Map
import qualified Data.Set as Set

transType :: Map.Map Name t -> Type -> PFTyp t
transType d = go
  where
    go (TyVar a)    = FTVar (fromJust $ Map.lookup a d)
    go  Int         = FInt
    go (Fun t1 t2)  = FFun (go t1) (go t2)
    go (Product ts) = FProduct (map go ts)
    go (Forall a t) = FForall (\a' -> transType (Map.insert a a' d) t)

desugarTcExpr :: TcExpr -> PFExp t e
desugarTcExpr = transTcExpr (Map.empty, Map.empty)

type TransEnv t e = (Map.Map Name t, Map.Map Name (Either e (PFExp t e)))

transTcExpr :: TransEnv t e -> TcExpr -> PFExp t e
transTcExpr (d, g) = go
  where
    go (Var (x,_t))      = case fromJust (Map.lookup x g) of
                             Left x' -> FVar x x'
                             Right e -> e
    go (Lit (Integer n)) = FLit n
    go (App e1 e2)       = FApp (go e1) (go e2)
    go (TApp e t)        = FTApp (go e) (transType d t)
    go (Tuple es)        = FTuple (map go es)
    go (Proj e i)        = FProj i (go e)
    go (PrimOp e1 op e2) = FPrimOp (go e1) op (go e2)
    go (If0 e1 e2 e3)    = FIf0 (go e1) (go e2) (go e3)
    go (Lam (x, t) e)    = FLam
                             (transType d t)
                             (\x' -> transTcExpr (d, Map.insert x (Left x') g) e)
    go (BLam a e)        = FBLam (\a' -> transTcExpr (Map.insert a a' d, g) e)
    go Let{..}           = invariantFailed "transTcExpr" (show (Let{..} :: TcExpr))
    go (LetOut _ [] e)   = go e

{-
   let f1 : t1 = e1 in e
~> (\(f1 : t1. e)) e1
-}
    go (LetOut NonRec [(f1, t1, e1)] e) =
      FApp
        (FLam (transType d t1) (\f1' -> transTcExpr (d, Map.insert f1 (Left f1') g) e))
        (go e1)

{-
Note that rewriting simultaneous let expressions by nesting is wrong unless we do
variable renaming. An example:

# let x = 1 in let x = 2 and y = x in y;;
- : int = 1

# let x = 1 in let x = 2 in let y = x in y;;
- : int = 2

   let f1 = e1, ..., fn = en in e
~> let y = (t1, ..., tn). (e1, ..., en) in e[*]
~> (\(y : (t1, ..., tn)). e[*]) (e1, ..., en)
-}
    go (LetOut NonRec bs@(_:_) e) =
      FApp
        (FLam
          (transType d tupled_ts)
          (\y -> transTcExpr (d, g' y `Map.union` g) e))
        (go tupled_es)
        where
          (fs, ts, es)  = unzip3 bs

          tupled_es = Tuple es
          tupled_ts = Product ts

          -- Substitution: fi -> y._(i-1)
          g' = \y -> Map.fromList $
                       zipWith
                         (\f i -> (f, Right (FProj i (FVar "" y))))
                         fs
                         [1..length bs]

{-
   let rec f1 = e1, ..., fn = en in e
~> let y = fix y (dummy : Int) : (t1, ..., tn). (e1, ..., en)[*] in e[*]
~> (\(y : Int -> (t1, ..., tn)). e[*])
     (fix y (dummy : Int) : (t1, ..., tn). (e1, ..., en)[*])
-}
    go (LetOut Rec bs@(_:_) e) =
      FApp
        (FLam
          (FFun FInt (transType d tupled_ts))
          (\y -> transTcExpr (d, g' y `Map.union` g) e))
        (FFix
          (\y _dummy -> transTcExpr (d, g' y `Map.union` g) tupled_es)
          FInt (transType d tupled_ts))
          where
            (fs, ts, es)  = unzip3 bs

            tupled_es = Tuple es
            tupled_ts = Product ts

            -- Substitution: fi -> (y 0)._(i-1)
            g' = \y -> Map.fromList $
                         zipWith
                          (\f i -> (f, Right (FProj i (FApp (FVar "" y) (FLit 0)))))
                          fs
                          [1..length bs]
