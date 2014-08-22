{-# LANGUAGE RecordWildCards #-}

module Desugar (desugarTcExpr) where

import ESF.Syntax
-- import ESF.TypeCheck

import SystemF.Syntax

import Data.Maybe       (fromJust)

import qualified Data.Map as Map

desugarTcExpr :: TcExpr -> PFExp t e
desugarTcExpr = dsTcExpr (Map.empty, Map.empty)

transType :: Map.Map Name t -> Type -> PFTyp t
transType d = go
  where
    go (TyVar a)    = FTVar (fromJust (Map.lookup a d))
    go (JClass c)   = FJClass c
    go (Fun t1 t2)  = FFun (go t1) (go t2)
    go (Product ts) = FProduct (map go ts)
    go (Forall a t) = FForall (\a' -> transType (Map.insert a a' d) t)
    go (ListOf t)   = FListOf (go t)

type DsEnv t e = (Map.Map Name t, Map.Map Name (Either e (PFExp t e)))

dsTcExpr :: DsEnv t e -> TcExpr -> PFExp t e
dsTcExpr (d, g) = go
  where
    go (Var (x,_t))      = case fromJust (Map.lookup x g) of
                             Left x' -> FVar x x'
                             Right e -> e
    go (Lit lit)         = FLit lit
    go (App e1 e2)       = FApp (go e1) (go e2)
    go (TApp e t)        = FTApp (go e) (transType d t)
    go (Tuple es)        = FTuple (map go es)
    go (Proj e i)        = FProj i (go e)
    go (PrimOp e1 op e2) = FPrimOp (go e1) op (go e2)
    go (If e1 e2 e3)     = FIf (go e1) (go e2) (go e3)
    go (Lam (x, t) e)    = FLam
                             (transType d t)
                             (\x' -> dsTcExpr (d, Map.insert x (Left x') g) e)
    go (BLam a e)        = FBLam (\a' -> dsTcExpr (Map.insert a a' d, g) e)
    go Let{..}           = invariantFailed "dsTcExpr" (show (Let{..} :: TcExpr))
    go (LetOut _ [] e)   = go e

{-
   let f1 : t1 = e1 in e
~> (\(f1 : t1. e)) e1
-}
    go (LetOut NonRec [(f1, t1, e1)] e) =
      FApp
        (FLam (transType d t1) (\f1' -> dsTcExpr (d, Map.insert f1 (Left f1') g) e))
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
          (\y -> dsTcExpr (d, g' y `Map.union` g) e))
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
   let f : forall A1...An. t1 -> t2 = e@(\(x : t1). peeled_e) in body
                                      ^
Inside the marked e, f is polymorphic.

~> let f = /\A1...An. (fix f (x1 : t1) : t2. peeled_e) in body
                                             ^
However, f no longer is polymorphic inside peeled_e as the type variables A1...An has
been instantiated. If inside the original e, f is instantiated with different sets of
type variables, then after this rewriting the expression no longer typechecks.

Conclusion: this rewriting cannot allow type variables in the RHS of the binding.

~> (\(f : forall A1...An. t1 -> t2). body)
     (/\A1...An. fix y (x1 : t1) : t2. peeled_e)
-}
    go (LetOut Rec [(f,t@(Fun _ _),e)] body) = dsLetRecDirect (d,g) (LetOut Rec [(f,t,e)] body)
    go (LetOut Rec [(f,t,e)] body)           = dsLetRecEncode (d,g) (LetOut Rec [(f,t,e)] body)
    go (LetOut Rec bs body)                  = dsLetRecEncode (d,g) (LetOut Rec bs body)

    go (JNewObj cName args)    = FJNewObj cName (map go args)
    go (JMethod ec mName args r) = FJMethod (go ec) mName (map go args) r
    go (PrimList l)              = FPrimList (map go l)



dsLetRecDirect :: DsEnv t e -> TcExpr -> PFExp t e
dsLetRecDirect (d,g) = go
  where
    go (LetOut Rec [(f,t,e)] body) =
      FApp
        (FLam
          (transType d t)
          (\f' -> dsTcExpr (d, addToEnv [(f,Left f')] g) body))
        (FFix
          (\f' x1' -> dsTcExpr (d, addToEnv [(f, Left f'), (x1, Left x1')] g) peeled_e)
          (transType d t1)
          (transType d t2))
          where
            addToEnv bindings g = foldr (\(x,x') acc -> Map.insert x x' acc) g bindings
            (Just (x1, t1), t2, peeled_e) = peel Nothing (e,t)
              where
                peel Nothing (Lam (x1,t1) e, Fun _ t) = (Just (x1,t1), t, e)
                peel _ _ = invariantFailed "peel" "I cannot peel an expression that is not a function"

{-
   let rec f1 = e1, ..., fn = en in e
~> let y = fix y (dummy : Int) : (t1, ..., tn). (e1, ..., en)[*] in e[*]
~> (\(y : Int -> (t1, ..., tn)). e[*])
     (fix y (dummy : Int) : (t1, ..., tn). (e1, ..., en)[*])
-}
dsLetRecEncode :: DsEnv t e -> TcExpr -> PFExp t e
dsLetRecEncode (d,g) = go
  where
    go (LetOut Rec bs@(_:_) e) =
      FApp
        (FLam
          (FFun (FJClass "java.lang.Integer") (transType d tupled_ts))
          (\y -> dsTcExpr (d, g' y `Map.union` g) e))
        (FFix
          (\y _dummy -> dsTcExpr (d, g' y `Map.union` g) tupled_es)
          (FJClass "java.lang.Integer")
          (transType d tupled_ts))
          where
            (fs, ts, es) = unzip3 bs

            tupled_es = Tuple es
            tupled_ts = Product ts

            -- Substitution: fi -> (y 0)._(i-1)
            g' = \y -> Map.fromList
                         (zipWith
                          -- TODO: better var name
                          (\f i -> (f, Right (FProj i (FApp (FVar "" y) (FLit (Integer 0))))))
                          fs
                          [1..length bs])
