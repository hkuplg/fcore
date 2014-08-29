{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE RecordWildCards #-}

module Desugar (desugar) where

import ESF.Syntax

import qualified Core as C

import JavaUtils

import Data.Maybe       (fromJust)

import qualified Data.Map as Map

desugar :: Expr TcId -> C.Expr t e
desugar = dsTcExpr (Map.empty, Map.empty)

transType :: Map.Map Name t -> Type -> C.Type t
transType d = go
  where
    go (TyVar a)    = C.TVar (fromJust (Map.lookup a d))
    go (JClass c)   = C.JClass c
    go (Fun t1 t2)  = C.Fun (go t1) (go t2)
    go (Product ts) = C.Product (map go ts)
    go (Forall a t) = C.Forall (\a' -> transType (Map.insert a a' d) t)
    go (And t1 t2)  = C.And (go t1) (go t2)

type DsEnv t e = (Map.Map Name t, Map.Map Name (Either e (C.Expr t e)))

dsTcExpr :: DsEnv t e -> Expr TcId -> C.Expr t e
dsTcExpr (d, g) = go
  where
    go (Var (x,_t))      = case fromJust (Map.lookup x g) of
                             Left x' -> C.Var x'
                             Right e -> e
    go (Lit lit)         = C.Lit lit
    go (App e1 e2)       = C.App (go e1) (go e2)
    go (TApp e t)        = C.TApp (go e) (transType d t)
    go (Tuple es)        = C.Tuple (map go es)
    go (Proj e i)        = C.Proj i (go e)
    go (PrimOp e1 op e2) = C.PrimOp (go e1) op (go e2)
    go (If e1 e2 e3)     = C.If (go e1) (go e2) (go e3)
    go (Lam (x, t) e)    = C.Lam
                               (transType d t)
                               (\x' -> dsTcExpr (d, Map.insert x (Left x') g) e)
    go (BLam a e)        = C.BLam (\a' -> dsTcExpr (Map.insert a a' d, g) e)
    go Let{..}           = invariantFailed "dsTcExpr" (show (Let{..} :: Expr TcId))
    go (LetOut _ [] e)   = go e
    go (Merge e1 e2)     = C.Merge (go e1) (go e2)

{-
   let f1 : t1 = e1 in e
~> (\(f1 : t1. e)) e1
-}
    go (LetOut NonRec [(f1, t1, e1)] e) =
      C.App
        (C.Lam (transType d t1) (\f1' -> dsTcExpr (d, Map.insert f1 (Left f1') g) e))
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
      C.App
        (C.Lam
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
                         (\f i -> (f, Right (C.Proj i (C.Var y))))
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

    go (JNewObj cName args)    = C.JNewObj cName (map go args)
    go (JMethod c mName args r) =
      case c of
        (Left cExpr)  -> C.JMethod (Left (go cExpr)) mName (map go args) r
        (Right cName) -> C.JMethod (Right cName) mName (map go args) r

    go (JField c fName r) =
      case c of
        (Left cExpr)  -> C.JField (Left (go cExpr)) fName r
        (Right cName) -> C.JField (Right cName) fName r

    go (SeqExprs es) = C.Seq (map go es)

dsLetRecDirect :: DsEnv t e -> Expr TcId -> C.Expr t e
dsLetRecDirect (d,g) = go
  where
    go (LetOut Rec [(f,t,e)] body) =
      C.App
          (C.Lam
              (transType d t)
              (\f' -> dsTcExpr (d, addToEnv [(f,Left f')] g) body))
          (C.Fix
              (\f' x1' -> dsTcExpr (d, addToEnv [(f, Left f'), (x1, Left x1')] g) peeled_e)
              (transType d t1)
              (transType d t2))
              where
                addToEnv bindings g = foldr (\(x,x') acc -> Map.insert x x' acc) g bindings
                (Just (x1, t1), t2, peeled_e) = peel Nothing (e,t)
                  where
                    peel Nothing (Lam (x1,t1) e, Fun _ t) = (Just (x1,t1), t, e)
                    peel _ _ = invariantFailed "peel" "I cannot peel an expression that is not a function"
    go _ = invariantFailed "dsLetDirect" "Unexpected pattern for a partial function"

{-
    let rec f1 = e1, ..., fn = en in e
~>  let y = fix y (dummy : Int) : (t1, ..., tn). (e1, ..., en)[*] in e[*]
~>  (\(y : Int -> (t1, ..., tn)). e[*])
      (fix y (dummy : Int) : (t1, ..., tn). (e1, ..., en)[*])
-}
dsLetRecEncode :: DsEnv t e -> Expr TcId -> C.Expr t e
dsLetRecEncode (d,g) = go
  where
    go (LetOut Rec bs@(_:_) e) =
      C.App
          (C.Lam
              (C.Fun (C.JClass javaIntClass) (transType d tupled_ts))
              (\y -> dsTcExpr (d, g' y `Map.union` g) e))
          (C.Fix
              (\y _dummy -> dsTcExpr (d, g' y `Map.union` g) tupled_es)
              (C.JClass javaIntClass)
              (transType d tupled_ts))
              where
                (fs, ts, es) = unzip3 bs

                tupled_es = Tuple es
                tupled_ts = Product ts

                -- Substitution: fi -> (y 0)._(i-1)
                g' = \y -> Map.fromList
                             (zipWith
                              -- TODO: better var name
                              (\f i -> (f, Right (C.Proj i (C.App (C.Var y) (C.Lit (Integer 0))))))
                              fs
                              [1..length bs])
    go _ = invariantFailed "dsLetDirect" "Unexpected pattern for a partial function"
