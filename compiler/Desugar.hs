-- The desugarer: turning the source language into System F (with intersection types).

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Desugar (desugar) where

import Src

import qualified Core as C

import Panic

import Data.Maybe       (fromMaybe)
import StringPrefixes

import qualified Data.Map as Map

desugar :: Expr TcId -> C.Expr t e
desugar = dsTcExpr (Map.empty, Map.empty)

type DsTyVarMap t = Map.Map Name t
type DsVarMap t e = Map.Map Name (C.Expr t e)

type DsMap t e = (DsTyVarMap t, DsVarMap t e)

transType :: DsTyVarMap t -> Type -> C.Type t
transType d = go
  where
    go (TyVar a)    = C.TVar (fromMaybe (panic "Desugar.transType: TyVar") (Map.lookup a d))
    go (JClass c)   = C.JClass c
    go (Fun t1 t2)  = C.Fun (go t1) (go t2)
    go (Product ts) = C.Product (map go ts)
    go (Forall a t) = C.Forall (\a' -> transType (Map.insert a a' d) t)
    go (And t1 t2)  = C.And (go t1) (go t2)
    go (RecordTy fs) =
      case fs  of
        [(l,t)]  -> C.RecordTy (l, go t)
        _        -> go (RecordTy (take (length fs - 1) fs)) `C.And` (C.RecordTy (let (l,t) = last fs in (l,go t)))
    go UnitType     = C.JClass "java.lang.Integer"

dsTcExpr :: DsMap t e -> Expr TcId -> C.Expr t e
dsTcExpr (d, g) = go
  where
    go (Var (x,_t))      = fromMaybe (panic "Desugar.dsTcExpr: Var") (Map.lookup x g)
    go (Lit lit)         = C.Lit
                             (case lit of Unit -> Integer 0
                                          _    -> lit)
    go (App e1 e2)       = C.App (go e1) (go e2)
    go (TApp e t)        = C.TApp (go e) (transType d t)
    go (Tuple es)        = C.Tuple (map go es)
    go (Proj e i)        = C.Proj i (go e)
    go (PrimOp e1 op e2) = C.PrimOp (go e1) op (go e2)
    go (If e1 e2 e3)     = C.If (go e1) (go e2) (go e3)
    go (Lam (x, t) e)    = C.Lam
                               (transType d t)
                               (\x' -> dsTcExpr (d, Map.insert x (C.Var x') g) e)
    go (BLam a e)        = C.BLam (\a' -> dsTcExpr (Map.insert a a' d, g) e)
    go Let{..}           = panic "Desugar.dsTcExpr: Let"
    go (LetOut _ [] e)   = go e
    go (Merge e1 e2)     = C.Merge (go e1) (go e2)
    go (Record fs)       =
      case fs of
        []       -> panic "Desugar.dsTcExpr: Record"
        [(l,e)]  -> C.RecordIntro (l, go e)
        _        -> go (Record (take (length fs - 1) fs)) `C.Merge` (C.RecordIntro (let (l,e) = last fs in (l,go e)))
    go (RecordAccess e l) = C.RecordElim (go e) l
    go (RecordUpdate e fs) =
      case fs of
        [] -> go e
        _  -> C.RecordUpdate (go (RecordUpdate e (take (length fs - 1) fs))) (let (l1,e1) = last fs in (l1, go e1))

    go (LetOut NonRec [(f1, t1, e1)] e) =
      -- C.App
      --   (C.Lam (transType d t1) (\f1' -> dsTcExpr (d, Map.insert f1 (C.Var f1') g) e))
      --   (go e1)
      C.Let (go e1) (\f1' -> dsTcExpr (d, Map.insert f1 (C.Var f1') g) e)

{-
Note that rewriting simultaneous let expressions by nesting is wrong unless we do
variable renaming. An example:

# let x = 1 in let x = 2 and y = x in y;;
- : int = 1

# let x = 1 in let x = 2 in let y = x in y;;
- : int = 2

   let f1 = e1, ..., fn = en in e
~> let y = (t1, ..., tn). (e1, ..., en) in e[*]
-}
    go (LetOut NonRec bs@(_:_) e) =
      C.Let
        (go tupled_es)
        (\y -> dsTcExpr (d, g' y `Map.union` g) e)
        where
          (fs, _, es)  = unzip3 bs

          tupled_es = Tuple es

          -- Substitution: fi -> y._(i-1)
          g' y = Map.fromList $
                   zipWith (\f i -> (f, (C.Proj i (C.Var y))))
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
    go (LetOut Rec [(f,t@(Fun _ _),e)] body) = dsLetRecToFix (d,g) (LetOut Rec [(f,t,e)] body)
    go (LetOut Rec [(f,t,e)] body)           = dsLetRecToLetRec (d,g) (LetOut Rec [(f,t,e)] body)
    go (LetOut Rec bs body)                  = dsLetRecToLetRec (d,g) (LetOut Rec bs body)
    go (JNewObj c args)          = C.JNewObj c (map go args)
    go (JMethod callee m args r) = C.JMethod (fmap go callee) m (map go args) r
    go (JField  callee f r)      = C.JField  (fmap go callee) f r

    -- Non Java Class translation
    -- go (PrimList l)              = FPrimList (map go l)

    -- Primitive List to java class

    go (PrimList l)              = case l of     -- translate to java new obj
                                     []   -> C.JNewObj (namespace ++ "FunctionalList") []
                                     x:xs -> C.JNewObj (namespace ++ "FunctionalList") [go x, go (PrimList xs)]

    go (Seq es) = C.Seq (map go es)

dsLetRecToFix :: DsMap t e -> Expr TcId -> C.Expr t e
dsLetRecToFix (d,g) = go
  where
    go (LetOut Rec [(f,t,e)] body) =
      C.App
          (C.Lam
              (transType d t)
              (\f' -> dsTcExpr (d, addToEnv [(f,C.Var f')] g) body))
          (C.Fix
              (\f' x1' -> dsTcExpr (d, addToEnv [(f, C.Var f'), (x1, C.Var x1')] g) peeled_e)
              (transType d t1)
              (transType d t2))
              where
                addToEnv binds g = foldr (\(x,x') acc -> Map.insert x x' acc) g binds -- TODO: subsumed
                (Just (x1, t1), t2, peeled_e) = peel Nothing (e,t)
                  where
                    peel Nothing (Lam (x1,t1) e, Fun _ t) = (Just (x1,t1), t, e)
                    peel _ _ = panic "Desugar.dsLetRecToFix: not a function"
    go _ = panic "Desugar.dsLetRecToFix"

{-
    let rec f1 = e1, ..., fn = en in e
~>  let y = fix y (dummy : Int) : (t1, ..., tn). (e1, ..., en)[*] in e[*]
~>  (\(y : Int -> (t1, ..., tn)). e[*])
      (fix y (dummy : Int) : (t1, ..., tn). (e1, ..., en)[*])
-}
dsLetRecToFixEncoded :: DsMap t e -> Expr TcId -> C.Expr t e
dsLetRecToFixEncoded (d,g) = go
  where
    go (LetOut Rec bs@(_:_) e) =
      C.App
          (C.Lam
              (C.Fun (C.JClass "java.lang.Integer") (transType d tupled_ts))
              (\y -> dsTcExpr (d, g' y `Map.union` g) e))
          (C.Fix
              (\y _dummy -> dsTcExpr (d, g' y `Map.union` g) tupled_es)
              (C.JClass "java.lang.Integer")
              (transType d tupled_ts))
              where
                (fs, ts, es) = unzip3 bs

                tupled_es = Tuple es
                tupled_ts = Product ts

                -- Substitution: fi -> (y 0)._(i-1)
                g' y = Map.fromList $
                         zipWith
                             -- TODO: better var name
                           (\f i -> (f, C.Proj i (C.App (C.Var y) (C.Lit (Integer 0)))))
                           fs
                           [1..length bs]
    go _ = panic "Desugar.dsLetRecEncode"

-- Convert from: LetOut RecFlag [(Name, Type, Expr TcId)] (Expr TcId)
-- To:           LetRec [Type t] ([e] -> [Expr t e]) ([e] -> Expr t e)
dsLetRecToLetRec :: DsMap t e -> Expr TcId -> C.Expr t e
dsLetRecToLetRec (d,g) (LetOut Rec binds@(_:_) body) = C.LetRec sigs' binds' body'
  where
    (ids, sigs, defs) = unzip3 binds
    sigs'  = map (transType d) sigs
    binds' = \ids' -> map (dsTcExpr (d, zip ids (map C.Var ids') `addToVarMap` g)) defs
    body'  = \ids' -> dsTcExpr (d, zip ids (map C.Var ids') `addToVarMap` g) body

dsLetRecToLetRec _ _ = panic "Desugar.dsLetRecToLetRec"

addToVarMap :: [(Name, C.Expr t e)] -> DsVarMap t e -> DsVarMap t e
addToVarMap xs var_map = foldr (\(x,x') acc -> Map.insert x x' acc) var_map xs
