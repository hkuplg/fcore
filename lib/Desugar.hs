{- |
Module      :  Desugar
Description :  The desugarer turns the source language into System FI.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3
Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>
Stability   :  experimental
Portability :  portable

The desugarer turns the source language into System FI,
including the removal of type synonyms. No desugaring should happen before this
stage.
-}

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Desugar (desugar) where

import Src
import SrcLoc

import qualified SystemFI as F

import Panic

import Data.Maybe       (fromMaybe)
import Data.List        (intercalate)

import Text.PrettyPrint.ANSI.Leijen

import qualified Data.Map as Map

desugar :: CheckedExpr -> F.Expr t e
desugar = desugarExpr (Map.empty, Map.empty)

type TVarMap t  = Map.Map ReaderId t
type VarMap t e = Map.Map ReaderId (F.Expr t e)

transType :: TVarMap t -> ReaderType -> F.Type t
transType d (TVar a)     = F.TVar a (fromMaybe (panic ("transType: " ++ show (TVar a))) (Map.lookup a d))
transType _ (JType (JClass c)) = F.JClass c
transType _ (JType (JPrim c))  = F.JClass c
transType d (Fun t1 t2)  = F.Fun (transType d t1) (transType d t2)
transType d (Product ts) = F.Product (map (transType d) ts)
transType d (Forall a t) = F.Forall a (\a' -> transType (Map.insert a a' d) t)
transType d (And t1 t2)  = F.And (transType d t1) (transType d t2)
transType d (RecordType fs)  =
                case fs  of
                  [(l,t)]  -> F.RecordType (l, transType d t)
                  _        -> transType d (RecordType (take (length fs - 1) fs)) `F.And` F.RecordType (let (l,t) = last fs in (l,transType d t))
transType _ Unit         = F.Unit
transType i (Thunk t)    = F.Fun F.Unit (transType i t)
transType i (Datatype n ts ns) = F.Datatype n (map (transType i) ts) ns
transType d (ListOf c)   = F.ListOf (transType d c)
transType _ t            = prettySorry "transType" (pretty t)

desugarExpr :: (TVarMap t, VarMap t e) -> CheckedExpr -> F.Expr t e
desugarExpr (d, g) = go
  where
    go (L _ (Var (x,t)))       = fromMaybe (panic "desugarExpr: Var") (Map.lookup x g)
    go (L _ (Lit lit))         = F.Lit lit
    go (L _ (App f x))         = F.App (go f) (go x)
    go (L _ (TApp e t))        = F.TApp (go e) (transType d t)
    go (L _ (Tuple es))        = F.Tuple (map go es)
    go (L _ (Proj e i))        = F.Proj i (go e)
    go (L _ (PrimOp e1 op e2)) = F.PrimOp (go e1) op (go e2)
    go (L _ (If e1 e2 e3))     = F.If (go e1) (go e2) (go e3)
    go (L _ (Lam (x, t) e))    = F.Lam x
                                 (transType d t)
                                 (\x' -> desugarExpr (d, Map.insert x (F.Var x x') g) e)
    go (L _ (BLam a e))        = F.BLam a (\a' -> desugarExpr (Map.insert a a' d, g) e)
    go (L _ Let{..})           = panic "desugarExpr: Let"
    go (L _ (LetOut _ [] e))   = go e
    go (L _ (Merge e1 e2))     = F.Merge (go e1) (go e2)
    go (L _ (RecordCon fs))    =
      case fs of
        []       -> panic "desugarExpr: Record"
        [(l,e)]  -> F.RecordCon (l, go e)
        _        -> go (noLoc $ RecordCon (take (length fs - 1) fs)) `F.Merge` F.RecordCon (let (l,e) = last fs in (l,go e))
    go (L _ (RecordProj e l)) = F.RecordProj (go e) l
    go (L _ (RecordUpdate e fs)) =
      case fs of
        [] -> go e
        _  -> F.RecordUpdate (go (noLoc $ RecordUpdate e (take (length fs - 1) fs))) (let (l1,e1) = last fs in (l1, go e1))

    go (L _ (LetOut NonRec [(f1, _, e1)] e)) =
      -- F.App
      --   (F.Lam (transType d t1) (\f1' -> desugarExpr (d, Map.insert f1 (F.Var f1') g) e))
      --   (go e1)
      F.Let f1 (go e1) (\f1' -> desugarExpr (d, Map.insert f1 (F.Var f1 f1') g) e)

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
    go (L _ (LetOut NonRec bs@(_:_) e)) =
      F.Let (intercalate "_" fs)
        (go tupled_es)
        (\y -> desugarExpr (d, g' y `Map.union` g) e)
        where
          (fs, _, es)  = unzip3 bs

          tupled_es = noLoc $ Tuple es

          -- Substitution: fi -> y._(i-1)
          g' y = Map.fromList $ -- `Map.fromList` is right-biased.
                   zipWith (\f i -> (f, F.Proj i (F.Var f y)))
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
    go (L _ (LetOut Rec [(f,t@(Fun _ _),e)] body)) = desugarLetRecToLetRec (d,g) (noLoc $ LetOut Rec [(f,t,e)] body)
    go (L _ (LetOut Rec [(f,t,e)] body))          = desugarLetRecToLetRec (d,g) (noLoc $ LetOut Rec [(f,t,e)] body)
    go (L _ (LetOut Rec bs body))                 = desugarLetRecToLetRec (d,g) (noLoc $ LetOut Rec bs body)
    go (L _ (JNew c args))             = F.JNew c (map go args)
    go (L _ (JMethod callee m args r)) = F.JMethod (fmap go callee) m (map go args) r
    go (L _ (JField  callee f r))      = F.JField  (fmap go callee) f r

    -- Non Java Class translation
    -- go (PrimList l)              = FPrimList (map go l)

    -- Primitive List to java class

    go (L _ (PolyList l t))            = case l of
                                           []   -> F.PolyList [] (transType d t)
                                           x:xs -> F.PolyList [go x, go (noLoc $ PolyList xs t )]  (transType d t)
    go (L _ (JProxyCall jmethod t))    = F.JProxyCall (go jmethod) (transType d t)

    go (L _ (Seq es)) = F.Seq (map go es)
    go (L _ (Data recflag databinds e)) = F.Data recflag (map desugarDatabind databinds) (go e)

    go (L _ (Constr c es)) = F.Constr (desugarConstructor d c) (map go es)
    go (L _ (Case e alts)) = F.Case (go e) (map desugarAlts alts)
    go (L _ (CaseString e alts)) =
            let emptytest = noLoc $ JMethod (NonStatic e) "isEmpty" [] "java.lang.Boolean"
                [emptyexpr]        = [ expr | ConstrAlt (Constructor "empty" _) _ expr <-  alts]
                [(var1, var2, b2)] = [ (var1',var2',expr) | ConstrAlt (Constructor "cons" _) [var1',var2'] expr <-  alts]
                headfetch = (var1, JType(JClass "java.lang.Character"), noLoc $ JMethod (NonStatic e) "charAt" [noLoc $ Lit (Int 0)] "java.lang.Character")
                tailfetch = (var2, JType(JClass "java.lang.String"), noLoc $ JMethod (NonStatic e) "substring" [noLoc $ Lit (Int 1)] "java.lang.String")
                nonemptyexpr = foldr (\x@(name,_,_) b -> if (name =="_") then b else noLoc $ LetOut NonRec [x] b) b2 [headfetch, tailfetch]
            in
            go (noLoc $ If emptytest emptyexpr nonemptyexpr)

    desugarDatabind (DataBind n params ctrs) =
         F.DataBind n params (\t ->
            let d' = addToTVarMap (zip params t) d
            in  map (desugarConstructor d') ctrs
         )
    desugarConstructor d' (Constructor n ts) = F.Constructor n (map (transType d') ts)
    desugarAlts (ConstrAlt c ns e) =
        let c' = desugarConstructor d c
            f ns' = desugarExpr (d, zipWith (\n e' -> (n, F.Var n e')) ns ns' `addToVarMap` g) e
        in F.ConstrAlt c' ns f


desugarLetRecToFix :: (TVarMap t, VarMap t e) -> CheckedExpr -> F.Expr t e
desugarLetRecToFix (d,g) (L _ (LetOut Rec [(f,t,e)] body)) =
  F.App
      (F.Lam f
          (transType d t)
          (\f' -> desugarExpr (d, addToEnv [(f, F.Var f f')] g) body))
      (F.Fix f x1
          (\f' x1' -> desugarExpr (d, addToEnv [(f, F.Var f f'), (x1, F.Var x1 x1')] g) peeled_e)
          (transType d t1)
          (transType d t2))
          where
            addToEnv binds g0 = foldr (\(x,x') acc -> Map.insert x x' acc) g0 binds -- TODO: subsumed
            (Just (x1, t1), t2, peeled_e) = peel Nothing (unLoc e,t)
              where
                peel Nothing (Lam param b, Fun _ ret_ty) = (Just param, ret_ty, b)
                peel _ (e,t) = prettyPanic "desugarLetRecToFix: not a function" (text (show e))
desugarLetRecToFix _ _ = panic "desugarLetRecToFix"

{-
    let rec f1 = e1, ..., fn = en in e
~>  let y = fix y (dummy : Int) : (t1, ..., tn). (e1, ..., en)[*] in e[*]
~>  (\(y : Int -> (t1, ..., tn)). e[*])
      (fix y (dummy : Int) : (t1, ..., tn). (e1, ..., en)[*])
-}
desugarLetRecToFixEncoded :: (TVarMap t, VarMap t e) -> CheckedExpr -> F.Expr t e
desugarLetRecToFixEncoded (d,g) = go
  where
    go (L _ (LetOut Rec bs@(_:_) e)) =
      F.App
          (F.Lam "_"
              (F.Fun (F.JClass "java.lang.Integer") (transType d tupled_ts))
              (\y -> desugarExpr (d, g' y `Map.union` g) e))
          (F.Fix "_" "_"
              -- Names ignored. Unused.
              (\y _dummy -> desugarExpr (d, g' y `Map.union` g) tupled_es)
              (F.JClass "java.lang.Integer")
              (transType d tupled_ts))
              where
                (fs, ts, es) = unzip3 bs

                tupled_es = noLoc $ Tuple es
                tupled_ts = Product ts

                -- Substitution: fi -> (y 0)._(i-1)
                g' y = Map.fromList $ -- `Map.fromList` is right-biased.
                         zipWith
                             -- TODO: better var name
                           (\f i -> (f, F.Proj i (F.App (F.Var f y) (F.Lit (Int 0)))))
                           fs
                           [1..length bs]
    go _ = panic "desugarLetRecEncode"

-- Convert from: LetOut RecFlag [(Name, Type, CheckedExpr)] (CheckedExpr)
-- To:           LetRec [Type t] ([e] -> [Expr t e]) ([e] -> Expr t e)
desugarLetRecToLetRec :: (TVarMap t, VarMap t e) -> CheckedExpr -> F.Expr t e
desugarLetRecToLetRec (d,g) (L _ (LetOut Rec binds@(_:_) body)) = F.LetRec names' sigs' binds' body'
  where
    (ids, sigs, defs) = unzip3 binds
    names'            = ids
    sigs'             = map (transType d) sigs
    binds' ids'       = map (desugarExpr (d, zipWith (\f f' -> (f, F.Var f f')) ids ids' `addToVarMap` g)) defs
    body'  ids'       = desugarExpr (d, zipWith (\f f' -> (f, F.Var f f')) ids ids' `addToVarMap` g) body

desugarLetRecToLetRec _ _ = panic "desugarLetRecToLetRec"

addToVarMap :: [(ReaderId, F.Expr t e)] -> VarMap t e -> VarMap t e
addToVarMap xs var_map = foldr (\(x,x') acc -> Map.insert x x' acc) var_map xs

addToTVarMap :: [(ReaderId, t)] -> TVarMap t -> TVarMap t
addToTVarMap xs tvar_map = foldr (\(x,x') acc -> Map.insert x x' acc) tvar_map xs
