-- The desugarer: turning the source language into System F (with intersection types).

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Desugar (desugar) where

import Src

import qualified Core as C

import Panic

import Data.Maybe       (fromMaybe, fromJust)
import Data.List        (intercalate)
import StringPrefixes
import Control.Monad.State.Lazy

import qualified Data.Map as Map

desugar :: Expr (Name,Type) -> C.Expr t e
desugar = desugarExpr (Map.empty, Map.empty)

type TVarMap t  = Map.Map Name t
type VarMap t e = Map.Map Name (C.Expr t e)

transType :: TVarMap t -> Type -> C.Type t
transType d (TVar a)     = C.TVar (fromMaybe (panic ("Desugar.transType: " ++ show (TVar a))) (Map.lookup a d))
transType _ (JType (JClass c))   = C.JClass c
transType _ (JType (JPrim c))   = C.JClass c
transType d (Fun t1 t2)  = C.Fun (transType d t1) (transType d t2)
transType d (Product ts) = C.Product (map (transType d) ts)
transType d (Forall a t) = C.Forall (\a' -> transType (Map.insert a a' d) t)
transType d (And t1 t2)  = C.And (transType d t1) (transType d t2)
transType d (Record fs)  =
                case fs  of
                  [(l,t)]  -> C.Record (l, transType d t)
                  _        -> transType d (Record (take (length fs - 1) fs)) `C.And` C.Record (let (l,t) = last fs in (l,transType d t))
transType _ Unit         = C.Unit
transType i (Thunk t)    = C.Thunk (transType i t)
transType d (Datatype n nts) = C.Datatype n (zip ns ts')
    where (ns, ts) = unzip nts
          ts' = map (transType d) ts

desugarExpr :: (TVarMap t, VarMap t e) -> Expr (Name,Type) -> C.Expr t e
desugarExpr (d, g) = go
  where
    go (Var (x,_t))      = fromMaybe (panic "Desugar.desugarExpr: Var") (Map.lookup x g)
    go (Lit lit)         = C.Lit lit
    go (App e1 e2)       = C.App (go e1) (go e2)
    go (TApp e t)        = C.TApp (go e) (transType d t)
    go (Tuple es)        = C.Tuple (map go es)
    go (Proj e i)        = C.Proj i (go e)
    go (PrimOp e1 op e2) = C.PrimOp (go e1) op (go e2)
    go (If e1 e2 e3)     = C.If (go e1) (go e2) (go e3)
    go (Lam (x, t) e)    = C.Lam x
                               (transType d t)
                               (\x' -> desugarExpr (d, Map.insert x (C.Var x x') g) e)
    go (BLam a e)        = C.BLam (\a' -> desugarExpr (Map.insert a a' d, g) e)
    go Let{..}           = panic "Desugar.desugarExpr: Let"
    go (LetOut _ [] e)   = go e
    go (Merge e1 e2)     = C.Merge (go e1) (go e2)
    go (RecordLit fs)       =
      case fs of
        []       -> panic "Desugar.desugarExpr: Record"
        [(l,e)]  -> C.RecordLit (l, go e)
        _        -> go (RecordLit (take (length fs - 1) fs)) `C.Merge` C.RecordLit (let (l,e) = last fs in (l,go e))
    go (RecordAccess e l) = C.RecordElim (go e) l
    go (RecordUpdate e fs) =
      case fs of
        [] -> go e
        _  -> C.RecordUpdate (go (RecordUpdate e (take (length fs - 1) fs))) (let (l1,e1) = last fs in (l1, go e1))

    go (LetOut NonRec [(f1, _, e1)] e) =
      -- C.App
      --   (C.Lam (transType d t1) (\f1' -> desugarExpr (d, Map.insert f1 (C.Var f1') g) e))
      --   (go e1)
      C.Let f1 (go e1) (\f1' -> desugarExpr (d, Map.insert f1 (C.Var f1 f1') g) e)

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
      C.Let (intercalate "_" fs)
        (go tupled_es)
        (\y -> desugarExpr (d, g' y `Map.union` g) e)
        where
          (fs, _, es)  = unzip3 bs

          tupled_es = Tuple es

          -- Substitution: fi -> y._(i-1)
          g' y = Map.fromList $
                   zipWith (\f i -> (f, C.Proj i (C.Var f y)))
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
    go (LetOut Rec [(f,t@(Fun _ _),e)] body) = desugarLetRecToFix (d,g) (LetOut Rec [(f,t,e)] body)
    go (LetOut Rec [(f,t,e)] body)           = desugarLetRecToLetRec (d,g) (LetOut Rec [(f,t,e)] body)
    go (LetOut Rec bs body)                  = desugarLetRecToLetRec (d,g) (LetOut Rec bs body)
    go (JNew c args)          = C.JNew c (map go args)
    go (JMethod callee m args r) = C.JMethod (fmap go callee) m (map go args) r
    go (JField  callee f r)      = C.JField  (fmap go callee) f r

    -- Non Java Class translation
    -- go (PrimList l)              = FPrimList (map go l)

    -- Primitive List to java class

    go (PrimList l)              = case l of     -- translate to java new obj
                                     []   -> C.JNew (namespace ++ "FunctionalList") []
                                     x:xs -> C.JNew (namespace ++ "FunctionalList") [go x, go (PrimList xs)]

    go (Seq es) = C.Seq (map go es)
    -- go (Data n cs e) =
    --     let names = map (\(Constructor s _) -> s) cs
    --         dt = C.Datatype n names
    --         c =
    --     in desugarExpr (Map.insert (n)d, g `Map.union` Map.fromList ) e
    go (Constr n es) = C.Constr n (map go es)

    go (Case e alts) = C.Case (go e) (map desugarAlts alts)

    desugarAlts (ConstrAlt n ns e) = C.ConstrAlt n ns (\es -> desugarExpr (d, g `Map.union` Map.fromList (map (\(x,x') -> (x, C.Var x x')) (zip ns es))) e)


desugarLetRecToFix :: (TVarMap t, VarMap t e) -> Expr (Name,Type) -> C.Expr t e
desugarLetRecToFix (d,g) (LetOut Rec [(f,t,e)] body) =
  C.App
      (C.Lam "_"
          (transType d t)
          (\f' -> desugarExpr (d, addToEnv [(f,C.Var f f')] g) body))
      (C.Fix
          (\f' x1' -> desugarExpr (d, addToEnv [(f, C.Var f f'), (x1, C.Var x1 x1')] g) peeled_e)
          (transType d t1)
          (transType d t2))
          where
            addToEnv binds g = foldr (\(x,x') acc -> Map.insert x x' acc) g binds -- TODO: subsumed
            (Just (x1, t1), t2, peeled_e) = peel Nothing (e,t)
              where
                peel Nothing (Lam (x1,t1) e, Fun _ t) = (Just (x1,t1), t, e)
                peel _ _ = panic "Desugar.desugarLetRecToFix: not a function"
desugarLetRecToFix _ _ = panic "Desugar.desugarLetRecToFix"

{-
    let rec f1 = e1, ..., fn = en in e
~>  let y = fix y (dummy : Int) : (t1, ..., tn). (e1, ..., en)[*] in e[*]
~>  (\(y : Int -> (t1, ..., tn)). e[*])
      (fix y (dummy : Int) : (t1, ..., tn). (e1, ..., en)[*])
-}
desugarLetRecToFixEncoded :: (TVarMap t, VarMap t e) -> Expr (Name,Type) -> C.Expr t e
desugarLetRecToFixEncoded (d,g) = go
  where
    go (LetOut Rec bs@(_:_) e) =
      C.App
          (C.Lam "_"
              (C.Fun (C.JClass "java.lang.Integer") (transType d tupled_ts))
              (\y -> desugarExpr (d, g' y `Map.union` g) e))
          (C.Fix
              (\y _dummy -> desugarExpr (d, g' y `Map.union` g) tupled_es)
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
                           (\f i -> (f, C.Proj i (C.App (C.Var f y) (C.Lit (Int 0)))))
                           fs
                           [1..length bs]
    go _ = panic "Desugar.desugarLetRecEncode"

-- Convert from: LetOut RecFlag [(Name, Type, Expr (Name,Type))] (Expr (Name,Type))
-- To:           LetRec [Type t] ([e] -> [Expr t e]) ([e] -> Expr t e)
desugarLetRecToLetRec :: (TVarMap t, VarMap t e) -> Expr (Name,Type) -> C.Expr t e
desugarLetRecToLetRec (d,g) (LetOut Rec binds@(_:_) body) = C.LetRec sigs' binds' body'
  where
    (ids, sigs, defs) = unzip3 binds
    sigs'  = map (transType d) sigs
    binds' ids' = map (desugarExpr (d, zipWith (\f f' -> (f, C.Var f f')) ids ids' `addToVarMap` g)) defs
    body'  ids' = desugarExpr (d, zipWith (\f f' -> (f, C.Var f f')) ids ids' `addToVarMap` g) body

desugarLetRecToLetRec _ _ = panic "Desugar.desugarLetRecToLetRec"

addToVarMap :: [(Name, C.Expr t e)] -> VarMap t e -> VarMap t e
addToVarMap xs var_map = foldr (\(x,x') acc -> Map.insert x x' acc) var_map xs

-- data Supply = Supply {dataSupply :: [Int], constrSupply :: [Int]}
-- type DatatypesM = State Supply

-- newDataID :: DatatypesM Int
-- newDataID = state $ \s -> case dataSupply s of
--                             [] -> panic "data names exhausted"
--                             (x:xs) -> (x, s{dataSupply = xs})

-- newConstrID :: DatatypesM Int
-- newConstrID = state $ \s -> case constrSupply s of
--                             [] -> panic "constructor names exhausted"
--                             (x:xs) -> (x, s{constrSupply = xs})

-- newData :: [(Name, [Type t])] -> DatatypesM (Type t, [Constructor t])
-- newData cs =
--     do name <- newDataID
--        names <- mapM
