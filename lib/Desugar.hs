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


{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Desugar (desugar) where

import           Panic
import           Src
import           SrcLoc
import           StringPrefixes
import qualified SystemFI as F

import           Data.List (intercalate)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set (member)
import           Text.PrettyPrint.ANSI.Leijen


-- | Generate a fresh variable in the context. This is a very general API and
-- requires caution from the user, that is, it is the responsibility of the user
-- to ensure that the fresh variable is inserted to VarMap in the ensuing code.
-- For example,
--
--     `Map.insert x (F.Var x x') g`
-- where x' is a bound *Haskell* variable.
--
-- Example:
-- If you want a fresh variable at the position denoted by "?" in the following
-- program, ``\x. \y. \?. e` with the current `VarMap` being `g`, then you can
-- write `let x = generateFreshVar g in ...` so that inside `...` you have
-- the fresh variable `x` as well as the updated `VarMap` `g1`.
--
-- The generated variable is guaranteed to be distinct:
-- 1) with all variables introduced at the outer scope (therefore
-- in this case, "?" will be neither "x" not "y"); and
-- 2) with all free variables of `e`, so that "?" will not accidently capture them.
--
-- A note on the implementation: For the sake of performance & convenience,
-- we do not compute the free variables of `e`, since at the stage of this
-- module, we can assume that `e` does not contain free variables that are not
-- captured by the outer scope. In other words, FV(e) \ FV(Γ) is empty.
generateFreshVar :: VarMap t e -> Name
generateFreshVar g0 = try g0 1
  where
    try :: VarMap t e -> Int -> Name
    try g n
      | fresh_var n `Set.member` Map.keysSet g = try g (n + 1)
      | otherwise                              = fresh_var n
    fresh_var n = "fresh" ++ show n

generateNFreshVar :: VarMap t e -> Int -> [Name]
generateNFreshVar g0 num0 = try g0 num0 1
  where
    try :: VarMap t e -> Int -> Int -> [Name]
    try g num n
      | num == 0                               = []
      | fresh_var n `Set.member` Map.keysSet g = try g num (n + 1)
      | otherwise                              = (fresh_var n) : (try g (num - 1) (n + 1))
    fresh_var n = "fresh" ++ show n

desugar :: CheckedExpr -> F.Expr t e
desugar = desugarExpr (Map.empty, Map.empty)

type TVarMap t  = Map.Map ReadId t
type VarMap t e = Map.Map ReadId (F.Expr t e)

transType :: TVarMap t -> ReadType -> F.Type t
transType d (TVar a)     = F.TVar a (fromMaybe (panic ("transType: " ++ show (TVar a))) (Map.lookup a d))
transType _ (JClass c)   = F.JClass c
transType d (Fun t1 t2)  = F.Fun (transType d t1) (transType d t2)
transType d (TupleType ts) = F.Product (map (transType d) ts)
transType d (Forall a t) = F.Forall a (\a' -> transType (Map.insert a a' d) t)
transType d (And t1 t2)  = F.And (transType d t1) (transType d t2)
transType d (RecordType fs)  =
                case fs  of
                  [(l,t)]  -> F.RecordType (l, transType d t)
                  _        -> transType d (RecordType (take (length fs - 1) fs)) `F.And` F.RecordType (let (l,t) = last fs in (l,transType d t))
transType _ Unit         = F.Unit
transType i (Datatype n ts ns) = F.Datatype n (map (transType i) ts) ns
transType _ t            = prettySorry "transType" (pretty t)


desugarExpr :: (TVarMap t, VarMap t e) -> CheckedExpr -> F.Expr t e
desugarExpr (d, g) = go
  where
    go (L _ (Var (x,t)))       = fromMaybe (panic "desugarExpr: Var") (Map.lookup x g)
    go (L _ (Lit lit))         = F.Lit lit
    go (L _ (App f x))         = F.App (go f) (go x)
    go (L _ (TApp e t))        = F.TApp (go e) (transType d t)
    go (L _ (TupleCon es))     = F.Tuple (map go es)
    go (L _ (TupleProj e i))   = F.Proj i (go e)
    go (L _ (PrimOp e1 op e2)) = F.PrimOp (go e1) op (go e2)
    go (L _ (If e1 e2 e3))     = F.If (go e1) (go e2) (go e3)
    go (L _ (Lam (x, t) e))    = F.Lam x
                                 (transType d t)
                                 (\x' -> desugarExpr (d, Map.insert x (F.Var x x') g) e)
    go (L _ (BLam a e))        = F.BLam a (\a' -> desugarExpr (Map.insert a a' d, g) e)
    go (L _ LetIn{})         = panic "desugarExpr: LetIn"
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

          tupled_es = noLoc $ TupleCon es

          -- Substitution: fi -> y._(i-1)
          g' y = Map.fromList $ -- `Map.fromList` is right-biased.
                   zipWith (\f i -> (f, F.Proj i (F.Var f y)))
                           fs
                           [1..length bs]

    go (L _ EModule{})                 = panic "desugarExpr: Module"


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
    go (L _ (JField  callee f r))      = F.JField  (fmap go callee) f (transType d r)

    -- Non Java Class translation
    -- go (PrimList l)              = FPrimList (map go l)

    -- Primitive List to java class

    go (L _ (Error ty str))            = F.Error (transType d ty) (go str)

    go (L _ (Seq es)) = F.Seq (map go es)
    go (L _ (Data recflag databinds e)) = F.Data recflag (map (desugarDatabind d) databinds) (go e)

    go (L _ (ConstrOut c es)) = F.ConstrOut (desugarConstructor d c) (map go es)
    go (L _ (Case e alts)) = decisionTree (d,g) [e] pats exprs (replicate (length alts) [])
                     where pats = [ [pat] | ConstrAlt pat _ <- alts]
                           exprs = [ expr | ConstrAlt _ expr <- alts]
    go (L _ (CaseString e alts)) =
            let emptytest = noLoc $ JMethod (NonStatic e) "isEmpty" [] "java.lang.Boolean"
                [ConstrAlt _ b1, ConstrAlt (PConstr _ [sub1, sub2]) b2] = alts
                headfetch = noLoc $ JMethod (NonStatic e) "charAt" [noLoc $ Lit (Int 0)] "java.lang.Character"
                tailfetch = noLoc $ JMethod (NonStatic e) "substring" [noLoc $ Lit (Int 1)] "java.lang.String"
                b2'  = case sub1 of PVar nam _ -> noLoc $ LetOut NonRec [(nam, (JClass "java.lang.Character"), headfetch)] b2
                                    _ -> b2
                b2'' = case sub2 of PVar nam _ -> noLoc $ LetOut NonRec [(nam, (JClass "java.lang.String"), tailfetch)] b2'
                                    _ -> b2'
            in
            go (noLoc $ If emptytest b1 b2'')

    go (L _ (EModuleOut pname binds)) = F.Module pname (desugarBindsToDefs (d, g) binds)


desugarBindsToDefs :: (TVarMap t, VarMap t e) -> [Definition] -> F.Definition t e
desugarBindsToDefs _ [] = F.Null
desugarBindsToDefs (d, g) (b:bs) =
  case b of
    Def (n, t, e) -> F.Def n (t, transType d t) (desugarExpr (d, g) e)
                       (\f -> desugarBindsToDefs (d, Map.insert n (F.Var n f) g) bs)
    -- at least one element
    DefRec binds -> F.DefRec names types exprs def
      where (ids, sigs, defs) = unzip3 binds
            names = ids
            types = map (\t -> (t, transType d t)) sigs
            exprs ids' = map (desugarExpr (d, zipWith (\f f' -> (f, F.Var f f')) ids ids' `addToVarMap` g)) defs
            def ids' = desugarBindsToDefs (d, zipWith (\f f' -> (f, F.Var f f')) ids ids' `addToVarMap` g) bs

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

                tupled_es = noLoc $ TupleCon es
                tupled_ts = TupleType ts

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

-- DataBind -> F.DataBind
desugarDatabind ::  TVarMap t -> DataBind -> F.DataBind t
desugarDatabind d = go
  where go (DataBind n params ctrs) =
               F.DataBind n params (\t ->
                  let d' = addToTVarMap (zip params t) d
                  in  map (desugarConstructor d') ctrs
               )

-- Constructor -> F.Constructor
desugarConstructor :: TVarMap t -> Constructor -> F.Constructor t
desugarConstructor d' = go
  where go (Constructor n ts) = F.Constructor n (map (transType d') ts)

{-
 - given: case var of Cons x (Cons y z) -> expr
 - result: case var of
 -              Cons p q ->
 -                  case q of
 -                      Cons r s ->
 -                          let z = s
 -                          let y = r
 -                          let x = p
 -}

decisionTree ::  (TVarMap t, VarMap t e) -> [CheckedExpr] -> [[Pattern]] -> [CheckedExpr] -> [[(Name, Type, LExpr (Name,Type) Type)]] -> F.Expr t e
decisionTree = go
  where go (d,g) exprs pats branches binds
            -- first pattern matches
          | all isWildcardOrVar (head pats) =
                let bind' = [(nam, ty, e) | (e, PVar nam ty) <- zip exprs (head pats)] ++ head binds
                in desugarExpr (d,g) $ noLoc $ LetOut NonRec bind' (head branches)
          | all isWildcardOrVar (map head pats) =
                let newbind (PVar nam ty:_) bind = (nam, ty, curExp):bind
                    newbind _               bind =  bind
                in go (d,g) (tail exprs) (map tail pats) branches (zipWith newbind pats binds)
          | null missingconstrs = F.Case (desugarExpr (d,g) curExp) (map makeNewAltFromCtr appearconstrs)
          | otherwise           = F.Case (desugarExpr (d,g) curExp) (map makeNewAltFromCtr appearconstrs ++ [makeDefault])
          where curExp = head exprs
                (appearconstrs, missingconstrs) = constrInfo (map head pats)
                makeNewAltFromCtr ctr@(Constructor name params) = F.ConstrAlt (desugarConstructor d ctr) freshes result
                     where len = length params - 1
                           freshes = generateNFreshVar g len
                           iterate (curPattern:rest, branch, bind) acc =
                              let nextPats = rest ++ (extractorsubpattern name len curPattern)
                              in  case curPattern of
                                    PWildcard -> ( nextPats, branch, bind) : acc
                                    PVar nam ty -> (nextPats, branch, (nam, ty, curExp) : bind) : acc
                                    PConstr (Constructor nam _) _
                                        | nam /= name -> acc
                                        | otherwise -> (nextPats, branch, bind):acc
                           result es =
                              let g' = zipWith (\fresh e -> (fresh, F.Var fresh e)) freshes es `addToVarMap` g
                                  exprs' = tail exprs ++ (zipWith (\fresh ty -> noLoc $ Var (fresh, ty)) freshes (init params))
                                  (pats', branches', binds') = unzip3 $ foldr iterate [] (zip3 pats branches binds)
                              in go (d, g') exprs' pats' branches' binds'
                makeDefault = F.Default $ go (d, g) (tail exprs) pats' branches' binds'
                     where  (pats', branches', binds') = unzip3 $ foldr iterate [] (zip3 pats branches binds)
                            iterate (curPattern:rest, branch, bind) acc =
                                 case curPattern of
                                    PWildcard -> (rest , branch, bind) :acc
                                    PVar nam ty -> (rest, branch, (nam, ty, curExp): bind) :acc
                                    _ -> acc

addToVarMap :: [(ReadId, F.Expr t e)] -> VarMap t e -> VarMap t e
addToVarMap xs var_map = foldr (\(x,x') acc -> Map.insert x x' acc) var_map xs

addToTVarMap :: [(ReadId, t)] -> TVarMap t -> TVarMap t
addToTVarMap xs tvar_map = foldr (\(x,x') acc -> Map.insert x x' acc) tvar_map xs
