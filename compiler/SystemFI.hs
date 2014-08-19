module SystemFI where

import SystemFI.Syntax
import SystemFI.TypeCheck

import SystemF.Syntax

import PrettyUtils

import Control.Monad.State

{-
*SystemFI> let t = let Forall f = tyTrue in f 0
*SystemFI> t
∀ A. A → A → A
*SystemFI> prettyType basePrecEnv 0 t
∀ A. A → A → A
*SystemFI> prettyType basePrecEnv 1 t
∀ B. A → B → A
*SystemFI> prettyType basePrecEnv 2 t
∀ C. A → C → A
-}

tyNat2Int = Nat `Fun` Int

tyInt2Nat = Int `Fun` Nat

-- Int → Int → Int
tyAdd = Int `Fun` (Int `Fun` Int)

-- (Int → Int) → Int
tyTakeIncrAndReturnInt = (Int `Fun` Int) `Fun` Int

-- ∀ A. A → A
tyIdent = Forall (\a -> TVar a `Fun` TVar a)

-- ∀ A. Int & (A -> A)
tyIdentAnd = Forall (\a -> Int `And` (TVar a `Fun` TVar a))

-- ∀ A. ∀ B. ∀ C. (A → B → C) -> A → B → C
tyIdent3 = Forall (\a -> Forall (\b -> Forall (\c -> (TVar a `Fun` (TVar b `Fun` TVar c)) `Fun` (TVar a `Fun` (TVar b `Fun` TVar c)))))

-- ∀ A. ∀ B. A -> B -> A
tyTrue = Forall (\a -> Forall (\b -> TVar a `Fun` (TVar b `Fun` TVar a)))

tyFalse = Forall (\a -> Forall (\b -> TVar a `Fun` (TVar b `Fun` TVar b)))

-- Int & Int
tyTwoInt = Int `And` Int

ident = BLam (\a -> Lam (TVar a) (\x -> Var x))

-- Λa. Λb. λ(a : A). λ(b : B). a
konst = BLam (\a -> BLam (\b ->
          Lam (TVar a) (\x -> Lam (TVar b) (\y -> Var x))))

lam = Lam Int (\x -> Var x)

app = lam `App` Lit 1

tapp1 = konst `TApp` Int

tapp2 = konst `TApp` tyIdent

tapp3 = tapp2 `TApp` Int

merge1 = Merge (Lit 1) (Lit 2)

merge2 = Merge konst (Lit 1)

e1 = App (Lam Int (\x -> Var x)) merge2

e2 = App (Lam Int (\x -> Var x)) (Merge konst konst)

e3 = App (Lam (Int `And` Int) (\x -> Var x)) (Lit 1)
