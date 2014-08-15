{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-unused-matches #-}
{-# LANGUAGE FlexibleInstances, ExplicitForAll #-}

module SystemFI.Syntax
  ( Type(..)
  , Expr(..)
  , TypeContext
  , ValueContext
  , structEqType
  , fsubstTT
  , fsubstTE
  , fsubstEE
  ) where

import PrettyUtils

import Text.PrettyPrint.Leijen

import qualified Data.Set as Set
import qualified Data.Map as Map

data Type a
  = TVar a                -- a
  | Int                   -- Int
  | Forall (a -> Type a)  -- ∀a. t
  | Fun (Type a) (Type a) -- t1 → t2
  | And (Type a) (Type a) -- t1 & t2

data Expr t e
  = Var e                        -- x
  | Lit Integer                  -- i
  | BLam (t -> Expr t e)         -- Λa. e
  | Lam (Type t) (e -> Expr t e) -- λ(x : t). e
  | TApp (Expr t e) (Type t )    -- e t
  | App (Expr t e) (Expr t e)    -- e1 e2
  | Merge (Expr t e) (Expr t e)  -- e1 ,, e2

type TypeContext t    = Set.Set t
type ValueContext t e = Map.Map e (Type t)

-- Structural equality of types

structEqType :: Type Int -> Type Int -> Bool
structEqType = structEqType' 0

structEqType' :: Int -> Type Int -> Type Int -> Bool
structEqType' i (TVar a)    (TVar b)    = a == b
structEqType' i  Int         Int        = True
structEqType' i (Forall f)  (Forall g)  = structEqType' i (f i) (g i)
structEqType' i (Fun s1 s2) (Fun t1 t2) = structEqType' i s1 t1 && structEqType' i s2 t2
structEqType' i (And s1 s2) (And t1 t2) = structEqType' i s1 t1 && structEqType' i s2 t2
structEqType' i  _           _          = False

-- Prettyprinting of types

instance Show (Type Int) where
  show = show . pretty

instance Pretty (Type Int) where
  pretty = prettyType basePrecEnv 0

prettyType :: PrecedenceEnv -> Int -> Type Int -> Doc
prettyType p i (TVar a)    = text (tvar a)
prettyType p i  Int        = text "Int"
prettyType p i (Forall f)  = parensIf p 1 (char '∀' <+> text (tvar i) <> dot <+>
                                           prettyType (1,PrecMinus) (succ i) (f i))
prettyType p i (Fun t1 t2) = parensIf p 2 (prettyType (2,PrecPlus) i t1 <+> char '→' <+> prettyType (2,PrecMinus) i t2)
prettyType p i (And t1 t2) = parensIf p 2 (prettyType (2,PrecMinus) i t1 <+> text "&"  <+> prettyType (2,PrecPlus) i t2)

-- Prettyprinting of expressions

instance Show (Expr Int Int) where
  show = show . pretty

instance Pretty (Expr Int Int) where
  pretty = prettyExpr basePrecEnv (0, 0)

prettyExpr :: PrecedenceEnv -> (Int, Int) -> Expr Int Int -> Doc
prettyExpr p (i,j) (Var x)       = text (var x)
prettyExpr p (i,j) (Lit n)       = integer n
prettyExpr p (i,j) (BLam f)      = parensIf p 1 (char 'Λ' <+> text (tvar i) <> dot <+>
                                                 prettyExpr (1,PrecMinus) (succ i, j) (f i))
prettyExpr p (i,j) (Lam t f)     = parensIf p 1 (char 'λ' <+>
                                                 parens (text (var j) <+> colon <+> prettyType basePrecEnv i t) <>
                                                 dot <+>
                                                 prettyExpr (1,PrecMinus) (i, succ j) (f j))
prettyExpr p (i,j) (Merge e1 e2) = parensIf p 2 (prettyExpr (2,PrecMinus) (i,j) e1 <+>
                                                 text ",," <+>
                                                 prettyExpr (2,PrecPlus) (i,j) e2)
prettyExpr p (i,j) (TApp e t)    = parensIf p 3 (prettyExpr (3,PrecMinus) (i,j) e  <+> prettyType (3,PrecPlus) i t)
prettyExpr p (i,j) (App e1 e2)   = parensIf p 3 (prettyExpr (3,PrecMinus) (i,j) e1 <+> prettyExpr (3,PrecPlus) (i,j) e2)

fsubstTT :: (Int, Type Int) -> Type Int -> Type Int
fsubstTT (x,r) (TVar a)
  | a == x                 = r
  | otherwise              = TVar a
fsubstTT (x,r)  Int        = Int
fsubstTT (x,r) (Forall f)  = Forall (fsubstTT (x,r) . f)
fsubstTT (x,r) (Fun t1 t2) = Fun (fsubstTT (x,r) t1) (fsubstTT (x,r) t2)
fsubstTT (x,r) (And t1 t2) = And (fsubstTT (x,r) t1) (fsubstTT (x,r) t2)

fsubstTE :: (Int, Type Int) -> Expr Int Int -> Expr Int Int
fsubstTE (x,r) (Var a)       = Var a
fsubstTE (x,r) (Lit n)       = Lit n
fsubstTE (x,r) (BLam g)      = BLam (fsubstTE (x,r) . g)
fsubstTE (x,r) (Lam t f)     = Lam (fsubstTT (x,r) t) (fsubstTE (x,r) . f)
fsubstTE (x,r) (TApp e t)    = TApp (fsubstTE (x,r) e) (fsubstTT (x,r) t)
fsubstTE (x,r) (App e1 e2)   = App (fsubstTE (x,r) e1) (fsubstTE (x,r) e2)
fsubstTE (x,r) (Merge e1 e2) = Merge (fsubstTE (x,r) e1) (fsubstTE (x,r) e2)

fsubstEE :: (Int, Expr Int Int) -> Expr Int Int -> Expr Int Int
fsubstEE (x,r) (Var a)
  | a == x                   = r
  | otherwise                = Var a
fsubstEE (x,r) (Lit n)       = Lit n
fsubstEE (x,r) (BLam g)      = BLam (fsubstEE (x,r) . g)
fsubstEE (x,r) (Lam t f)     = Lam t (fsubstEE (x,r) . f)
fsubstEE (x,r) (TApp e t)    = TApp (fsubstEE (x,r) e) t
fsubstEE (x,r) (App e1 e2)   = App (fsubstEE (x,r) e1) (fsubstEE (x,r) e2)
fsubstEE (x,r) (Merge e1 e2) = Merge (fsubstEE (x,r) e1) (fsubstEE (x,r) e2)