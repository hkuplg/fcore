{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-unused-matches #-}
{-# LANGUAGE FlexibleInstances, GADTs, ExplicitForAll #-}

module SystemFI.Syntax
  ( Type(..)
  , Expr(..)
  , structEqType
  ) where

import PrettyUtils

import Text.PrettyPrint.Leijen

data Type a where
  TVar   :: a -> Type a                -- a
  Int    :: Type a                     -- Int
  Forall :: (a -> Type a) -> Type a    -- forall a. t
  Fun    :: Type a -> Type a -> Type a -- t1 -> t2
  And    :: Type a -> Type a -> Type a -- t1 & t2

data Expr t e where
  Var   :: e -> Expr t e                         -- x
  Lit   :: Integer -> Expr t e                   -- i
  Lam   :: Type t -> (e -> Expr t e) -> Expr t e -- \(x : t). e
  BLam  :: (t -> Expr t e) -> Expr t e           -- /\a. e
  TApp  :: Expr t e -> Type t -> Expr t e        -- e t
  App   :: Expr t e -> Expr t e -> Expr t e      -- e1 e2
  Merge :: Expr t e -> Expr t e -> Expr t e      -- e1 ,, e2

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
prettyType _ _ (TVar a)    = text (tvar a)
prettyType _ _  Int        = text "Int"
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