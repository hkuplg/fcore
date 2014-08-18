{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module SystemF.Syntax
    ( PFTyp(..)
    , PFExp(..)
    , PrimLit
    , alphaEqTyp
    , fsubstTT
    , fsubstTE
    , fsubstEE
    ) where

import PrettyUtils
import ESF.Syntax (Operator(..),Lit(..))

import Text.PrettyPrint.Leijen

data PFTyp t
  = FTVar t
  | FForall (t -> PFTyp t)
  | FFun (PFTyp t) (PFTyp t)
  | FJClass String
  | FProduct [PFTyp t]

data PFExp t e
  = FVar String e
  | FBLam          (t -> PFExp t e)
  | FLam (PFTyp t) (e -> PFExp t e)
  | FTApp (PFExp t e) (PFTyp t)
  | FApp  (PFExp t e) (PFExp t e)
  | FPrimOp (PFExp t e) Operator (PFExp t e) -- SystemF extension from: https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf (no int restriction)
  | FLit Lit
  | FIf (PFExp t e) (PFExp t e) (PFExp t e)
  | FTuple [PFExp t e]
  | FProj Int (PFExp t e)

    -- fix x (x1 : t1) : t2. e, or the new syntax:
    -- fix (x : t1 -> t2). \x1. e
  | FFix (e -> e -> PFExp t e)
         (PFTyp t) -- t1
         (PFTyp t) -- t2
-- Java
  | FJNewObj String [PFExp t e]
  | FJMethod (PFExp t e) String [PFExp t e] (Maybe String)

newtype Typ = HideTyp { revealTyp :: forall t. PFTyp t } -- type of closed types

type PrimLit = Integer -- later maybe Bool | Char

newtype Exp = HideExp { revealExp :: forall t e. PFExp t e }

alphaEqTyp :: PFTyp Int -> PFTyp Int -> Bool
alphaEqTyp = alphaEqTyp' 0

alphaEqTyp' :: Int -> PFTyp Int -> PFTyp Int -> Bool

alphaEqTyp' i (FTVar x)     (FTVar y)    = x == y
alphaEqTyp' i (FJClass c1)  (FJClass c2) = c1 == c2
alphaEqTyp' i (FForall f)   (FForall g)  = alphaEqTyp' (i + 1) (f i) (g i)
alphaEqTyp' i (FFun s1 s2)  (FFun t1 t2) = alphaEqTyp' i s1 t1 && alphaEqTyp' i s2 t2
alphaEqTyp' i (FProduct ss) (FProduct ts)
  | length ss == length ts               = uncurry (alphaEqTyp' i) `all` zip ss ts
  | otherwise                            = False
alphaEqTyp' i  _             _           = False

-- Prettyprinting of types

instance Show (PFTyp Int) where
  show = show . pretty

instance Pretty (PFTyp Int) where
  pretty = prettyTyp basePrecEnv 0

prettyTyp :: PrecedenceEnv -> Int -> PFTyp Int -> Doc
prettyTyp p i (FTVar a)     = text (nameTVar a)
prettyTyp p i (FJClass a)   = text a
prettyTyp p i (FForall f)   = parensIf p 1 (char '∀' <+> text (nameTVar i) <> dot <+> prettyTyp (1,PrecMinus) (succ i) (f i))
prettyTyp p i (FFun t1 t2)  = parensIf p 2 (prettyTyp (2,PrecPlus) i t1 <+> char '→' <+> prettyTyp (2,PrecMinus) i t2)
prettyTyp p i (FProduct ts) = tupled (map (prettyTyp basePrecEnv i) ts)

-- Prettyprinting of expressions

instance Show (PFExp Int Int) where
  show = show . pretty

instance Pretty (PFExp Int Int) where
  pretty = prettyExp basePrecEnv (0, 0)

prettyExp :: PrecedenceEnv -> (Int, Int) -> PFExp Int Int -> Doc
prettyExp p (i,j) (FVar _ x)      = text (nameVar x)
prettyExp p (i,j) (FLit (Integer n)) = integer n
prettyExp p (i,j) (FLit (String s))  = string s
prettyExp p (i,j) (FLit (Boolean b)) = bool b
prettyExp p (i,j) (FTuple es)     = tupled (map (prettyExp basePrecEnv (i,j)) es)
prettyExp p (i,j) (FIf e1 e2 e3)  = parensIf p 1 (text "if" <+> prettyExp (1,PrecMinus) (i,j) e1 <+>
                                                   indent 2 (text "then" <+> prettyExp (1, PrecMinus) (i,j) e2 <+>
                                                             text "else" <+> prettyExp (1, PrecMinus) (i,j) e3))
prettyExp p (i,j) (FBLam f)       = parensIf p 2 (char 'Λ' <+> text (nameTVar i) <> dot <+>
                                                  prettyExp (2,PrecMinus) (succ i, j) (f i))
prettyExp p (i,j) (FLam t f)      = parensIf p 2 (char 'λ' <+>
                                                  parens (text (nameVar j) <+> colon <+> prettyTyp basePrecEnv i t) <>
                                                  dot <+>
                                                  prettyExp (2,PrecMinus) (i, succ j) (f j))
prettyExp p (i,j) (FTApp e t)     = parensIf p 4 (prettyExp (4,PrecMinus) (i,j) e  <+> prettyTyp (4,PrecPlus) i t)
prettyExp p (i,j) (FApp e1 e2)    = parensIf p 4 (prettyExp (4,PrecMinus) (i,j) e1 <+> prettyExp (4,PrecPlus) (i,j) e2)
prettyExp p i (FProj n e)         = parensIf p 5 (prettyExp (5,PrecMinus) i e) <> dot <> char '_' <> int n

-- Substitutions

fsubstEE :: (Int, PFExp Int Int) -> PFExp Int Int -> PFExp Int Int
fsubstEE (x,r) = go
    where go (FVar s x')
            | x' == x            = r
            | otherwise          = FVar s x'
          go (FLit n)            = FLit n
          go (FBLam f)           = FBLam (go . f)
          go (FLam t' f)         = FLam t' (go . f)
          go (FApp e1 e2)        = FApp (go e1) (go e2)
          go (FPrimOp e1 op e2)  = FPrimOp (go e1) op (go e2)
          go (FIf e1 e2 e3)      = FIf (go e1) (go e2) (go e3)
          go (FTuple es)         = FTuple (map go es)
          go (FProj i' e)        = FProj i' (go e)
          go (FJNewObj s args)   = FJNewObj s (map go args)
          go (FJMethod c s args ss) = FJMethod (go c) s (map go args) ss

fsubstTT :: (Int, PFTyp Int) -> PFTyp Int -> PFTyp Int
fsubstTT (i,t) (FTVar j)
    | j == i                 = t
    | otherwise              = FTVar j
fsubstTT (i,t) (FJClass c)   = FJClass c
fsubstTT (i,t) (FForall f)   = FForall (fsubstTT (i,t) . f)
fsubstTT (i,t) (FFun t1 t2)  = FFun (fsubstTT (i,t) t1) (fsubstTT (i,t) t2)
fsubstTT (i,t) (FProduct ts) = FProduct (map (fsubstTT (i,t)) ts)

fsubstTE :: (Int, PFTyp Int) -> PFExp Int Int -> PFExp Int Int
fsubstTE (i, t) = go
    where go (FVar s x)          = FVar s x
          go (FLit n)            = FLit n
          go (FBLam f)           = FBLam (go . f)
          go (FLam t' f)         = FLam (fsubstTT (i, t) t') (go . f)
          go (FApp e1 e2)        = FApp (go e1) (go e2)
          go (FPrimOp e1 op e2)  = FPrimOp (go e1) op (go e2)
          go (FIf e1 e2 e3)      = FIf (go e1) (go e2) (go e3)
          go (FTuple es)         = FTuple (map go es)
          go (FProj i' e)        = FProj i' (go e)
          go (FJNewObj s args)   = FJNewObj s (map go args)
          go (FJMethod c s args ss) = FJMethod (go c) s (map go args) ss
