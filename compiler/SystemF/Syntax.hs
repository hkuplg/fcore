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
import qualified Language.Java.Syntax as J (Op(..))

data PFTyp t
  = FTVar t
  | FForall (t -> PFTyp t)
  | FFun (PFTyp t) (PFTyp t)
  | FInt
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
  | FJMethod (PFExp t e) String [PFExp t e]

newtype Typ = HideTyp { revealTyp :: forall t. PFTyp t } -- type of closed types

type PrimLit = Integer -- later maybe Bool | Char

newtype Exp = HideExp { revealExp :: forall t e. PFExp t e }

alphaEqTyp :: PFTyp Int -> PFTyp Int -> Bool
alphaEqTyp = alphaEqTyp' 0

alphaEqTyp' :: Int -> PFTyp Int -> PFTyp Int -> Bool

alphaEqTyp' i (FTVar x)     (FTVar y)    = x == y
alphaEqTyp' i  FInt          FInt        = True
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
prettyTyp p i (FTVar a)     = text (tvar a)
prettyTyp p i  FInt         = text "Int"
prettyTyp p i (FForall f)   = parensIf p 1 (char '∀' <+> text (tvar i) <> dot <+> prettyTyp (1,PrecMinus) (succ i) (f i))
prettyTyp p i (FFun t1 t2)  = parensIf p 2 (prettyTyp (2,PrecPlus) i t1 <+> char '→' <+> prettyTyp (2,PrecMinus) i t2)
prettyTyp p i (FProduct ts) = tupled (map (prettyTyp basePrecEnv i) ts)

-- Prettyprinting of expressions

instance Show (PFExp Int Int) where
  show = show . pretty

instance Pretty (PFExp Int Int) where
  pretty = prettyExp basePrecEnv (0, 0)

prettyExp :: PrecedenceEnv -> (Int, Int) -> PFExp Int Int -> Doc
prettyExp p (i,j) (FVar _ x)      = text (var x)
prettyExp p (i,j) (FLit (Integer n)) = integer n
prettyExp p (i,j) (FLit (String s))  = string s
prettyExp p (i,j) (FLit (Boolean b)) = bool b
prettyExp p (i,j) (FTuple es)     = tupled (map (prettyExp basePrecEnv (i,j)) es)
prettyExp p (i,j) (FIf e1 e2 e3) = parensIf p 1 (text "if" <+> prettyExp (1,PrecMinus) (i,j) e1 <+>
                                                  indent 2 (text "then" <+> prettyExp (1, PrecMinus) (i,j) e2 <+>
                                                            text "else" <+> prettyExp (1, PrecMinus) (i,j) e3))
prettyExp p (i,j) (FBLam f)       = parensIf p 2 (char 'Λ' <+> text (tvar i) <> dot <+>
                                                  prettyExp (2,PrecMinus) (succ i, j) (f i))
prettyExp p (i,j) (FLam t f)      = parensIf p 2 (char 'λ' <+>
                                                  parens (text (var j) <+> colon <+> prettyTyp basePrecEnv i t) <>
                                                  dot <+>
                                                  prettyExp (2,PrecMinus) (i, succ j) (f j))
prettyExp p (i,j) (FTApp e t)     = parensIf p 4 (parens (prettyExp (4,PrecMinus) (i,j) e)  <+>
                                                  parens (prettyTyp (4,PrecPlus) i t))
prettyExp p (i,j) (FApp e1 e2)    = parensIf p 4 (parens (prettyExp (4,PrecMinus) (i,j) e1) <+>
                                                  parens (prettyExp (4,PrecPlus) (i,j) e2))
prettyExp p i (FProj n e)         = parensIf p 5 (prettyExp (5,PrecMinus) i e) <> dot <> char '_' <> int n

-- Substitutions

fsubstEE :: (Int, PFExp Int Int) -> PFExp Int Int -> PFExp Int Int
fsubstEE (x,r) (FVar s x')
  | x' == x                       = r
  | otherwise                     = FVar s x'
fsubstEE (x,r) (FLit n)           = FLit n
fsubstEE (x,r) (FBLam f)          = FBLam (fsubstEE (x,r) . f)
fsubstEE (x,r) (FLam t' f)        = FLam t' (fsubstEE (x,r) . f)
fsubstEE (x,r) (FApp e1 e2)       = FApp (fsubstEE (x,r) e1) (fsubstEE (x,r) e2)
fsubstEE (x,r) (FPrimOp e1 op e2) = FPrimOp (fsubstEE (x,r) e1) op (fsubstEE (x,r) e2)
fsubstEE (x,r) (FIf e1 e2 e3)    = FIf (fsubstEE (x,r) e1) (fsubstEE (x,r) e2) (fsubstEE (x,r) e3)
fsubstEE (x,r) (FTuple es)        = FTuple (map (fsubstEE (x,r)) es)
fsubstEE (x,r) (FProj i' e)       = FProj i' (fsubstEE (x,r) e)

fsubstTT :: (Int, PFTyp Int) -> PFTyp Int -> PFTyp Int
fsubstTT (i,t) (FTVar j)
    | j == i                 = t
    | otherwise              = FTVar j
fsubstTT (i,t) (FForall f)   = FForall (fsubstTT (i,t) . f)
fsubstTT (i,t) (FFun t1 t2)  = FFun (fsubstTT (i,t) t1) (fsubstTT (i,t) t2)
fsubstTT (i,t)  FInt         = FInt
fsubstTT (i,t) (FProduct ts) = FProduct (map (fsubstTT (i,t)) ts)

fsubstTE :: (Int, PFTyp Int) -> PFExp Int Int -> PFExp Int Int
fsubstTE (i,t) (FVar s x)         = FVar s x
fsubstTE (i,t) (FLit n)           = FLit n
fsubstTE (i,t) (FBLam f)          = FBLam (fsubstTE (i,t) . f)
fsubstTE (i,t) (FLam t' f)        = FLam (fsubstTT (i,t) t') (fsubstTE (i,t) . f)
fsubstTE (i,t) (FApp e1 e2)       = FApp (fsubstTE (i,t) e1) (fsubstTE (i,t) e2)
fsubstTE (i,t) (FPrimOp e1 op e2) = FPrimOp (fsubstTE (i,t) e1) op (fsubstTE (i,t) e2)
fsubstTE (i,t) (FIf e1 e2 e3)    = FIf (fsubstTE (i,t) e1) (fsubstTE (i,t) e2) (fsubstTE (i,t) e3)
fsubstTE (i,t) (FTuple es)        = FTuple (map (fsubstTE (i,t)) es)
fsubstTE (i,t) (FProj i' e)       = FProj i' (fsubstTE (i,t) e)