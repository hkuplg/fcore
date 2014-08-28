{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module SystemF.Syntax
    ( Type(..)
    , Expr(..)
    , PrimLit
    , alphaEqTyp
    , fsubstTT
    , fsubstTE
    , fsubstEE
    , prettyExp
    , prettyTyp
    ) where

import PrettyUtils
import ESF.Syntax (Operator(..),Lit(..))

import Text.PrettyPrint.Leijen

data Type t
  = TVar t
  | Forall (t -> Type t)
  | Fun (Type t) (Type t)
  | JClass String
  | Product [Type t]

data Expr t e
  = Var String e
  | BLam          (t -> Expr t e)
  | Lam (Type t) (e -> Expr t e)
  | TApp (Expr t e) (Type t)
  | App  (Expr t e) (Expr t e)
  | PrimOp (Expr t e) Operator (Expr t e) -- SystemF extension from: https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf (no int restriction)
  | Lit Lit
  | If (Expr t e) (Expr t e) (Expr t e)
  | Tuple [Expr t e]
  | Proj Int (Expr t e)

    -- fix x (x1 : t1) : t2. e, or the new syntax:
    -- fix (x : t1 -> t2). \x1. e
  | LetRec [(Type t, Type t)] ([e] -> [Expr t e]) ([e] -> Expr t e)
  | Fix (e -> e -> Expr t e)
         (Type t) -- t1
         (Type t) -- t2
-- Java
  | JNewObj String [Expr t e]
  | JMethod (Either (Expr t e) String) String [Expr t e] String
  | JField (Either (Expr t e) String) String String
  | Seq [Expr t e]

newtype Typ = HideTyp { revealTyp :: forall t. Type t } -- type of closed types

type PrimLit = Integer -- later maybe Bool | Char

newtype Exp = HideExp { revealExp :: forall t e. Expr t e }

alphaEqTyp :: Type Int -> Type Int -> Bool
alphaEqTyp = alphaEqTyp' 0

alphaEqTyp' :: Int -> Type Int -> Type Int -> Bool

alphaEqTyp' i (TVar x)     (TVar y)    = x == y
alphaEqTyp' i (JClass c1)  (JClass c2) = c1 == c2
alphaEqTyp' i (Forall f)   (Forall g)  = alphaEqTyp' (i + 1) (f i) (g i)
alphaEqTyp' i (Fun s1 s2)  (Fun t1 t2) = alphaEqTyp' i s1 t1 && alphaEqTyp' i s2 t2
alphaEqTyp' i (Product ss) (Product ts)
  | length ss == length ts             = uncurry (alphaEqTyp' i) `all` zip ss ts
  | otherwise                          = False
alphaEqTyp' i  _             _         = False

-- Prettyprinting of types

instance Show (Type Int) where
  show = show . pretty

instance Pretty (Type Int) where
  pretty = prettyTyp basePrecEnv 0

prettyTyp :: PrecedenceEnv -> Int -> Type Int -> Doc
prettyTyp p i (TVar a)     = text (nameTVar a)
prettyTyp p i (JClass a)   = text a
prettyTyp p i (Forall f)   = parensIf p 1 (char '∀' <+> text (nameTVar i) <> dot <+> prettyTyp (1,PrecMinus) (succ i) (f i))
prettyTyp p i (Fun t1 t2)  = parensIf p 2 (prettyTyp (2,PrecPlus) i t1 <+> char '→' <+> prettyTyp (2,PrecMinus) i t2)
prettyTyp p i (Product ts) = tupled (map (prettyTyp basePrecEnv i) ts)

-- Prettyprinting of expressions

instance Show (Expr Int Int) where
  show = show . pretty

instance Pretty (Expr Int Int) where
  pretty = prettyExp basePrecEnv (0, 0)

prettyExp :: PrecedenceEnv -> (Int, Int) -> Expr Int Int -> Doc
prettyExp p (i,j) (Var _ x)         = text (nameVar x)
prettyExp p (i,j) (Lit (Integer n)) = integer n
prettyExp p (i,j) (Lit (String s))  = string s
prettyExp p (i,j) (Lit (Boolean b)) = bool b
prettyExp p (i,j) (Tuple es)     = tupled (map (prettyExp basePrecEnv (i,j)) es)
prettyExp p (i,j) (If e1 e2 e3)  = parensIf p 1 (text "if" <+> prettyExp (1,PrecMinus) (i,j) e1 <+>
                                                   indent 2 (text "then" <+> prettyExp (1, PrecMinus) (i,j) e2 <+>
                                                             text "else" <+> prettyExp (1, PrecMinus) (i,j) e3))
prettyExp p (i,j) (BLam f)       = parensIf p 2 (char 'Λ' <+> text (nameTVar i) <> dot <+>
                                                  prettyExp (2,PrecMinus) (succ i, j) (f i))
prettyExp p (i,j) (Lam t f)      = parensIf p 2 (char 'λ' <+>
                                                  parens (text (nameVar j) <+> colon <+> prettyTyp basePrecEnv i t) <>
                                                  dot <+>
                                                  prettyExp (2,PrecMinus) (i, succ j) (f j))
prettyExp p (i,j) (TApp e t)     = parensIf p 4 (prettyExp (4,PrecMinus) (i,j) e  <+> prettyTyp (4,PrecPlus) i t)
prettyExp p (i,j) (App e1 e2)    = parensIf p 4 (prettyExp (4,PrecMinus) (i,j) e1 <+> prettyExp (4,PrecPlus) (i,j) e2)
prettyExp p i (Proj n e)         = parensIf p 5 (prettyExp (5,PrecMinus) i e) <> dot <> char '_' <> int n

-- Substitutions

fsubstEE :: (Int, Expr Int Int) -> Expr Int Int -> Expr Int Int
fsubstEE (x,r)
  = go
  where
    go (Var s x')
      | x' == x           = r
      | otherwise         = Var s x'
    go (Lit n)            = Lit n
    go (BLam f)           = BLam (go . f)
    go (Lam t' f)         = Lam t' (go . f)
    go (App e1 e2)        = App (go e1) (go e2)
    go (PrimOp e1 op e2)  = PrimOp (go e1) op (go e2)
    go (If e1 e2 e3)      = If (go e1) (go e2) (go e3)
    go (Tuple es)         = Tuple (map go es)
    go (Proj i' e)        = Proj i' (go e)
    go (JNewObj s args)   = JNewObj s (map go args)
    go (JMethod c s args ss) =
      case c of (Left ce)  -> JMethod (Left $ go ce) s (map go args) ss
                (Right cn) -> JMethod (Right cn) s (map go args) ss

fsubstTT :: (Int, Type Int) -> Type Int -> Type Int
fsubstTT (i,t) (TVar j)
    | j == i                = t
    | otherwise             = TVar j
fsubstTT (i,t) (JClass c)   = JClass c
fsubstTT (i,t) (Forall f)   = Forall (fsubstTT (i,t) . f)
fsubstTT (i,t) (Fun t1 t2)  = Fun (fsubstTT (i,t) t1) (fsubstTT (i,t) t2)
fsubstTT (i,t) (Product ts) = Product (map (fsubstTT (i,t)) ts)

fsubstTE :: (Int, Type Int) -> Expr Int Int -> Expr Int Int
fsubstTE (i, t)
  = go
  where
    go (Var s x)          = Var s x
    go (Lit n)            = Lit n
    go (BLam f)           = BLam (go . f)
    go (Lam t' f)         = Lam (fsubstTT (i, t) t') (go . f)
    go (App e1 e2)        = App (go e1) (go e2)
    go (PrimOp e1 op e2)  = PrimOp (go e1) op (go e2)
    go (If e1 e2 e3)      = If (go e1) (go e2) (go e3)
    go (Tuple es)         = Tuple (map go es)
    go (Proj i' e)        = Proj i' (go e)
    go (JNewObj s args)   = JNewObj s (map go args)
    go (JMethod c s args ss) =
      case c of (Left ce)  -> JMethod (Left $ go ce) s (map go args) ss
                (Right cn) -> JMethod (Right cn) s (map go args) ss
