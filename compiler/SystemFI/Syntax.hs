{-# LANGUAGE FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fwarn-incomplete-patterns #-}

module SystemFI.Syntax
  ( Type(..)
  , Expr(..)
  , TypeContext
  , ValueContext
  , emptyValueContext

  -- "Eq"
  , structEqType

  -- "Show"
  , prettyType
  , prettyExpr

  , fsubstTT
  , fsubstTE
  , fsubstEE
  ) where

import qualified ESF.Syntax as E

import JavaUtils
import PrettyUtils

import Text.PrettyPrint.Leijen

import qualified Data.Set as Set
import qualified Data.Map as Map

data Type t
  = TVar t                -- a
  | Fun (Type t) (Type t) -- t1 -> t2
  | Forall (t -> Type t)  -- forall a. t
  | Product [Type t]      -- (t1, ..., tn)
  | JClass String         -- C
  | And (Type t) (Type t) -- t1 & t2

data Expr t e
  = Var e

  | Lam (Type t) (e -> Expr t e)
  | App  (Expr t e) (Expr t e)

  | BLam (t -> Expr t e)
  | TApp (Expr t e) (Type t)

  | Lit E.Lit                           -- Literal introduction
  | If (Expr t e) (Expr t e) (Expr t e) -- Literal elimination
  | PrimOp (Expr t e) E.Operator (Expr t e)
      -- SystemF extension from:
      -- https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf
      -- (no int restriction)

  | Tuple [Expr t e]    -- Tuple introduction
  | Proj Int (Expr t e) -- Tuple elimination

    -- fix x (x1 : t1) : t2. e     Syntax in the tal-toplas paper
    -- fix (x : t1 -> t2). \x1. e  Alternative syntax, which is arguably clear
  | Fix
      (e -> e -> Expr t e)
      (Type t) -- t1
      (Type t) -- t2

  -- Java
  | JNewObj ClassName [Expr t e]
  | JMethod
      (Either (Expr t e) ClassName) MethodName [Expr t e]
      ClassName
  | JField
      (Either (Expr t e) ClassName) FieldName
      ClassName
  | Seq [Expr t e]

  | Merge (Expr t e) (Expr t e)  -- e1 ,, e2

newtype Typ = HideTyp { revealTyp :: forall t. Type t } -- type of closed types

newtype Exp = HideExp { revealExp :: forall t e. Expr t e }

type TypeContext t    = Set.Set t
type ValueContext t e = Map.Map e (Type t)

emptyValueContext :: ValueContext t e
emptyValueContext = Map.empty

structEqType :: Type Int -> Type Int -> Bool
structEqType = go 0
  where
    go :: Int -> Type Int -> Type Int -> Bool

    go i (TVar a)    (TVar b)    = a == b
    go i (Forall f)  (Forall g)  = go i (f i) (g i)
    go i (Fun s1 s2) (Fun t1 t2) = go i s1 t1 && go i s2 t2
    go i (And s1 s2) (And t1 t2) = go i s1 t1 && go i s2 t2
    go i  _           _          = False

alphaEqType :: Type Int -> Type Int -> Bool
alphaEqType = go 0
  where
    go :: Int -> Type Int -> Type Int -> Bool

    go i (TVar x)     (TVar y)    = x == y
    go i (Fun s1 s2)  (Fun t1 t2) = go i s1 t1 && go i s2 t2
    go i (Forall f)   (Forall g)  = go (i + 1) (f i) (g i)
    go i (Product ss) (Product ts)
      | length ss == length ts    = (\(s,t) -> go i s t) `all` zip ss ts
      | otherwise                 = False
    go i (JClass c)   (JClass d)  = c == d
    go i  _             _         = False

prettyType :: PrecedenceEnv -> Int -> Type Int -> Doc
prettyType p i (TVar a)     = text (nameTVar a)
prettyType p i (Fun t1 t2)  =
  parensIf p 2
    (prettyType (2,PrecPlus) i t1 <+> text "->" <+> prettyType (2,PrecMinus) i t2)
prettyType p i (Forall f)   =
  parensIf p 1
    (text "forall" <+> text (nameTVar i) <> dot <+>
     prettyType (1,PrecMinus) (succ i) (f i))
prettyType p i (Product ts) = tupled (map (prettyType basePrecEnv i) ts)
prettyType p i (JClass "java.lang.Integer")   = text "Int"
prettyType p i (JClass "java.lang.String")    = text "String"
prettyType p i (JClass "java.lang.Boolean")   = text "Bool"
prettyType p i (JClass "java.lang.Character") = text "Char"
prettyType p i (JClass c)                     = text c
prettyType p i (And t1 t2) = parensIf p 2 (prettyType (2,PrecMinus) i t1 <+>
                                           text "&"  <+>
                                           prettyType (2,PrecPlus) i t2)

instance Show (Expr Int Int) where
  show = show . pretty

instance Pretty (Expr Int Int) where
  pretty = prettyExpr basePrecEnv (0, 0)

prettyExpr :: PrecedenceEnv -> (Int, Int) -> Expr Int Int -> Doc
prettyExpr p (i,j) (Var x) = text (nameVar x)
prettyExpr p (i,j) (Lam t f) =
  parensIf p 2
    (text "\\" <>
     parens (text (nameVar j) <+> colon <+> prettyType basePrecEnv i t) <>
     dot <+>
     prettyExpr (2,PrecMinus) (i, succ j) (f j))
prettyExpr p (i,j) (App e1 e2) =
  parensIf p 4
    (prettyExpr (4,PrecMinus) (i,j) e1 <+> prettyExpr (4,PrecPlus) (i,j) e2)
prettyExpr p (i,j) (BLam f) =
  parensIf p 2
    (text "/\\" <> text (nameTVar i) <> dot <+>
     prettyExpr (2,PrecMinus) (succ i, j) (f i))
prettyExpr p (i,j) (TApp e t) =
  parensIf p 4
    (prettyExpr (4,PrecMinus) (i,j) e <+> prettyType (4,PrecPlus) i t)
prettyExpr p (i,j) (Lit (E.Integer n)) = integer n
prettyExpr p (i,j) (Lit (E.String s))  = string s
prettyExpr p (i,j) (Lit (E.Boolean b)) = bool b
prettyExpr p (i,j) (Lit (E.Char c))    = char c
prettyExpr p (i,j) (If e1 e2 e3) =
  parensIf p 1
    (text "if"   <+> prettyExpr (1,PrecMinus) (i,j) e1 <+>
     text "then" <+> prettyExpr (1,PrecMinus) (i,j) e2 <+>
     text "else" <+> prettyExpr (1,PrecMinus) (i,j) e3)
prettyExpr p (i,j) (PrimOp e1 op e2) =
  parens (prettyExpr p (i,j) e1 <+> text "_" <+> prettyExpr p (i,j) e2)
    -- TODO: consider precedence & pretty-print operator
prettyExpr p (i,j) (Tuple es) = tupled (map (prettyExpr basePrecEnv (i,j)) es)
prettyExpr p i (Proj n e) =
  parensIf p 5
    (prettyExpr (5,PrecMinus) i e <> dot <> char '_' <> int n)
prettyExpr p (i,j) (JNewObj c args) =
  parens (text "new" <+> text c <> tupled (map (prettyExpr basePrecEnv (i,j)) args))
prettyExpr p (i,j) (JMethod e m args r) = undefined
prettyExpr p (i,j) (JField e m r) = undefined
prettyExpr p (i,j) (Fix f t1 t) =
  parens
    (text "fix" <+> text (nameVar j) <+>
     parens (text (nameVar (succ j)) <+> colon <+> prettyType p i t1) <+>
     colon <+>
     prettyType p i t <> dot <+>
     prettyExpr p (i, succ (succ j)) (f j (succ j)))

fsubstTT :: (Int, Type Int) -> Type Int -> Type Int
fsubstTT (x,r) (TVar a)
  | a == x                 = r
  | otherwise              = TVar a
fsubstTT (x,r) (Fun t1 t2) = Fun (fsubstTT (x,r) t1) (fsubstTT (x,r) t2)
fsubstTT (x,r) (Forall f)  = Forall (\a -> fsubstTT (x,r) (f a))
fsubstTT (x,r) (JClass c)  = JClass c
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
fsubstEE (x,r) (BLam f)      = BLam (fsubstEE (x,r) . f )
fsubstEE (x,r) (Lam t f)     = Lam t (fsubstEE (x,r) . f)
fsubstEE (x,r) (TApp e t)    = TApp (fsubstEE (x,r) e) t
fsubstEE (x,r) (App e1 e2)   = App (fsubstEE (x,r) e1) (fsubstEE (x,r) e2)
-- fsubstEE (x,r) (JNewObj s args) = JNewObj s (map go args)
-- fsubstEE (x,r) (JMethod c s args ss) =
--                   case c of (Left ce)  -> JMethod (Left $ go ce) s (map go args) ss
--                             (Right cn) -> JMethod (Right cn) s (map go args) ss
-- fsubstEE (x,r) (Merge e1 e2) = Merge (fsubstEE (x,r) e1) (fsubstEE (x,r) e2)