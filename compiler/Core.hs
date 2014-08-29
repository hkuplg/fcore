{-# LANGUAGE FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fwarn-incomplete-patterns #-}

module Core
  ( Type(..)
  , Expr(..)
  , TypeContext
  , ValueContext
  , emptyValueContext

  -- "Eq"
  , alphaEqType

  -- "Show"
  , prettyType, prettyExpr

  , fsubstTT, fsubstTE, fsubstEE

  , isSystemfType, isSystemfExpr

  , javaInt, javaString, javaBool, javaChar
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

    -- fix x (x1 : t1) : t. e     Syntax in the tal-toplas paper
    -- fix (x : t1 -> t). \x1. e  Alternative syntax, which is arguably clear
  | Fix
      (e -> e -> Expr t e)
      (Type t) -- t1
      (Type t) -- t

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

-- newtype Typ = HideTyp { revealTyp :: forall t. Type t } -- type of closed types

-- newtype Exp = HideExp { revealExp :: forall t e. Expr t e }

type TypeContext t    = Set.Set t
type ValueContext t e = Map.Map e (Type t)

emptyValueContext :: ValueContext t e
emptyValueContext = Map.empty

alphaEqType :: Type Int -> Type Int -> Bool
alphaEqType = go 0
  where
    go i (TVar a)    (TVar b)      = a == b
    go i (Fun s1 s2) (Fun t1 t2)   = go i s1 t1 && go i s2 t2
    go i (Forall f)  (Forall g)    = go (succ i) (f i) (g i)
    go i (Product ss) (Product ts) = length ss == length ts &&
                                     uncurry (go i) `all` zip ss ts
    go i (JClass c)  (JClass d)    = c == d
    go i (And s1 s2) (And t1 t2)   = go i s1 t1 && go i s2 t2
    go i  _           _            = False

prettyType :: Prec -> Int -> Type Int -> Doc

prettyType p i (TVar a)     = pprTVar a

prettyType p i (Fun t1 t2)  =
  parensIf p 2
    (prettyType (2,PrecPlus) i t1 <+> arrow <+> prettyType (2,PrecMinus) i t2)

prettyType p i (Forall f)   =
  parensIf p 1
    (forall <+> pprTVar i <> dot <+>
     prettyType (1,PrecMinus) (succ i) (f i))

prettyType p i (Product ts) = tupled (map (prettyType basePrec i) ts)

prettyType p i (JClass "java.lang.Integer")   = text "Int"
prettyType p i (JClass "java.lang.String")    = text "String"
prettyType p i (JClass "java.lang.Boolean")   = text "Bool"
prettyType p i (JClass "java.lang.Character") = text "Char"
prettyType p i (JClass c)                     = text c

prettyType p i (And t1 t2) =
  parensIf p 2
    (prettyType (2,PrecMinus) i t1 <+>
     ampersand  <+>
     prettyType (2,PrecPlus) i t2)

instance Show (Expr Int Int) where
  show = show . pretty

instance Pretty (Expr Int Int) where
  pretty = prettyExpr basePrec (0, 0)

prettyExpr :: Prec -> (Int, Int) -> Expr Int Int -> Doc

prettyExpr p (i,j) (Var x) = pprVar x

prettyExpr p (i,j) (Lam t f) =
  parensIf p 2
    (lambda <>
     parens (pprVar j <+> colon <+> prettyType basePrec i t) <>
     dot <+>
     prettyExpr (2,PrecMinus) (i, succ j) (f j))

prettyExpr p (i,j) (App e1 e2) =
  parensIf p 4
    (prettyExpr (4,PrecMinus) (i,j) e1 <+> prettyExpr (4,PrecPlus) (i,j) e2)

prettyExpr p (i,j) (BLam f) =
  parensIf p 2
    (biglambda <> pprTVar i <> dot <+>
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

prettyExpr p (i,j) (Tuple es) = tupled (map (prettyExpr basePrec (i,j)) es)

prettyExpr p i (Proj n e) =
  parensIf p 5
    (prettyExpr (5,PrecMinus) i e <> dot <> char '_' <> int n)

prettyExpr p (i,j) (JNewObj c args) =
  parens (text "new" <+> text c <> tupled (map (prettyExpr basePrec (i,j)) args))

prettyExpr p (i,j) (JMethod e m args r) = undefined

prettyExpr p (i,j) (JField e m r) = undefined

prettyExpr p (i,j) (Seq es) = semiBraces (map (prettyExpr p (i,j)) es)

prettyExpr p (i,j) (Fix f t1 t) =
  parens
    (text "fix" <+> pprVar j <+>
     parens (pprVar (succ j) <+> colon <+> prettyType p i t1) <+>
     colon <+>
     prettyType p i t <> dot <+>
     prettyExpr p (i, succ (succ j)) (f j (succ j)))

prettyExpr p (i,j) (Merge e1 e2) =
  parens $ prettyExpr p (i,j) e1 <+> dcomma <+> prettyExpr p (i,j) e2

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
fsubstEE (x,r)
  = go
  where
    go (Var a)
      | a == x                        = r
      | otherwise                     = Var a
    go (Lam t f)                      = Lam t (go . f)
    go (App e1 e2)                    = App (go e1) (go e2)
    go (BLam f)                       = BLam (go . f )
    go (TApp e t)                     = TApp (go e) t
    go (Lit n)                        = Lit n
    go (If prd b1 b2)                 = If (go prd) (go b1) (go b2)
    go (PrimOp e1 op e2)              = PrimOp (go e1) op (go e2)
    go (Tuple es)                     = Tuple (map go es)
    go (Proj i e)                     = Proj i (go e)
    go (Fix f t1 t)                   = Fix (\x' x1 -> go (f x' x1)) t1 t
    go (JNewObj s args)               = JNewObj s (map go args)
    go (JMethod (Left e)  m args ret) = JMethod (Left (go e)) m (map go args) ret
    go (JMethod (Right c) m args ret) = JMethod (Right c)     m (map go args) ret
    go (JField (Left e) f ret)        = JField (Left (go e)) f ret
    go (JField (Right c) f ret)       = JField (Right c)     f ret
    go (Seq es)                       = Seq (map go es)
    go (Merge e1 e2)                  = Merge (go e1) (go e2)

isSystemfType :: Type t -> Bool
isSystemfType = error "isSystemfType"

isSystemfExpr :: Expr t e -> Bool
isSystemfExpr = error "isSystemfExpr"

javaInt, javaString, javaBool, javaChar :: Type t
javaInt    = JClass javaIntClass
javaString = JClass javaStringClass
javaBool   = JClass javaBoolClass
javaChar   = JClass javaCharClass