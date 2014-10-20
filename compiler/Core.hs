{-# LANGUAGE FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fwarn-incomplete-patterns #-}

module Core
  ( Type(..)
  , Expr(..)
  , TypeContext
  , ValueContext
  , alphaEquiv
  , fsubstTT, fsubstTE, fsubstEE
  , prettyType, prettyExpr
  ) where

import qualified Src

import JavaUtils
import PrettyUtils

import Text.PrettyPrint.Leijen
import qualified Language.Java.Syntax as J (Op(..))
import qualified Language.Java.Pretty      (prettyPrint)

import           Data.List (intersperse)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Prelude hiding (id)

data Type t
  = TyVar t                -- a
  | JClass ClassName       -- C
  | Fun (Type t) (Type t)  -- t1 -> t2
  | Forall (t -> Type t)   -- forall a. t
  | Product [Type t]       -- (t1, ..., tn)
  | And (Type t) (Type t)  -- t1 & t2
  | RecordTy (Src.Label, Type t)
  | ListOf (Type t)
    -- Warning: If you ever add a case to this, you MUST also define the binary
    -- relations on your new case. Namely, add cases for your data constructor
    -- in `alphaEquiv' and `coerce' below. Consult George if you're not sure.

data Expr t e
  = Var e
  | Lit Src.Lit

  -- Binders we have: λ, fix, letrec, and Λ
  | Lam (Type t) (e -> Expr t e)
  | Fix (e -> e -> Expr t e)
        (Type t)  -- t1
        (Type t)  -- t
      -- fix x (x1 : t1) : t. e     Syntax in the tal-toplas paper
      -- fix (x : t1 -> t). \x1. e  Alternative syntax, which is arguably clear
  | LetRec [Type t]             -- Signatures
           ([e] -> [Expr t e])  -- Bindings
           ([e] -> Expr t e)    -- Body
  | BLam (t -> Expr t e)

  | App  (Expr t e) (Expr t e)
  | TApp (Expr t e) (Type t)

  | If (Expr t e) (Expr t e) (Expr t e)
  | PrimOp (Expr t e) Src.Operator (Expr t e)
      -- SystemF extension from:
      -- https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf
      -- (no int restriction)

  | Tuple [Expr t e]     -- Tuple introduction
  | Proj Int (Expr t e)  -- Tuple elimination

  -- Java
  | JNewObj ClassName [Expr t e]
  | JMethod (Either ClassName (Expr t e)) MethodName [Expr t e] ClassName
  | JField  (Either ClassName (Expr t e)) FieldName ClassName

  | Seq [Expr t e]

  | PrimList [Expr t e]

  | Merge (Expr t e) (Expr t e)  -- e1 ,, e2
  | Record (Src.Label, Expr t e)
  | RecordAccess (Expr t e) Src.Label
  | RecordUpdate (Expr t e) (Src.Label, Expr t e)

-- newtype Typ = HideTyp { revealTyp :: forall t. Type t } -- type of closed types

-- newtype Exp = HideExp { revealExp :: forall t e. Expr t e }

type TypeContext t    = Set.Set t
type ValueContext t e = Map.Map e (Type t)

type Index = Int

alphaEquiv :: Type Index -> Type Index -> Bool
alphaEquiv = go 0
  where
    go i (TyVar a)    (TyVar b)    = a == b
    go i (JClass c)   (JClass d)   = c == d
    go i (Fun s1 s2)  (Fun t1 t2)  = go i s1 t1 && go i s2 t2
    go i (Forall f)   (Forall g)   = go (succ i) (f i) (g i)
    go i (Product ss) (Product ts) = length ss == length ts
                                     && uncurry (go i) `all` zip ss ts
    go i (And s1 s2)  (And t1 t2)  = go i s1 t1 && go i s2 t2
    go i (ListOf t1)  (ListOf t2)  = go i t1 t2
    go i t1           t2           = False

prettyType :: Prec -> Index -> Type Index -> Doc

prettyType p i (TyVar a)     = prettyTVar a

prettyType p i (Fun t1 t2)  =
  parensIf p 2
    (prettyType (2,PrecPlus) i t1 <+> arrow <+> prettyType (2,PrecMinus) i t2)

prettyType p i (Forall f)   =
  parensIf p 1
    (forall <+> prettyTVar i <> dot <+>
     prettyType (1,PrecMinus) (succ i) (f i))

prettyType p i (Product ts) = parens $ hcat (intersperse comma (map (prettyType basePrec i) ts))

prettyType p i (ListOf t) = text "ListOf" <+> prettyType p i t

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

prettyType p i (RecordTy (l,t)) = lbrace <> text l <> colon <> prettyType basePrec i t <> rbrace

-- instance Show (Expr Index Index) where
--   show = show . pretty

instance Pretty (Expr Index Index) where
  pretty = prettyExpr basePrec (0, 0)

prettyExpr :: Prec -> (Index, Index) -> Expr Index Index -> Doc

prettyExpr p (i,j) (Var x) = prettyVar x

prettyExpr p (i,j) (Lam t f) =
  parensIf p 2
    (hang 3 (lambda <+> parens (prettyVar j <+> colon <+> prettyType basePrec i t) <> dot <+>
             prettyExpr (2,PrecMinus) (i, succ j) (f j)))

prettyExpr p (i,j) (App e1 e2) =
  parensIf p 4
    (prettyExpr (4,PrecMinus) (i,j) e1 <+> prettyExpr (4,PrecPlus) (i,j) e2)

prettyExpr p (i,j) (BLam f) =
  parensIf p 2
    (biglambda <+> prettyTVar i <> dot <+>
     prettyExpr (2,PrecMinus) (succ i, j) (f i))

prettyExpr p (i,j) (TApp e t) =
  parensIf p 4
    (prettyExpr (4,PrecMinus) (i,j) e <+> prettyType (4,PrecPlus) i t)

prettyExpr p (i,j) (Lit (Src.Integer n)) = integer n
prettyExpr p (i,j) (Lit (Src.String s))  = dquotes (string s)
prettyExpr p (i,j) (Lit (Src.Boolean b)) = bool b
prettyExpr p (i,j) (Lit (Src.Char c))    = char c

prettyExpr p (i,j) (If e1 e2 e3)
  = parensIf p prec
      (hang 3 (text "if"   <+> prettyExpr (prec,PrecMinus) (i,j) e1 <+>
               text "then" <+> prettyExpr (prec,PrecMinus) (i,j) e2 <+>
               text "else" <+> prettyExpr (prec,PrecMinus) (i,j) e3))
  where prec = 3

prettyExpr p (i,j) (PrimOp e1 op e2)
  = parens (prettyExpr p (i,j) e1 <+> pretty_op <+> prettyExpr p (i,j) e2)
  where
    pretty_op = text (Language.Java.Pretty.prettyPrint java_op)
    java_op   = case op of
                  Src.Arith   op' -> op'
                  Src.Compare op' -> op'
                  Src.Logic   op' -> op'

prettyExpr p (i,j) (Tuple es) = parens $ hcat (intersperse comma (map (prettyExpr basePrec (i,j)) es))

prettyExpr p i (Proj n e) =
  parensIf p 5
    (prettyExpr (5,PrecMinus) i e <> dot <> char '_' <> int n)

prettyExpr p (i,j) (JNewObj c args) =
  parens (text "new" <+> text c <> tupled (map (prettyExpr basePrec (i,j)) args))

prettyExpr p i (JMethod name m args r) = methodStr name <> dot <> text m <> tupled (map (prettyExpr basePrec i) args)
  where
    methodStr (Left x) = text x
    methodStr (Right x) = prettyExpr (6,PrecMinus) i x

prettyExpr p i (JField name f r) = fieldStr name <> dot <> text f
  where
    fieldStr (Left x) = text x
    fieldStr (Right x) = prettyExpr (6,PrecMinus) i x

prettyExpr p (i,j) (Seq es) = semiBraces (map (prettyExpr p (i,j)) es)

prettyExpr p (i,j) (Fix f t1 t) =
  parens
    (text "fix" <+> prettyVar j <+>
     parens (prettyVar (succ j) <+> colon <+> prettyType p i t1) <+>
     colon <+>
     prettyType p i t <> dot <$$>
     indent 2 (prettyExpr p (i, j + 2) (f j (j + 1))))

prettyExpr p (i,j) (LetRec sigs binds body)
  = text "let" <+> text "rec" <$$>
    vcat (intersperse (text "and") (map (indent 2) pretty_binds)) <$$>
    text "in" <$$>
    pretty_body
  where
    n   = length sigs
    ids = [i..(i+n-1)]
    pretty_ids   = map prettyVar ids
    pretty_sigs  = map (prettyType p i) sigs
    pretty_defs  = map (prettyExpr p (i, j + n)) (binds ids)
    pretty_binds = zipWith3 (\pretty_id pretty_sig pretty_def ->
                  pretty_id <+> colon <+> pretty_sig <$$> indent 2 (equals <+> pretty_def))
                  pretty_ids pretty_sigs pretty_defs
    pretty_body  = prettyExpr p (i, j + n) (body ids)

prettyExpr p (i,j) (Merge e1 e2) =
  parens $ prettyExpr p (i,j) e1 <+> dcomma <+> prettyExpr p (i,j) e2

prettyExpr p (i,j) (Record (l,e))          = lbrace <> text l <> equals <> prettyExpr basePrec (i,j) e <> rbrace
prettyExpr p (i,j) (RecordAccess e l)      = prettyExpr p (i,j) e <> dot <> text l
prettyExpr p (i,j) (RecordUpdate e (l,e1)) = prettyExpr p (i,j) e <+> text "with" <+> prettyExpr p (i,j) (Record (l,e1))

fsubstTT :: (Index, Type Index) -> Type Index -> Type Index
fsubstTT (x,r) (TyVar a)
  | a == x                 = r
  | otherwise              = TyVar a
fsubstTT (x,r) (Fun t1 t2) = Fun (fsubstTT (x,r) t1) (fsubstTT (x,r) t2)
fsubstTT (x,r) (Forall f)  = Forall (\a -> fsubstTT (x,r) (f a))
fsubstTT (x,r) (JClass c)  = JClass c
fsubstTT (x,r) (And t1 t2) = And (fsubstTT (x,r) t1) (fsubstTT (x,r) t2)

fsubstTE :: (Index, Type Index) -> Expr Index Index -> Expr Index Index
fsubstTE (x,r) (Var a)       = Var a
fsubstTE (x,r) (Lit n)       = Lit n
fsubstTE (x,r) (BLam g)      = BLam (fsubstTE (x,r) . g)
fsubstTE (x,r) (Lam t f)     = Lam (fsubstTT (x,r) t) (fsubstTE (x,r) . f)
fsubstTE (x,r) (TApp e t)    = TApp (fsubstTE (x,r) e) (fsubstTT (x,r) t)
fsubstTE (x,r) (App e1 e2)   = App (fsubstTE (x,r) e1) (fsubstTE (x,r) e2)
fsubstTE (x,r) (Merge e1 e2) = Merge (fsubstTE (x,r) e1) (fsubstTE (x,r) e2)

fsubstEE :: (Index, Expr Index Index) -> Expr Index Index -> Expr Index Index
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
    go (JMethod (Right e)  m args ret) = JMethod (Right (go e)) m (map go args) ret
    go (JMethod (Left c) m args ret)  = JMethod (Left c)     m (map go args) ret
    go (JField (Right e) f ret)       = JField (Right (go e)) f ret
    go (JField (Left c) f ret)        = JField (Left c)     f ret
    go (Seq es)                       = Seq (map go es)
    go (Merge e1 e2)                  = Merge (go e1) (go e2)
    go (LetRec sigs binds body)       = LetRec sigs (\ids -> map go (binds ids)) (\ids -> go (body ids))
    go (PrimList es)                  = PrimList (map go es)
