{-# LANGUAGE FlexibleInstances, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-matches -fwarn-incomplete-patterns #-}

module Core
  ( Type(..)
  , Expr(..)
  , TypeContext
  , ValueContext
  , emptyValueContext

  -- "Eq"
  , alphaEquiv

  -- "Show"
  , pprType, pprExpr

  , fsubstTT, fsubstTE, fsubstEE
  , isSystemfType, isSystemfExpr
  , opReturnType
  ) where

import qualified Src

import JavaUtils
import PrettyUtils
import Panic

import Text.PrettyPrint.Leijen
import qualified Language.Java.Pretty (prettyPrint)

import Data.List (intersperse)
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
  | Let (Type t) (Expr t e) (e -> Expr t e)
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

  | Merge (Expr t e) (Expr t e)  -- e1 ,, e2

-- newtype Typ = HideTyp { revealTyp :: forall t. Type t } -- type of closed types

-- newtype Exp = HideExp { revealExp :: forall t e. Expr t e }

type TypeContext t    = Set.Set t
type ValueContext t e = Map.Map e (Type t)

emptyValueContext :: ValueContext t e
emptyValueContext = Map.empty

alphaEquiv :: Type Int -> Type Int -> Bool
alphaEquiv = go 0
  where
    go i (TyVar a)    (TyVar b)    = a == b
    go i (JClass c)   (JClass d)   = c == d
    go i (Fun s1 s2)  (Fun t1 t2)  = go i s1 t1 && go i s2 t2
    go i (Forall f)   (Forall g)   = go (succ i) (f i) (g i)
    go i (Product ss) (Product ts) = length ss == length ts
                                     && uncurry (go i) `all` zip ss ts
    go i (And s1 s2)  (And t1 t2)  = go i s1 t1 && go i s2 t2
    go i t1           t2           = False

pprType :: Prec -> Int -> Type Int -> Doc

pprType p i (TyVar a)     = pprTVar a

pprType p i (Fun t1 t2)  =
  parensIf p 2
    (pprType (2,PrecPlus) i t1 <+> arrow <+> pprType (2,PrecMinus) i t2)

pprType p i (Forall f)   =
  parensIf p 1
    (forall <+> pprTVar i <> dot <+>
     pprType (1,PrecMinus) (succ i) (f i))

pprType p i (Product ts) = tupled (map (pprType basePrec i) ts)


pprType p i (JClass "java.lang.Integer")   = text "Int"
pprType p i (JClass "java.lang.String")    = text "String"
pprType p i (JClass "java.lang.Boolean")   = text "Bool"
pprType p i (JClass "java.lang.Character") = text "Char"
pprType p i (JClass c)                     = text c

pprType p i (And t1 t2) =
  parensIf p 2
    (pprType (2,PrecMinus) i t1 <+>
     ampersand  <+>
     pprType (2,PrecPlus) i t2)

-- instance Show (Expr Int Int) where
--   show = show . pretty

instance Pretty (Expr Int Int) where
  pretty = pprExpr basePrec (0, 0)

pprExpr :: Prec -> (Int, Int) -> Expr Int Int -> Doc

pprExpr p (i,j) (Var x) = pprVar x

pprExpr p (i,j) (Lam t f) =
  parensIf p 2
    (hang 3 (lambda <> parens (pprVar j <+> colon <+> pprType basePrec i t) <> dot <$$>
             pprExpr (2,PrecMinus) (i, succ j) (f j)))

pprExpr p (i,j) (App e1 e2) =
  parensIf p 4
    (pprExpr (4,PrecMinus) (i,j) e1 <+> pprExpr (4,PrecPlus) (i,j) e2)

pprExpr p (i,j) (BLam f) =
  parensIf p 2
    (biglambda <> pprTVar i <> dot <+>
     pprExpr (2,PrecMinus) (succ i, j) (f i))

pprExpr p (i,j) (TApp e t) =
  parensIf p 4
    (pprExpr (4,PrecMinus) (i,j) e <+> pprType (4,PrecPlus) i t)

pprExpr p (i,j) (Lit (Src.Integer n)) = integer n
pprExpr p (i,j) (Lit (Src.String s))  = string s
pprExpr p (i,j) (Lit (Src.Boolean b)) = bool b
pprExpr p (i,j) (Lit (Src.Char c))    = char c

pprExpr p (i,j) (If e1 e2 e3)
  = parensIf p prec
      (hang 3 (text "if"   <+> pprExpr (prec,PrecMinus) (i,j) e1 <$$>
               text "then" <+> pprExpr (prec,PrecMinus) (i,j) e2 <$$>
               text "else" <+> pprExpr (prec,PrecMinus) (i,j) e3))
  where prec = 3

pprExpr p (i,j) (PrimOp e1 op e2)
  = parens (pprExpr p (i,j) e1 <+> ppr_op <+> pprExpr p (i,j) e2)
  where
    ppr_op = text (Language.Java.Pretty.prettyPrint (Src.unwrapOp op))

pprExpr p (i,j) (Tuple es) = tupled (map (pprExpr basePrec (i,j)) es)

pprExpr p i (Proj n e) =
  parensIf p 5
    (pprExpr (5,PrecMinus) i e <> dot <> char '_' <> int n)

pprExpr p (i,j) (JNewObj c args) =
  parens (text "new" <+> text c <> tupled (map (pprExpr basePrec (i,j)) args))

pprExpr p (i,j) (JMethod e m args r) = sorry "Core.pprExpr: JMethod"

pprExpr p (i,j) (JField e m r) = sorry "Core.pprExpr: JField"

pprExpr p (i,j) (Seq es) = semiBraces (map (pprExpr p (i,j)) es)

pprExpr p (i,j) (Fix f t1 t) =
  parens
    (text "fix" <+> pprVar j <+>
     parens (pprVar (succ j) <+> colon <+> pprType p i t1) <+>
     colon <+>
     pprType p i t <> dot <$$>
     indent 2 (pprExpr p (i, j + 2) (f j (j + 1))))

pprExpr p (i,j) (Let t bind body)
  = text "let" <$$>
    indent 2 (pprVar j <+> colon <+> pprType basePrec i t <+> equals <+> pprExpr p (i,succ j) bind) <$$>
    text "in" <$$>
    indent 2 (pprExpr p (i, succ j) (body j))

pprExpr p (i,j) (LetRec sigs binds body)
  = text "let" <+> text "rec" <$$>
    vcat (intersperse (text "and") (map (indent 2) ppr_binds)) <$$>
    text "in" <$$>
    ppr_body
  where
    n   = length sigs
    ids = [i..(i+n-1)]
    ppr_ids   = map pprVar ids
    ppr_sigs  = map (pprType p i) sigs
    ppr_defs  = map (pprExpr p (i, j + n)) (binds ids)
    ppr_binds = zipWith3 (\ppr_id ppr_sig ppr_def ->
                  ppr_id <+> colon <+> ppr_sig <$$> indent 2 (equals <+> ppr_def))
                  ppr_ids ppr_sigs ppr_defs
    ppr_body  = pprExpr p (i, j + n) (body ids)

pprExpr p (i,j) (Merge e1 e2) =
  parens $ pprExpr p (i,j) e1 <+> dcomma <+> pprExpr p (i,j) e2

fsubstTT :: (Int, Type Int) -> Type Int -> Type Int
fsubstTT (x,r) (TyVar a)
  | a == x                 = r
  | otherwise              = TyVar a
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
    go (JMethod (Right e)  m args ret) = JMethod (Right (go e)) m (map go args) ret
    go (JMethod (Left c) m args ret)  = JMethod (Left c)     m (map go args) ret
    go (JField (Right e) f ret)       = JField (Right (go e)) f ret
    go (JField (Left c) f ret)        = JField (Left c)     f ret
    go (Seq es)                       = Seq (map go es)
    go (Merge e1 e2)                  = Merge (go e1) (go e2)
    go (Let t bind body)              = Let t (go bind) (\e -> go (body e))
    go (LetRec sigs binds body)       = LetRec sigs (\ids -> map go (binds ids)) (\ids -> go (body ids))

isSystemfType :: Type t -> Bool
isSystemfType = sorry "Core.isSystemfType"

isSystemfExpr :: Expr t e -> Bool
isSystemfExpr = sorry "Core.isSystemfExpr"

opReturnType :: Src.Operator -> Type t
opReturnType (Src.Arith _)   = JClass "java.lang.Integer"
opReturnType (Src.Compare _) = JClass "java.lang.Boolean"
opReturnType (Src.Logic _)   = JClass "java.lang.Boolean"
