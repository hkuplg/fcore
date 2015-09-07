{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{- |
Module      :  CoreNew
Description :  Abstract syntax and pretty printer for the New Core.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>, Haoyuan Zhang <zhanghaoyuan00@gmail.com>
Stability   :  experimental
Portability :  portable
-}

module CoreNew
  ( Type
  , Expr(..)
  , Context
  , Index
  , alphaEq
  , mapVar
  , fsubstTT
  , fsubstTE
  , fsubstEE
  , join
  , CoreNew.pi
  , var
  , lam
  , CoreNew.mu
  , CoreNew.pretty
  , javaInt
  , coreTypeToNew
  , coreExprToNew
  ) where

import qualified Src

import           JavaUtils
import           PrettyUtils
import qualified Core as C

import           Data.List (intersperse)
import qualified Data.Map as Map
import qualified Language.Java.Pretty (prettyPrint)
import           Prelude hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

-- Definition

type Type t = Expr t

data Expr e
  = Var Src.ReadId e
  | Lit Src.Lit

  -- Binders we have: λ, μ, Π
  | Lam Src.ReadId (Type e) (e -> Expr e)
  | Pi Src.ReadId (Type e) (e -> Type e) -- Pi (x : t) . e
  | Mu Src.ReadId (Type e) (e -> Expr e) -- mu (x : t) . e
  | Let Src.ReadId (Expr e) (e -> Expr e)
  | App  (Expr e) (Expr e)

  | If (Expr e) (Expr e) (Expr e)
  | PrimOp (Expr e) Src.Operator (Expr e)
      -- SystemF extension from:
      -- https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf
      -- (no int restriction)

  | Tuple [Expr e]     -- Tuple introduction
  | Proj Int (Expr e)  -- Tuple elimination
  | Product [Type e]   -- Type of tuple. TODO: make it dependent?
  | Unit
  | Star

  -- Java
  | JNew ClassName [Expr e]
  | JMethod (Src.JReceiver (Expr e)) MethodName [Expr e] ClassName
  | JField  (Src.JReceiver (Expr e)) FieldName (Type e)
  | JClass ClassName

  | Seq [Expr e]
  | Error (Type e) (Expr e)

type Context e = Map.Map e (Type e)
type Index = Int

-- Translation

coreTypeToNew :: C.Type t -> Type t -- TODO: Datatypes missing
coreTypeToNew (C.TVar n x)   = Var n x
coreTypeToNew (C.JClass c)   = JClass c
coreTypeToNew (C.Fun t1 t2)  = Pi "_" (coreTypeToNew t1) (\_ -> coreTypeToNew t2)
coreTypeToNew (C.Forall n f) = Pi n Star (coreTypeToNew . f)
coreTypeToNew (C.Product ts) = Product (map coreTypeToNew ts)
coreTypeToNew C.Unit         = Unit
coreTypeToNew _ = error "CoreNew.coreTypeToNew: translation not defined"

coreExprToNew :: C.Expr e e -> Expr e -- TODO: Datatypes missing
coreExprToNew = transExpr
  where transType = coreTypeToNew
        transExpr (C.Var n e) = Var n e
        transExpr (C.Lit l) = Lit l
        transExpr (C.Lam n t f) = Lam n (transType t) (transExpr . f)
        transExpr (C.Fix n1 n2 f t1 t) = Mu n1 (transType (C.Fun t1 t))
                                         (\e -> Lam n2 (transType t) (transExpr . f e))
        transExpr (C.Let n b e) = Let n (transExpr b) (transExpr . e)
        -- TODO: translate letrec for mutual recursion
        transExpr (C.LetRec ns ts bi bo) = let n = head ns
                                               t = head ts
                                             in Let n (Mu n (transType t) (\e -> transExpr . head . bi  $ [e]))
                                                (\e -> transExpr . bo $ [e])
        transExpr (C.BLam n f) = Lam n Star (transExpr . f)
        transExpr (C.App f e) = App (transExpr f) (transExpr e)
        transExpr (C.TApp f e) = App (transExpr f) (transType e)
        transExpr (C.If e1 e2 e3) = If (transExpr e1) (transExpr e2) (transExpr e3)
        transExpr (C.PrimOp e1 op e2) = PrimOp (transExpr e1) op (transExpr e2)
        transExpr (C.Tuple es) = Tuple (map transExpr es)
        transExpr (C.Proj i e) = Proj i (transExpr e)
        transExpr (C.JNew n es) = JNew n (map transExpr es)
        transExpr (C.JMethod callee m args c) = JMethod (fmap transExpr callee) m (map transExpr args) c
        transExpr (C.JField callee n t) = JField (fmap transExpr callee) n (transType t)
        transExpr (C.Seq es) = Seq (map transExpr es)
        transExpr (C.Error t e) = Error (transType t) (transExpr e)
        transExpr _ = error "CoreNew.coreExprToNew: translation not defined"

-- Utilities

alphaEq :: Int -> Type Index -> Type Index -> Bool
alphaEq _ (Var _ a)   (Var _ b)     = a == b
alphaEq i (Pi _ t1 f) (Pi _ t2 g)   = alphaEq (succ i) (f i) (g i) && alphaEq i t1 t2
alphaEq i (Mu _ t1 f) (Mu _ t2 g)   = alphaEq (succ i) (f i) (g i) && alphaEq i t1 t2
alphaEq _ (JClass c)   (JClass d)   = c == d
alphaEq i (Product ss) (Product ts) = length ss == length ts && uncurry (alphaEq i) `all` zip ss ts
alphaEq _  Unit         Unit        = True
alphaEq _  _            _           = False

mapVar :: (Src.ReadId -> e -> Expr e) -> (Type e -> Type e) -> Expr e -> Expr e
mapVar g _ (Var n a)                 = g n a
mapVar _ _ (Lit n)                   = Lit n
mapVar g h (Lam n t f)               = Lam n (h t) (mapVar g h . f)
mapVar g h (Pi n t f)                = Pi n (h t) (mapVar g h . f)
mapVar g h (Mu n t f)                = Mu n (h t) (mapVar g h . f)
mapVar g h (Let n b e)               = Let n (mapVar g h b) (mapVar g h . e)
mapVar g h (App f e)                 = App (mapVar g h f) (mapVar g h e)
mapVar g h (If p b1 b2)              = If (mapVar g h p) (mapVar g h b1) (mapVar g h b2)
mapVar g h (PrimOp e1 op e2)         = PrimOp (mapVar g h e1) op (mapVar g h e2)
mapVar g h (Tuple es)                = Tuple (map (mapVar g h) es)
mapVar g h (Proj i e)                = Proj i (mapVar g h e)
mapVar g h (Product ts)              = Product (map (mapVar g h) ts)
mapVar _ _ Unit                      = Unit
mapVar _ _ Star                      = Star
mapVar g h (JNew c args)             = JNew c (map (mapVar g h) args)
mapVar g h (JMethod callee m args c) = JMethod (fmap (mapVar g h) callee) m (map (mapVar g h) args) c
mapVar g h (JField  callee f c)      = JField (fmap (mapVar g h) callee) f (h c)
mapVar _ _ (JClass c)                = JClass c
mapVar g h (Seq es)                  = Seq (map (mapVar g h) es)
mapVar g h (Error ty str)            = Error (h ty) (mapVar g h str)

fsubstEE :: Eq a => a -> Expr a -> Expr a -> Expr a
fsubstEE x r = mapVar (\n a -> if a == x then r else Var n a) id

fsubstTE :: Eq t => t -> Type t -> Expr t -> Expr t
fsubstTE x r = mapVar Var (fsubstTT x r)

fsubstTT :: Eq a => a -> Type a -> Type a -> Type a
fsubstTT = fsubstEE

join :: Type (Type t) -> Type t
join (Var _ a)    = a
join (Pi n t f)   = Pi n (join t) (join . f . Var "_")
join (Mu n t f)   = Mu n (join t) (join . f . Var "_")
join (JClass c)   = JClass c
join (Product ts) = Product (map join ts)
join  Unit        = Unit
join  Star        = Star
join _            = error "CoreNew.join: behavior not defined"

pi :: Type t -> (t -> Type t) -> Type t
pi = Pi "_"

var :: e -> Expr e
var = Var "_"

lam :: Type e -> (e -> Expr e) -> Expr e
lam = Lam "_"

mu :: Type e -> (e -> Expr e) -> Expr e
mu = Mu "_"

-- instance Show (Type Index) where
--   show = show . pretty

-- instance Pretty (Type Index) where
--   pretty = prettyType

pretty :: Expr Index -> Doc
pretty = pretty' basePrec 0

pretty' :: Prec -> Index -> Expr Index -> Doc
pretty' _ _ (Var n _)   = text n
pretty' _ i (Product ts) = parens $ hcat (intersperse comma (map (pretty' basePrec i) ts))
pretty' _ _  Unit = text "Unit"
pretty' _ _  Star = text "*"
pretty' _ _ (JClass "java.lang.Integer")   = text "Int"
pretty' _ _ (JClass "java.lang.String")    = text "String"
pretty' _ _ (JClass "java.lang.Boolean")   = text "Bool"
pretty' _ _ (JClass "java.lang.Character") = text "Char"
pretty' _ _ (JClass c)                     = text c

pretty' p i (Lam n t f)
  = parensIf p 2 $ group $ hang 2 $
      lambda <+> parens (text n <+> colon <+> pretty' basePrec i t) <+> text "->" <$>
      pretty' (2,PrecMinus) (i + 1) (f i)

pretty' p i (Pi n t f)
  = parensIf p 2 $ group $ hang 2 $
      (if n == "_" then pretty' basePrec i t else parens (text n <+> colon <+> pretty' basePrec i t)) <+> text "->" <$>
      pretty' (2,PrecMinus) (i + 1) (f i)

pretty' p i (Mu n t f)
  = parensIf p 2 $ group $ hang 2 $
      PrettyUtils.mu <+> parens (text n <+> colon <+> pretty' basePrec i t) <+> text "->" <$>
      pretty' (2,PrecMinus) (i + 1) (f i)

pretty' p i (App e1 e2)
  = parensIf p 4 $
      group $ hang 2 $ pretty' (4,PrecMinus) i e1 <$> pretty' (4,PrecPlus) i e2

pretty' _ _ (Lit (Src.Int n))    = integer n
pretty' _ _ (Lit (Src.String s)) = dquotes (string s)
pretty' _ _ (Lit (Src.Bool b))   = bool b
pretty' _ _ (Lit (Src.Char c))   = char c
pretty' _ _ (Lit Src.UnitLit)    = unit

pretty' p i (If e1 e2 e3)
  = parensIf p prec
      (hang 3 (text "if"   <+> pretty' (prec,PrecMinus) i e1 <+>
               text "then" <+> pretty' (prec,PrecMinus) i e2 <+>
               text "else" <+> pretty' (prec,PrecMinus) i e3))
  where prec = 3

pretty' p i (PrimOp e1 op e2)
  = parens (pretty' p i e1 <+> pretty_op <+> pretty' p i e2)
  where
    pretty_op = text (Language.Java.Pretty.prettyPrint java_op)
    java_op   = case op of
                  Src.Arith   op' -> op'
                  Src.Compare op' -> op'
                  Src.Logic   op' -> op'

pretty' _ i (Tuple es) = tupled (map (pretty' basePrec i) es)

pretty' p i (Proj n e) =
  parensIf p 5
    (pretty' (5,PrecMinus) i e <> dot <> char '_' <> int n)

pretty' _ i (JNew c args) =
  parens (text "new" <+> text c <> tupled (map (pretty' basePrec i) args))

pretty' _ i (JMethod name m args _) = methodStr name <> dot <> text m <> tupled (map (pretty' basePrec i) args)
  where
    methodStr (Src.Static x) = text x
    methodStr (Src.NonStatic x) = pretty' (6,PrecMinus) i x

pretty' _ i (JField name f _) = fieldStr name <> dot <> text f
  where
    fieldStr (Src.Static x) = text x
    fieldStr (Src.NonStatic x) = pretty' (6,PrecMinus) i x

pretty' p i (Error _ str) = text "error:" <+> pretty' p i str

pretty' p i (Seq es) = semiBraces (map (pretty' p i) es)

pretty' _ i (Let n b e) =
  text "let" <+> text n <+> equals <+> pretty' basePrec (succ i) b <$> text "in" <$>
  pretty' basePrec (succ i) (e i)

javaInt :: Type t
javaInt = JClass "java.lang.Integer"
