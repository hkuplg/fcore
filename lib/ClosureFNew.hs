{-# Language RankNTypes, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{- |
Module      :  ClosureFNew
Description :   Abstract syntax and pretty printer for ClosureFNew.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Jeremy <bixuanxbi@gmail.com>, Tomas <tomtau@connect.hku.hk>
Stability   :  stable
Portability :  non-portable (MPTC)

-}
module ClosureFNew where

import qualified CoreNew as C
import           JavaUtils
import           Panic
import           PrettyUtils
import qualified Src as S
import qualified Language.Java.Pretty (prettyPrint)
import           Prelude hiding ((<$>))
import           Text.PrettyPrint.ANSI.Leijen

-- Closure F syntax

data Scope b e =
      Body b
    | Type (Type e) (e -> Scope b e)

-- type TScope t = Scope (Type t) t
type EScope e = Scope (Expr e) e

type Type t = Expr t

data Expr e =
     Var S.ReadId e
   | Lit S.Lit

   | Lam S.ReadId (EScope e)
   | Pi S.ReadId (EScope e)
   | Mu S.ReadId (EScope e)
   | Let S.ReadId (Expr e) (e -> Expr e)
   | App (Expr e) (Expr e)

   | If (Expr e) (Expr e) (Expr e)
   | PrimOp (Expr e) S.Operator (Expr e)

   | Tuple [Expr e]
   | Proj Int (Expr e)
   | TupleType [Type e]
   | Unit
   | Star

   | JNew ClassName [Expr e]
   | JMethod (Either ClassName (Expr e)) MethodName [Expr e] ClassName
   | JField  (Either ClassName (Expr e)) FieldName (Type e)
   | JClass ClassName

   | SeqExprs [Expr e]
   | Error (Type e) (Expr e)

-- System F to Closure F

fexp2scope :: C.Expr e -> EScope e
fexp2scope (C.Pi _ t f)  = Type (fexp2cexp t) (fexp2scope . f)
fexp2scope (C.Lam _ t f) = Type (fexp2cexp t) (fexp2scope . f)
fexp2scope (C.Mu _ t f)  = Type (fexp2cexp t) (fexp2scope . f)
fexp2scope e             = Body (fexp2cexp e)

fexp2cexp :: C.Expr e -> Expr e -- TODO: Datatypes missing
fexp2cexp (C.JClass c)               = JClass c
fexp2cexp (C.Product ts)             = TupleType (map fexp2cexp ts)
fexp2cexp (C.Unit)                   = Unit
fexp2cexp (C.Star)                   = Star
fexp2cexp t@(C.Pi n _ _)             = Pi n (fexp2scope t)
fexp2cexp e@(C.Lam n _ _)            = Lam n (fexp2scope e)
fexp2cexp e@(C.Mu n _ _)             = Mu n (fexp2scope e)
fexp2cexp (C.Var rid x)              = Var rid x
fexp2cexp (C.App e1 e2)              = App (fexp2cexp e1) (fexp2cexp e2)
fexp2cexp (C.PrimOp e1 op e2)        = PrimOp (fexp2cexp e1) op (fexp2cexp e2)
fexp2cexp (C.Lit S.UnitLit)          = Lit S.UnitLit
fexp2cexp (C.Lit e)                  = Lit e
fexp2cexp (C.If e1 e2 e3)            = If (fexp2cexp e1) (fexp2cexp e2) (fexp2cexp e3)
fexp2cexp (C.Tuple tuple)            = Tuple (map fexp2cexp tuple)
fexp2cexp (C.Proj i e)               = Proj i (fexp2cexp e)
fexp2cexp (C.Let n bind body)        = Let n (fexp2cexp bind) (fexp2cexp . body)
fexp2cexp (C.JNew cName args)        = JNew cName (map fexp2cexp args)
fexp2cexp (C.JMethod c mName args r) =
  case c of (S.NonStatic ce) -> JMethod (Right $ fexp2cexp ce) mName (map fexp2cexp args) r
            (S.Static cn)    -> JMethod (Left cn) mName (map fexp2cexp args) r
fexp2cexp (C.JField c fName r)       =
  case c of (S.NonStatic ce) -> JField (Right $ fexp2cexp ce) fName (fexp2cexp r)
            (S.Static cn)    -> JField (Left cn) fName (fexp2cexp r)
fexp2cexp (C.Error ty str)           = Error (fexp2cexp ty) (fexp2cexp str)
fexp2cexp (C.Seq es)                 = SeqExprs (map fexp2cexp es)

adjust :: C.Type t -> EScope t -> EScope t
adjust (C.Pi _ _ f) (Type t' g) = Type t' (\t -> adjust (f t) (g t))
adjust t (Body _)               = Body (fexp2cexp t)
adjust _ _ = sorry "ClosureFNew.adjust: no idea how to do"

-- join

scope2ctyp :: EScope t -> Type t
scope2ctyp (Body t)  = t
scope2ctyp s         = Pi "_" s

getArity :: EScope t -> Int
getArity (Type _ g) = 1 + getArity (g undefined) -- TODO: better choice
getArity _ = 0

joinType :: Type (Type t) -> Type t
joinType (Var _ t)   = t
joinType Unit = Unit
joinType Star = Star
joinType (Pi n s) = Pi n (joinTScope s)
joinType (JClass c) = JClass c
joinType (TupleType ts) = TupleType (map joinType ts)
joinType _ = error "ClosureFNew.joinType: not defined"

joinTScope :: EScope (Type t) -> EScope t
joinTScope (Body b)   = Body (joinType b)
joinTScope (Type t f) = Type (joinType t) (joinTScope . f . Var "")

-- Free variable substitution

substScope :: Subst t => Int -> Type Int -> EScope t  -> EScope t
substScope n t (Body t1) = Body (substType n t t1)
substScope n t (Type t1 f) = Type (substType n t t1) (substScope n t . f)

substType :: Subst t => Int -> Type Int -> Type t -> Type t
substType n t (Var _ x) = subst n t x
substType n t (Pi n' s) = Pi n' (substScope n t s)
substType _ _ x = x

class Subst t where
   subst :: Int -> Type Int -> t -> Type t

instance Subst Int where
   subst n t x
      | n == x = t
      | otherwise = Var "" x

instance Subst t => Subst (Type t) where
   subst n t x = Var "" (substType n t x)

-- Pretty Printing
-- TODO: fix pretty printing with the propagated names
type Index = Int

pretty :: Expr Index -> Doc
pretty = prettyExpr basePrec 0

isSimpleExpr :: Expr Index -> Bool
isSimpleExpr (Var _ _)         = True
isSimpleExpr (PrimOp{})  = True
isSimpleExpr (App (Var _ _) _) = True
isSimpleExpr (Lit _)         = True
isSimpleExpr _               = False

prettyScope :: Prec -> Index -> EScope Index -> Doc
prettyScope p i (Body t) = prettyExpr p i t
prettyScope p i (Type t f) = backslash <+> parens (prettyVar i <+> colon <+> prettyExpr p (i + 1) t) <> dot <$> prettyScope p (i + 1) (f i)

prettyExpr :: Prec -> Index -> Expr Index -> Doc
prettyExpr _ _ Unit = text "Unit"
prettyExpr _ _ Star = text "Star"

prettyExpr _ _ (JClass "java.lang.Integer") = text "Int"
prettyExpr _ _ (JClass "java.lang.String") = text "String"
prettyExpr _ _ (JClass "java.lang.Boolean") = text "Bool"
prettyExpr _ _ (JClass "java.lang.Character") = text "Char"
prettyExpr _ _ (JClass c) = text c

prettyExpr p i (TupleType l) = tupled (map (prettyExpr p i) l)

prettyExpr _ _ (Var _ x) = prettyVar x

prettyExpr p i (Lam _ e) = nest 2 (text "Lam(" <$> prettyScope p i e) <$> text ")"
prettyExpr p i (Mu _ e) = nest 2 (text "Mu(" <$> prettyScope p i e) <$> text ")"
prettyExpr p i (Pi _ e) = nest 2 (text "Pi(" <$> prettyScope p i e) <$> text ")"

prettyExpr p i (App (Lam _ e1) e2) =
  nest 2 (text "App(" <$>
  prettyExpr p i (Lam "" e1) <> comma <$>
  prettyExpr p i e2) <$>
  text ")"

-- To be modified. Remove useless parens.
prettyExpr p i (App e1 e2) = parensIf p 4 (prettyExpr (4, PrecMinus) i e1 <+> prettyExpr (4, PrecPlus) i e2)

prettyExpr p i (PrimOp e1 op e2)
  = parens (prettyExpr p i e1 <+> pretty_op <+> prettyExpr p i e2)
  where
    pretty_op = text (Language.Java.Pretty.prettyPrint java_op)
    java_op   = case op of
                  S.Arith   op' -> op'
                  S.Compare op' -> op'
                  S.Logic   op' -> op'


prettyExpr _ _ (Lit (S.Int n))    = integer n
prettyExpr _ _ (Lit (S.String s)) = dquotes (string s)
prettyExpr _ _ (Lit (S.Bool b))   = bool b
prettyExpr _ _ (Lit (S.Char c))   = char c
prettyExpr _ _ (Lit  S.UnitLit)   = unit

prettyExpr _ i (If e1 e2 e3)
  = ifPart <$> thenPart <$> elsePart
  where
    ifPart   = text "if" <+> prettyExpr (3, PrecMinus) i e1
    thenPart = if   isSimpleExpr e2
               then text "then" <+> prettyExpr (3, PrecMinus) i e2
               else nest 2 (text "then" <$> prettyExpr (3, PrecMinus) i e2)
    elsePart = if   isSimpleExpr e3
               then text "else" <+> prettyExpr (3, PrecMinus) i e3
               else nest 2 (text "else" <$> prettyExpr (3, PrecMinus) i e3)

prettyExpr p i (Tuple l) = tupled (map (prettyExpr p i) l)

--To be modified. Remove useless parens.
prettyExpr p i (Proj n e) = parensIf p 5 (prettyExpr (5, PrecMinus) i e <> text "._" <> int n)

prettyExpr _ i (Let _ x f) =
  text "let" <$$>
  indent 2 (prettyVar i <+> text "=" <+> prettyExpr basePrec (succ i) x) <$$>
  text "in" <$$>
  indent 2 (prettyExpr basePrec (succ i) (f i))

prettyExpr p i (JNew name l) = parens (text "new" <+> text name <> tupled (map (prettyExpr p i) l))

prettyExpr _ i (JMethod name m args _) = methodStr name <> dot <> text m <> tupled (map (prettyExpr basePrec i) args)
  where
    methodStr (Left x)  = text x
    methodStr (Right x) = prettyExpr (6,PrecMinus) i x

prettyExpr _ i (JField name f _) = fieldStr name <> dot <> text f
  where
    fieldStr (Left x)  = text x
    fieldStr (Right x) = prettyExpr (6,PrecMinus) i x

prettyExpr p i (Error _ str) = text "error:" <+> prettyExpr p i str

prettyExpr p i (SeqExprs l) = semiBraces (map (prettyExpr p i) l)

