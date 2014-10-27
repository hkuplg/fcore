{-# Language RankNTypes, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module ClosureF where

import qualified Src  as S
import qualified Core as C

import JavaUtils
import Panic

import PrettyUtils
import Text.PrettyPrint.Leijen
import qualified Language.Java.Pretty (prettyPrint)
import Data.List (intersperse)

-- Closure F syntax

data Scope b t e =
      Body b
    | Kind (t -> Scope b t e)
    | Type (Type t) (e -> Scope b t e)

type TScope t = Scope (Type t) t ()

type EScope t e = Scope (Expr t e) t e

data Type t =
      TVar t
    | Forall (TScope t)
    | JClass ClassName
    | CFInt
    | CFInteger
    | TupleType [Type t]

data Expr t e =
     Var e
   | FVar Int
   | Lam (EScope t e)
   | App (Expr t e) (Expr t e)
   | TApp (Expr t e) (Type t)
   | PrimOp (Expr t e) S.Operator (Expr t e)
   | Lit S.Lit
   | If (Expr t e) (Expr t e) (Expr t e)
   | Tuple [Expr t e]
   | Proj Int (Expr t e)
   | Let (Expr t e) (e -> Expr t e)
   -- fixpoints
   | LetRec [Type t] ([e] -> [Expr t e]) ([e] -> Expr t e)
   | Fix (Type t) (e -> EScope t e)
   -- Java
   | JNewObj ClassName [Expr t e]
   | JMethod (Either ClassName (Expr t e)) MethodName [Expr t e] ClassName
   | JField  (Either ClassName (Expr t e)) FieldName ClassName
   | SeqExprs [Expr t e]


-- System F to Closure F

ftyp2scope :: C.Type t -> TScope t
ftyp2scope (C.Forall f)   = Kind (\a -> ftyp2scope (f a))
ftyp2scope (C.Fun t1 t2)  = Type (ftyp2ctyp t1) (\_ -> ftyp2scope t2)
ftyp2scope t             = Body (ftyp2ctyp t)
-- ftyp2scope PFInt         = Body CInt
-- ftyp2scope (FTVar x)     = Body (CTVar x)

{-
ftyp2ctyp2 :: C.Type Int -> [t] -> Type t
ftyp2ctyp2 = sorry "ClosureF.ftyp2ctyp2"
-}

ftyp2ctyp :: C.Type t -> Type t
ftyp2ctyp (C.TVar x)                     = TVar x
ftyp2ctyp (C.JClass "java.lang.Integer") = CFInt
ftyp2ctyp (C.JClass c)                   = JClass c
ftyp2ctyp (C.Product ts)                 = TupleType (map ftyp2ctyp ts)
ftyp2ctyp t                              = Forall (ftyp2scope t)

{-
fexp2cexp2 :: C.Expr Int (Int,F.Type Int) -> [t] -> [e] -> Expr t e
fexp2cexp2 (C.Var _ t) tenv env = CVar (env !! fst t)
fexp2cexp2 e tenv env = CLam (groupLambda2 e tenv env)
-}

{-
fexp2cexp2 :: C.Expr t (e, Type t) -> (Expr t e, Type t)
fexp2cexp2 (C.Var _ (x,t))      = (CVar x,t)
fexp2cexp2 (C.App e1 e2)        =
   let (c1,CForall (Typ t g))  = fexp2cexp2 e1
       (c2,t2)                 = fexp2cexp2 e2
   in (CApp c1 c2, undefined (g ()))
fexp2cexp2 (C.TApp e t)   =
   let (c1,t1) = fexp2cexp e
CTApp (fexp2cexp e) (ftyp2ctyp t)
-}


fexp2cexp :: C.Expr t e -> Expr t e
fexp2cexp (C.Var x)                  = Var x
fexp2cexp (C.App e1 e2)              = App (fexp2cexp e1) (fexp2cexp e2)
fexp2cexp (C.TApp e t)               = TApp (fexp2cexp e) (ftyp2ctyp t)
fexp2cexp (C.PrimOp e1 op e2)        = PrimOp (fexp2cexp e1) op (fexp2cexp e2)
fexp2cexp (C.Lit e) = Lit e
fexp2cexp (C.If e1 e2 e3)            = If (fexp2cexp e1) (fexp2cexp e2) (fexp2cexp e3)
fexp2cexp (C.Tuple tuple)            = Tuple (map fexp2cexp tuple)
fexp2cexp (C.Proj i e)               = Proj i (fexp2cexp e)
fexp2cexp (C.Let bind body)          = Let (fexp2cexp bind) (\e -> fexp2cexp $ body e)
fexp2cexp (C.LetRec ts f g) = LetRec (map ftyp2ctyp ts) (\decls -> map fexp2cexp (f decls)) (\decls -> fexp2cexp (g decls))
fexp2cexp (C.Fix f t1 t2) =
  let  g e = groupLambda (C.Lam t1 (f e)) -- is this right???? (BUG)
  in   Fix (Forall (adjust (C.Fun t1 t2) (g undefined))) g
fexp2cexp (C.JNew cName args)     = JNewObj cName (map fexp2cexp args)
fexp2cexp (C.JMethod c mName args r) =
  case c of (S.NonStatic ce) -> JMethod (Right $ fexp2cexp ce) mName (map fexp2cexp args) r
            (S.Static cn)    -> JMethod (Left cn) mName (map fexp2cexp args) r
fexp2cexp (C.JField c fName r) =
  case c of (S.NonStatic ce) -> JField (Right $ fexp2cexp ce) fName r
            (S.Static cn)    -> JField (Left cn) fName r
fexp2cexp (C.Seq es)            = SeqExprs (map fexp2cexp es)
fexp2cexp e                         = Lam (groupLambda e)

adjust :: C.Type t -> EScope t e -> TScope t
adjust (C.Fun t1 t2) (Type t1' g) = Type t1' (\_ -> adjust t2 (g undefined)) -- not very nice!
adjust (C.Forall f) (Kind g)     = Kind (\t -> adjust (f t) (g t))
adjust t (Body b)               = Body (ftyp2ctyp t)
adjust _ _ = sorry "ClosureF.adjust: no idea how to do"

{-
groupLambda2 :: C.Expr Int (Int,F.Type Int) -> [t] -> [e] -> EScope t e
groupLambda2 (FBLam f) tenv env = Kind (\a -> groupLambda2 (f (length tenv)) (a:tenv) env)
groupLambda2 (FLam t f) tenv env =
  Typ (ftyp2ctyp2 t tenv) (\x -> groupLambda2 (f (length env,t)) tenv (x:env))
-}

groupLambda :: C.Expr t e -> EScope t e
groupLambda (C.BLam f)  = Kind (\a -> groupLambda (f a))
groupLambda (C.Lam t f) = Type (ftyp2ctyp t) (\x -> groupLambda (f x))
groupLambda e          = Body (fexp2cexp e)

-- join

scope2ctyp :: TScope t -> Type t
scope2ctyp (Body t)  = t
scope2ctyp s         = Forall s

joinType :: Type (Type t) -> Type t
joinType (TVar t)   = t
joinType CFInt = CFInt
joinType CFInteger = CFInteger
joinType (Forall s) = Forall (joinTScope s)
joinType (JClass c) = JClass c
joinType (TupleType ts) = TupleType (map joinType ts)

joinTScope :: TScope (Type t) -> TScope t
joinTScope (Body b)   = Body (joinType b)
joinTScope (Kind f)   = Kind (joinTScope . f . TVar)
joinTScope (Type t f) = let t' = joinType t in Type t' (\x -> joinTScope (f x))

-- Free variable substitution

substScope :: Subst t => Int -> Type Int -> Scope (Type t) t () -> Scope (Type t) t ()
substScope n t (Body t1) = Body (substType n t t1)
substScope n t (Kind f)  = Kind (\a -> substScope n t (f a))
substScope n t (Type t1 f) = Type (substType n t t1) (\x -> substScope n t (f x))

substType :: Subst t => Int -> Type Int -> Type t -> Type t
substType n t (TVar x) = subst n t x
substType n t (Forall s) = Forall (substScope n t s)
substType n t x = x

class Subst t where
   subst :: Int -> Type Int -> t -> Type t

instance Subst Int where
   subst n t x
      | n == x = t
      | otherwise = TVar x

instance Subst t => Subst (Type t) where
   subst n t x = TVar (substType n t x)

-- Pretty Printing

type Index = Int

prettyTScope :: Prec -> Index -> TScope Index -> Doc

prettyTScope p i (Body t) = prettyType p i t


prettyTScope p i (Kind f) = nest 4 (text "TScope_Kind(" <$> text "\\_ -> " <> prettyTScope p i (f 0)) <$> text ")"

prettyTScope p i (Type t f) = prettyType p i t <> text " -> " <> prettyTScope p i (f ())

prettyEScope :: Prec -> Index -> EScope Index Index -> Doc

prettyEScope p i (Body t) = nest 4 (text "EScope_Body(" <$> prettyExpr p (0, 0) t) <$> text ")"

prettyEScope p i (Kind f) = nest 4 (text "EScope_Kind(" <$> text "\\_ -> " <> prettyEScope p i (f 0)) <$> text ")"

prettyEScope p i (Type t f) = backslash <> prettyVar i <> dot <+> prettyType p i t <$> prettyEScope p (succ i) (f i)

prettyType :: Prec -> Index -> Type Index -> Doc

prettyType _ _ (TVar t) = prettyTVar t

prettyType p i (Forall f) = text "Forall(" <> prettyTScope p i f <> text ")"

prettyType _ _ (JClass "java.lang.Integer") = text "Int"
prettyType _ _ (JClass "java.lang.String") = text "String"
prettyType _ _ (JClass "java.lang.Boolean") = text "Bool"
prettyType _ _ (JClass "java.lang.Character") = text "Char"
prettyType _ _ (JClass c) = text c

prettyType _ _ CFInt = text "CFInt"
prettyType _ _ CFInteger = text "CFInteger"

prettyType p i (TupleType l) = tupled (map (prettyType p i) l)

prettyExpr :: Prec -> (Index, Index) -> Expr Index Index -> Doc

prettyExpr _ _ (Var x) = prettyVar x

prettyExpr _ _ (FVar x) = text "FVar()"

prettyExpr p (i, j) (Lam e) = nest 4 (text "Lam(" <$> prettyEScope p j e) <$> text ")"

prettyExpr p i (App (Var x) y) = 
  parens (prettyExpr p i (Var x) <+> parens(prettyExpr p i y))
prettyExpr p i (App e1 e2) = 
  nest 4 (text "App(" <$> 
  prettyExpr p i e1 <> comma <$> 
  prettyExpr p i e2) <$> 
  text ")"

prettyExpr p (i, j) (TApp e t) = nest 4 (text "TApp(" <$> prettyExpr p (i, j) e <> comma <$> prettyType p i t) <$> text ")"

prettyExpr p i (PrimOp e1 op e2) 
  = prettyExpr p i e1 <+> pretty_op <+> prettyExpr p i e2
  where
    pretty_op = text (Language.Java.Pretty.prettyPrint java_op)
    java_op   = case op of
                  S.Arith   op' -> op'
                  S.Compare op' -> op'
                  S.Logic   op' -> op'


prettyExpr _ _ (Lit (S.Integer n)) = integer n
prettyExpr _ _ (Lit (S.String s))  = string s
prettyExpr _ _ (Lit (S.Boolean b)) = bool b
prettyExpr _ _ (Lit (S.Char c))    = char c

prettyExpr p i (If e1 e2 e3) = nest 4 (text "if (" <$> prettyExpr p i e1) <$> nest 4 (text ") then (" <$> prettyExpr p i e2) <$> nest 4 (text ") else (" <$> prettyExpr p i e3) <$> text ")"

prettyExpr p i (Tuple l) = tupled (map (prettyExpr p i) l)

prettyExpr p i (Proj n e) = parens (prettyExpr p i e) <> text "._" <> int n


prettyExpr p (i, j) (LetRec sigs binds body)
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


prettyExpr p (i, j) (Let _ _) = text "Let()"
prettyExpr p (i, j) (LetRec _ _ _) = text "LetRec()"

prettyExpr p (i, j) (Fix t f) = 
  nest 4 (text "Fix(" <$> 
  backslash <> prettyVar j <> dot <+> prettyType p i t <> comma <$> 
  prettyEScope p (succ j) (f j)) <$> 
  text ")"

prettyExpr p i (JNewObj name l) = parens (text "new" <+> text name <> tupled (map (prettyExpr p i) l))

prettyExpr p i (JMethod name m args r) = methodStr name <> dot <> text m <> tupled (map (prettyExpr basePrec i) args)
  where
    methodStr (Left x) = text x
    methodStr (Right x) = prettyExpr (6,PrecMinus) i x

prettyExpr p i (JField name f r) = fieldStr name <> dot <> text f
  where
    fieldStr (Left x) = text x
    fieldStr (Right x) = prettyExpr (6,PrecMinus) i x

prettyExpr p i (SeqExprs l) = semiBraces (map (prettyExpr p i) l)


