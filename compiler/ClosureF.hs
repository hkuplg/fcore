{-# Language RankNTypes, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module ClosureF where

import qualified Src  as S
import qualified Core as C

import JavaUtils
import Panic

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
ftyp2ctyp (C.TyVar x) = TVar x
ftyp2ctyp (C.JClass "java.lang.Integer") = CFInt
ftyp2ctyp (C.JClass c) = JClass c
ftyp2ctyp (C.Product ts) = TupleType (map ftyp2ctyp ts)
ftyp2ctyp t         = Forall (ftyp2scope t)

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
fexp2cexp (C.JNewObj cName args)     = JNewObj cName (map fexp2cexp args)
fexp2cexp (C.JMethod c mName args r) =
  case c of (Right ce)  -> JMethod (Right $ fexp2cexp ce) mName (map fexp2cexp args) r
            (Left cn) -> JMethod (Left cn) mName (map fexp2cexp args) r
fexp2cexp (C.JField c fName r) =
  case c of (Right ce)  -> JField (Right $ fexp2cexp ce) fName r
            (Left cn) -> JField (Left cn) fName r
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

showType :: Type Int -> Int -> String
showType (TVar i) n = "a" ++ show i
showType (Forall s) n = "(forall " ++ showTScope s n ++ ")"
showType (CFInt) n = "Int"
showType (CFInteger) n = "Int"
-- showType (CJClass "java.lang.Integer") n = "Int"
showType (JClass c) n                   = c

showType (TupleType ts) n = sorry "ClosureF.showType: TupleType"

showTScope :: Scope (Type Int) Int () -> Int -> String
showTScope (Body t) n = ". " ++ showType t n
showTScope (Kind f) n = "a" ++ show n ++ " " ++ showTScope (f n) (n+1)
showTScope (Type t f) n = "(_ : " ++ showType t n ++ ") " ++ showTScope (f ()) n

showEScope :: EScope Int Int -> Int -> String
showEScope (Body t) n = ". " ++ showExpr t n
showEScope (Kind f) n = "a" ++ show n ++ " " ++ showEScope (f n) (n+1)
showEScope (Type t f) n = "(x" ++ show n ++ " : " ++ showType t n ++ ") " ++ showEScope (f n) (n+1)

showExpr :: Expr Int Int -> Int -> String
showExpr (Var x) n      = "x" ++ show x
showExpr (Lam s) n      = "(\\" ++ showEScope s n ++ ")"
showExpr (App e1 e2) n  = showExpr e1 n ++ " " ++ showExpr e2 n
showExpr (TApp e t) n   = showExpr e n ++ " " ++ showType t n
showExpr (Fix t f) n    = sorry "ClosureF.showExpr: Fix"
showExpr _ _   = sorry "ClosureF.showExpr: no idea how to do"

instance Show (Type Int) where
   show e = showType e 0

instance Show (TScope Int) where
   show e = showTScope e 0

instance Show (EScope Int Int) where
   show e = showEScope e 0

instance Show (Expr Int Int) where
   show e = showExpr e 0
