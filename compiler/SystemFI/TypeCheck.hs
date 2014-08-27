{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-unused-matches #-}
{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module SystemFI.TypeCheck where

import SystemFI.Syntax

import qualified SystemF.Syntax  as F
import qualified ESF.Syntax      as E

import Mixins

import Unsafe.Coerce

import Control.Monad.Identity
import Control.Monad.State

takeFreshIndex :: State Int Int
takeFreshIndex =
  do i <- get
     put (i + 1)
     return i

subtype :: Type Int -> Type Int -> Bool
subtype t1 t2 = runIdentity (new subtypeM 0 t1 t2)

subtypeM:: Monad m => Open (Int -> Type Int -> Type Int -> m Bool)
subtypeM this i (TVar a)    (TVar b)    = return (a == b)
subtypeM this i (Forall f)  (Forall g)  = this (i + 1)(f i) (g i)
subtypeM this i (Fun t1 t2) (Fun t3 t4) = liftM2 (&&) (this i t3 t1) (this i t2 t4)
subtypeM this i t              (And t1 t2) = liftM2 (&&) (this i t  t1) (this i t  t2)
subtypeM this i (And t1 t2) t              = liftM2 (||) (this i t1 t)  (this i t2 t)
subtypeM this i _           _                 = error "subtypeM"

infer :: Expr t e -> PrettyPHOAS (Type Int)
infer = PrettyPHOAS . runIdentity . new inferM 0 . unsafeCoerce

inferM:: Monad m => Open (Int -> Expr Int (Type Int) -> m (Type Int, Int))
inferM this i (Var x)       = return (x, i)
inferM this i (Lit (E.Integer _)) = return (JClass "java.lang.Integer", i)
inferM this i (Lit (E.String _))  = return (JClass "java.lang.String", i)
inferM this i (Lit (E.Boolean _)) = return (JClass "java.lang.Boolean", i)
inferM this i (Lit (E.Char _))    = return (JClass "java.lang.Character", i)
inferM this i (BLam f)      = do (t, _) <- this (i + 1) (f i)
                                 return (Forall (\a -> fsubstTT (i, TVar a) t), i)
inferM this i (Lam t f)     = do (t', _) <- this i (f t)
                                 return (Fun t t', i)
inferM this i (TApp e t)    = do (t', i') <- this i e
                                 case t' of
                                   Forall f -> return (fsubstTT (i', t) (f i'), i' + 1)
                                   _        -> fail ""
inferM this i (App e1 e2)   = do (t1, _) <- this i e1
                                 (t2, _) <- this i e2
                                 case t1 of
                                   Fun t t'| t2 `subtype` t -> return (t', i)
                                   _                        -> fail ""
inferM this i (Merge e1 e2) = do (t1, _) <- this i e1
                                 (t2, _) <- this i e2
                                 return (And t1 t2, i)

transType :: Int -> Type Int -> F.Type Int
transType i (TVar a)      = F.TVar a
transType i (JClass c)    = F.JClass c
transType i (a1 `Fun` a2) = transType i a1 `F.Fun` transType i a2
transType i (a1 `And` a2) = F.Product [transType i a1, transType i a2]
transType i (Forall f)    = F.Forall (\a -> transType (i + 1) (f i)) -- bug!


transType2 :: Type t -> F.Type t
transType2 (TVar a)      = F.TVar a
transType2 (JClass c)    = F.JClass c
transType2 (a1 `Fun` a2) = transType2 a1 `F.Fun` transType2 a2
transType2 (a1 `And` a2) = F.Product [transType2 a1, transType2 a2]
transType2 (Forall f)    = F.Forall (transType2 . f)

coerce :: Int -> Type Int -> Type Int -> Maybe (F.Expr Int Int)
coerce  i t1@(JClass c1) (JClass c2)
  | c1 == c2  = return (F.Lam (transType i t1) (\x -> F.Var "" x))
  | otherwise = Nothing
coerce  i t1@(TVar a)    (TVar b) | a == b = return (F.Lam (transType i t1) (\x -> F.Var "" x)) -- requires variable identity
coerce  i t1@(Forall f)  (Forall g)  =
  do c <- coerce (i + 1) (f i) (g i)
     return (F.Lam (transType i t1) (\x -> F.BLam (\a -> c `F.App` (F.Var "" x `F.TApp` F.TVar a))))
coerce  i f1@(Fun t1 t2) (Fun t3 t4) =
  do c1 <- coerce  i t3 t1
     c2 <- coerce  i t2 t4
     return (F.Lam (transType i f1) (\f -> F.Lam (transType i t3) (\x -> c2 `F.App` (F.Var "" f `F.App` (c1 `F.App` F.Var "" x)))))
coerce  i t1 (And t2 t3) =
  do c1 <- coerce  i t1 t2
     c2 <- coerce  i t1 t3
     return (F.Lam (transType i t1) (\x -> F.Tuple [c1 `F.App` F.Var "" x, c2 `F.App` F.Var "" x]))
coerce  i t@(And t1 t2) t3 =
  case coerce i t1 t3 of
    Nothing -> case coerce i t2 t3 of
                 Nothing -> Nothing
                 Just c -> return (F.Lam (transType i t1) (\x -> c `F.App` F.Proj 2 (F.Var "" x)))
    Just c -> return (F.Lam (transType i t1) (\x -> c `F.App` F.Proj 1 (F.Var "" x)))
coerce  i _ _ = Nothing

translate:: Expr Int (Int, Type Int) -> State Int (Type Int, F.Expr Int Int)
translate (Var (x, t)) = return (t, F.Var "" x)
translate (Lit (E.Integer n)) = return (JClass "java.lang.Integer",   F.Lit (E.Integer n))
translate (Lit (E.String s))  = return (JClass "java.lang.String",    F.Lit (E.String s))
translate (Lit (E.Boolean b)) = return (JClass "java.lang.Boolean",   F.Lit (E.Boolean b))
translate (Lit (E.Char c))    = return (JClass "java.lang.Character", F.Lit (E.Char c))
translate (BLam f) =
  do i <- takeFreshIndex
     (t, m) <- translate (f i)
     return (Forall (\a -> fsubstTT (i, TVar a) t), F.BLam (\a -> F.fsubstEE (i, F.Var "" a) m))
translate (Lam t f) =
  do i <- takeFreshIndex
     (t1, m) <- translate (f (i, t))
     return (Fun t t1, F.Lam (transType i t) (\x -> F.fsubstEE (i, F.Var "" x) m))
translate (TApp e t) =
  do i <- takeFreshIndex
     (t1, m) <- translate e
     case t1 of
       Forall f -> return (fsubstTT (i, t) (f i), F.TApp m (transType i t))
       _        -> fail ""
translate (App e1 e2)  =
  do (t1, m1) <- translate e1
     case t1 of
        Fun t t'  ->
          do (t2, m2) <- translate e2
             i <- get
             case coerce i t2 t of
               Just c  -> return (t', m1 `F.App` (c `F.App` m2))
               Nothing -> fail ""
        _            -> fail ""

translate (PrimOp e1 op e2) =
 do (t1, m1) <- translate e1
    (t2, m2) <- translate e2
    return (And t1 t2, F.PrimOp m1 op m2)
translate (If pred b1 b2) =
  do (predTy, pred') <- translate pred
     (b1Ty, b1')     <- translate b1
     (b2Ty, b2')     <- translate b2
     return (b1Ty, F.If pred' b1' b2')
translate (Tuple es) =
  do (ts, es') <- mapAndUnzipM translate es
     return (Product ts, F.Tuple es')
translate (Proj i e) =
  do (Product ts, e') <- translate e
     return (ts !! (i-1), F.Proj i e')
translate (Merge e1 e2) =
  do (t1, m1) <- translate e1
     (t2, m2) <- translate e2
     return (And t1 t2, F.Tuple [m1, m2])
translate (JNewObj c es) =
  do (ts, es') <- mapAndUnzipM translate es
     return (JClass c, F.JNewObj c es')

data CoercionPrimitive
  = IntOfNat
  | TakeFst
  | TakeSnd
  -- f ↪ C2 (λ(x : t). f (C1 x))
  | EtaExpand (F.Type Int, Coercion) -- C1
              Coercion -- C2
  | BigEtaExpand
  | Clone Coercion Coercion
  deriving (Show)

type Coercion = [CoercionPrimitive]

applyCoercion :: Coercion -> F.Expr Int Int -> F.Expr Int Int
applyCoercion [] e                        = e
applyCoercion (TakeFst:cs) e              = F.Proj 1 (applyCoercion cs e)
applyCoercion (TakeSnd:cs) e              = F.Proj 2 (applyCoercion cs e)
applyCoercion (EtaExpand (t, c1) c2:cs) e = applyCoercion c2 (F.Lam t (\x -> e `F.App` applyCoercion c1 (F.Var "" x)))
applyCoercion (BigEtaExpand:cs) e         = F.BLam (\a -> e `F.TApp` F.TVar a)
applyCoercion (Clone c1 c2:cs) e          = F.Tuple [applyCoercion c1 e, applyCoercion c2 e]

genCoercion :: Int -> Type Int -> Type Int -> Maybe Coercion
genCoercion i (TVar a) (TVar b) | a == b = return []
genCoercion i t           (And t1 t2) =
  case genCoercion i t t1 of
    Just c  -> return (TakeFst:c)
    Nothing -> case genCoercion i t t2 of
                 Just c  -> return (TakeSnd:c)
                 Nothing -> Nothing
genCoercion i (And t1 t2) t =
  do c1 <- genCoercion i t1 t
     c2 <- genCoercion i t2 t
     return [Clone c1 c2]
genCoercion i (Fun t1 t2) (Fun t3 t4) =
  do c1 <- genCoercion i t3 t1
     c2 <- genCoercion i t2 t4
     return (if null c1 && null c2
                then []
                else [EtaExpand (transType i t3, c1) c2]) -- TODO
genCoercion i (Forall f) (Forall g)  =
  do c <- genCoercion (i + 1) (f i) (g i)
     return (if null c
                then []
                else BigEtaExpand:c)
genCoercion i _ _ = Nothing

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return
