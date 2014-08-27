{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-unused-matches #-}
{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module SystemFI.TypeCheck where

import qualified SystemF.Syntax  as F
import qualified SystemFI.Syntax as FI
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

subtype :: FI.Type Int -> FI.Type Int -> Bool
subtype t1 t2 = runIdentity (new subtypeM 0 t1 t2)

subtypeM:: Monad m => Open (Int -> FI.Type Int -> FI.Type Int -> m Bool)
subtypeM this i (FI.TVar a)    (FI.TVar b)    = return (a == b)
subtypeM this i (FI.Forall f)  (FI.Forall g)  = this (i + 1)(f i) (g i)
subtypeM this i (FI.Fun t1 t2) (FI.Fun t3 t4) = liftM2 (&&) (this i t3 t1) (this i t2 t4)
subtypeM this i t              (FI.And t1 t2) = liftM2 (&&) (this i t  t1) (this i t  t2)
subtypeM this i (FI.And t1 t2) t              = liftM2 (||) (this i t1 t)  (this i t2 t)
subtypeM this i _           _                 = error "subtypeM"

infer :: FI.Expr t e -> FI.PrettyPHOAS (FI.Type Int)
infer = FI.PrettyPHOAS . runIdentity . new inferM 0 . unsafeCoerce

inferM:: Monad m => Open (Int -> FI.Expr Int (FI.Type Int) -> m (FI.Type Int, Int))
inferM this i (FI.Var x)       = return (x, i)
inferM this i (FI.Lit (E.Integer _)) = return (FI.JClass "java.lang.Integer", i)
inferM this i (FI.Lit (E.String _))  = return (FI.JClass "java.lang.String", i)
inferM this i (FI.Lit (E.Boolean _)) = return (FI.JClass "java.lang.Boolean", i)
inferM this i (FI.Lit (E.Char _))    = return (FI.JClass "java.lang.Character", i)
inferM this i (FI.BLam f)      = do (t, _) <- this (i + 1) (f i)
                                    return (FI.Forall (\a -> FI.fsubstTT (i, FI.TVar a) t), i)
inferM this i (FI.Lam t f)     = do (t', _) <- this i (f t)
                                    return (FI.Fun t t', i)
inferM this i (FI.TApp e t)    = do (t', i') <- this i e
                                    case t' of
                                      FI.Forall f -> return (FI.fsubstTT (i', t) (f i'), i' + 1)
                                      _        -> fail ""
inferM this i (FI.App e1 e2)   = do (t1, _) <- this i e1
                                    (t2, _) <- this i e2
                                    case t1 of
                                      FI.Fun t t'| t2 `subtype` t -> return (t', i)
                                      _                        -> fail ""
inferM this i (FI.Merge e1 e2) = do (t1, _) <- this i e1
                                    (t2, _) <- this i e2
                                    return (FI.And t1 t2, i)

transType :: Int -> FI.Type Int -> F.Type Int
transType i (FI.TVar a)      = F.TVar a
transType i (FI.JClass c)    = F.JClass c
transType i (a1 `FI.Fun` a2) = transType i a1 `F.Fun` transType i a2
transType i (a1 `FI.And` a2) = F.Product [transType i a1, transType i a2]
transType i (FI.Forall f)    = F.Forall (\a -> transType (i + 1) (f i)) -- bug!


transType2 :: FI.Type t -> F.Type t
transType2 (FI.TVar a)      = F.TVar a
transType2 (FI.JClass c)    = F.JClass c
transType2 (a1 `FI.Fun` a2) = transType2 a1 `F.Fun` transType2 a2
transType2 (a1 `FI.And` a2) = F.Product [transType2 a1, transType2 a2]
transType2 (FI.Forall f)    = F.Forall (transType2 . f)

coerce :: Int -> FI.Type Int -> FI.Type Int -> Maybe (F.Expr Int Int)
coerce  i t1@(FI.JClass c1) (FI.JClass c2)
  | c1 == c2  = return (F.Lam (transType i t1) (\x -> F.Var "" x))
  | otherwise = Nothing
coerce  i t1@(FI.TVar a)    (FI.TVar b) | a == b = return (F.Lam (transType i t1) (\x -> F.Var "" x)) -- requires variable identity
coerce  i t1@(FI.Forall f)  (FI.Forall g)  =
  do c <- coerce (i + 1) (f i) (g i)
     return (F.Lam (transType i t1) (\x -> F.BLam (\a -> c `F.App` (F.Var "" x `F.TApp` F.TVar a))))
coerce  i f1@(FI.Fun t1 t2) (FI.Fun t3 t4) =
  do c1 <- coerce  i t3 t1
     c2 <- coerce  i t2 t4
     return (F.Lam (transType i f1) (\f -> F.Lam (transType i t3) (\x -> c2 `F.App` (F.Var "" f `F.App` (c1 `F.App` F.Var "" x)))))
coerce  i t1 (FI.And t2 t3) =
  do c1 <- coerce  i t1 t2
     c2 <- coerce  i t1 t3
     return (F.Lam (transType i t1) (\x -> F.Tuple [c1 `F.App` F.Var "" x, c2 `F.App` F.Var "" x]))
coerce  i t@(FI.And t1 t2) t3 =
  case coerce i t1 t3 of
    Nothing -> case coerce i t2 t3 of
                 Nothing -> Nothing
                 Just c -> return (F.Lam (transType i t1) (\x -> c `F.App` F.Proj 2 (F.Var "" x)))
    Just c -> return (F.Lam (transType i t1) (\x -> c `F.App` F.Proj 1 (F.Var "" x)))
coerce  i _ _ = Nothing

translate:: FI.Expr Int (Int, FI.Type Int) -> State Int (FI.Type Int, F.Expr Int Int)
translate (FI.Var (x, t)) = return (t, F.Var "" x)
translate (FI.Lit (E.Integer n)) = return (FI.JClass "java.lang.Integer",   F.Lit (E.Integer n))
translate (FI.Lit (E.String s))  = return (FI.JClass "java.lang.String",    F.Lit (E.String s))
translate (FI.Lit (E.Boolean b)) = return (FI.JClass "java.lang.Boolean",   F.Lit (E.Boolean b))
translate (FI.Lit (E.Char c))    = return (FI.JClass "java.lang.Character", F.Lit (E.Char c))
translate (FI.BLam f) =
  do i <- takeFreshIndex
     (t, m) <- translate (f i)
     return (FI.Forall (\a -> FI.fsubstTT (i, FI.TVar a) t), F.BLam (\a -> F.fsubstEE (i, F.Var "" a) m))
translate (FI.Lam t f) =
  do i <- takeFreshIndex
     (t1, m) <- translate (f (i, t))
     return (FI.Fun t t1, F.Lam (transType i t) (\x -> F.fsubstEE (i, F.Var "" x) m))
translate (FI.TApp e t) =
  do i <- takeFreshIndex
     (t1, m) <- translate e
     case t1 of
       FI.Forall f -> return (FI.fsubstTT (i, t) (f i), F.TApp m (transType i t))
       _        -> fail ""
translate (FI.App e1 e2)  =
  do (t1, m1) <- translate e1
     case t1 of
        FI.Fun t t'  ->
          do (t2, m2) <- translate e2
             i <- get
             case coerce i t2 t of
               Just c  -> return (t', m1 `F.App` (c `F.App` m2))
               Nothing -> fail ""
        _            -> fail ""
translate (FI.Merge e1 e2) =
  do (t1, m1) <- translate e1
     (t2, m2) <- translate e2
     return (FI.And t1 t2, F.Tuple [m1, m2])

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

genCoercion :: Int -> FI.Type Int -> FI.Type Int -> Maybe Coercion
genCoercion i (FI.TVar a) (FI.TVar b) | a == b = return []
genCoercion i t           (FI.And t1 t2) =
  case genCoercion i t t1 of
    Just c  -> return (TakeFst:c)
    Nothing -> case genCoercion i t t2 of
                 Just c  -> return (TakeSnd:c)
                 Nothing -> Nothing
genCoercion i (FI.And t1 t2) t =
  do c1 <- genCoercion i t1 t
     c2 <- genCoercion i t2 t
     return [Clone c1 c2]
genCoercion i (FI.Fun t1 t2) (FI.Fun t3 t4) =
  do c1 <- genCoercion i t3 t1
     c2 <- genCoercion i t2 t4
     return (if null c1 && null c2
                then []
                else [EtaExpand (transType i t3, c1) c2]) -- TODO
genCoercion i (FI.Forall f) (FI.Forall g)  =
  do c <- genCoercion (i + 1) (f i) (g i)
     return (if null c
                then []
                else BigEtaExpand:c)
genCoercion i _ _ = Nothing

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return
