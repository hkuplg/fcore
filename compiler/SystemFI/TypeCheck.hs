{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-unused-matches #-}
{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module SystemFI.TypeCheck where

import SystemF.Syntax  as F
import SystemFI.Syntax as I
import ESF.Syntax (Lit(..))

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
subtypeM this i Int         Int         = return True
subtypeM this i Nat         Nat         = return True
subtypeM this i Nat         Int         = return True
subtypeM this i Int         Nat         = return False
subtypeM this i (TVar a)    (TVar b)    = return (a == b)
subtypeM this i (Forall f)  (Forall g)  = this (i + 1)(f i) (g i)
subtypeM this i (Fun t1 t2) (Fun t3 t4) = liftM2 (&&) (this i t3 t1) (this i t2 t4)
subtypeM this i t           (And t1 t2) = liftM2 (&&) (this i t  t1) (this i t  t2)
subtypeM this i (And t1 t2) t           = liftM2 (||) (this i t1 t)  (this i t2 t)
subtypeM this i _           _           = return False

infer :: Expr t e -> PrettyPHOAS (Type Int)
infer = PrettyPHOAS . runIdentity . new inferM 0 . unsafeCoerce

inferM:: Monad m => Open (Int -> Expr Int (Type Int) -> m (Type Int, Int))
inferM this i (Var x)       = return (x, i)
inferM this i (Lit n)       = return (Int, i)
inferM this i (BLam f)      = do (t, _) <- this (i + 1) (f i)
                                 return (Forall (\a -> I.fsubstTT (i, TVar a) t), i)
inferM this i (Lam t f)     = do (t', _) <- this i (f t)
                                 return (Fun t t', i)
inferM this i (TApp e t)    = do (t', i') <- this i e
                                 case t' of
                                   Forall f -> return (I.fsubstTT (i', t) (f i'), i' + 1)
                                   _        -> fail ""
inferM this i (App e1 e2)   = do (t1, _) <- this i e1
                                 (t2, _) <- this i e2
                                 case t1 of
                                   Fun t t'| t2 `subtype` t -> return (t', i)
                                   _                        -> fail ""
inferM this i (Merge e1 e2) = do (t1, _) <- this i e1
                                 (t2, _) <- this i e2
                                 return (And t1 t2, i)

transType :: Int -> Type Int -> PFTyp Int
transType i (TVar a)      = FTVar a
transType i Int           = FJClass "java.lang.Integer"
transType i (a1 `Fun` a2) = transType i a1 `FFun` transType i a2
transType i (a1 `And` a2) = FProduct [transType i a1, transType i a2]
transType i (Forall f)    = FForall (\a -> transType (i + 1) (f i))

coerce :: Int -> Type Int -> Type Int -> Maybe (PFExp Int Int)
coerce  i t1@Int         Int               = return (FLam (transType i t1) (\x -> FVar "" x))
coerce  i t1@(TVar a)    (TVar b) | a == b = return (FLam (transType i t1) (\x -> FVar "" x))
coerce  i t1@(Forall f)  (Forall g)  =
  do c <- coerce (i + 1) (f i) (g i)
     return (FLam (transType i t1) (\x -> FBLam (\a -> c `FApp` (FVar "" x `FTApp` FTVar a))))
coerce  i f1@(Fun t1 t2) (Fun t3 t4) =
  do c1 <- coerce  i t3 t1
     c2 <- coerce  i t2 t4
     return (FLam (transType i f1) (\f -> FLam (transType i t3) (\x -> c2 `FApp` (FVar "" f `FApp` (c1 `FApp` FVar "" x)))))
coerce  i t1 (And t2 t3) =
  do c1 <- coerce  i t1 t2
     c2 <- coerce  i t1 t3
     return (FLam (transType i t1) (\x -> FTuple [c1 `FApp` FVar "" x, c2 `FApp` FVar "" x]))
coerce  i t@(And t1 t2) t3 =
  case coerce i t1 t3 of
    Nothing -> case coerce i t2 t3 of
                 Nothing -> Nothing
                 Just c -> return (FLam (transType i t1) (\x -> c `FApp` FProj 2 (FVar "" x)))
    Just c -> return (FLam (transType i t1) (\x -> c `FApp` FProj 1 (FVar "" x)))
coerce  i _ _ = Nothing

translate:: Expr Int (Int, Type Int) -> State Int (Type Int, PFExp Int Int)
translate (Var (x, t)) = return (t, FVar "" x)
translate (Lit n)      = return (Int, FLit (Integer n))
translate (BLam f) =
  do i <- takeFreshIndex
     (t, m) <- translate (f i)
     return (Forall (\a -> I.fsubstTT (i, TVar a) t), FBLam (\a -> F.fsubstEE (i, FVar "" a) m))
translate (Lam t f) =
  do i <- takeFreshIndex
     (t1, m) <- translate (f (i, t))
     return (Fun t t1, FLam (transType i t) (\x -> F.fsubstEE (i, FVar "" x) m))
translate (TApp e t) =
  do i <- takeFreshIndex
     (t1, m) <- translate e
     case t1 of
       Forall f -> return (I.fsubstTT (i, t) (f i), FTApp m (transType i t))
       _        -> fail ""
translate (App e1 e2)  =
  do (t1, m1) <- translate e1
     case t1 of
        Fun t t'  ->
          do (t2, m2) <- translate e2
             i <- get
             case coerce i t2 t of
               Just c  -> return (t', m1 `FApp` (c `FApp` m2))
               Nothing -> fail ""
        _            -> fail ""
translate (Merge e1 e2) =
  do (t1, m1) <- translate e1
     (t2, m2) <- translate e2
     return (And t1 t2, FTuple [m1, m2])

data CoercionPrimitive
  = IntOfNat
  | TakeFst
  | TakeSnd
  -- f ↪ C2 (λ(x : t). f (C1 x))
  | EtaExpand (PFTyp Int, Coercion) -- C1
              Coercion -- C2
  | BigEtaExpand
  | Clone Coercion Coercion
  deriving (Show)

type Coercion = [CoercionPrimitive]

applyCoercion :: Coercion -> PFExp Int Int -> PFExp Int Int
applyCoercion [] e                        = e
applyCoercion (TakeFst:cs) e              = FProj 1 (applyCoercion cs e)
applyCoercion (TakeSnd:cs) e              = FProj 2 (applyCoercion cs e)
applyCoercion (EtaExpand (t, c1) c2:cs) e = applyCoercion c2 (FLam t (\x -> e `FApp` applyCoercion c1 (FVar "" x)))
applyCoercion (BigEtaExpand:cs) e         = FBLam (\a -> e `FTApp` FTVar a)
applyCoercion (Clone c1 c2:cs) e          = FTuple [applyCoercion c1 e, applyCoercion c2 e]

genCoercion :: Int -> Type Int -> Type Int -> Maybe Coercion
genCoercion i Int         Int         = return []
genCoercion i Nat         Nat         = return []
genCoercion i Int         Nat         = return [IntOfNat]
genCoercion i (TVar a)    (TVar b) | a == b = return []
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
genCoercion i (Forall f)  (Forall g)  =
  do c <- genCoercion (i + 1) (f i) (g i)
     return (if null c
                then []
                else BigEtaExpand:c)
genCoercion i _ _ = Nothing

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return