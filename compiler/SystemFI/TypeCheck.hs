{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-unused-matches #-}
{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module SystemFI.TypeCheck where

import SystemFI.Syntax hiding (fsubstEE, fsubstTE)
import SystemF.Syntax  hiding (fsubstTT)

import Mixins
import PrettyUtils

import Unsafe.Coerce

import Text.PrettyPrint.Leijen

import Control.Monad.Identity

subtype :: Type Int -> Type Int -> Bool
subtype t1 t2 = runIdentity (new subtype' 0 t1 t2)

subtype':: Monad m => Open (Int -> Type Int -> Type Int -> m Bool)
subtype' this i Int         Int         = return True
subtype' this i Nat         Nat         = return True
subtype' this i Nat         Int         = return True
subtype' this i Int         Nat         = return False
subtype' this i (TVar a)    (TVar b)    = return (a == b)
subtype' this i (Forall f)  (Forall g)  = this (i + 1)(f i) (g i)
subtype' this i (Fun t1 t2) (Fun t3 t4) = liftM2 (&&) (this i t3 t1) (this i t2 t4)
subtype' this i t           (And t1 t2) = liftM2 (&&) (this i t  t1) (this i t  t2)
subtype' this i (And t1 t2) t           = liftM2 (||) (this i t1 t)  (this i t2 t)
subtype' this i _           _           = return False

data CoercionPrimitive
  = IntOfNat
  | TakeFst
  | TakeSnd

  -- f ↪ C2 (λx. f (C1 x))
  | EtaExpand Coercion -- C1
              Coercion -- C2

  | BigEtaExpand
  | Clone Coercion Coercion
  deriving (Show)

type Coercion = [CoercionPrimitive]

coerce :: Type Int -> Type Int -> Maybe Coercion
coerce = coerce' 0

coerce' :: Int -> Type Int -> Type Int -> Maybe Coercion
coerce' i Int         Int         = return []
coerce' i Nat         Nat         = return []
coerce' i Int         Nat         = return [IntOfNat]
coerce' i (TVar a)    (TVar b) | a == b = return []
coerce' i t           (And t1 t2) = case coerce' i t t1 of
                                     Just c  -> return (TakeFst:c)
                                     Nothing -> case coerce' i t t2 of
                                       Just c  -> return (TakeSnd:c)
                                       Nothing -> Nothing
coerce' i (Fun t1 t2) (Fun t3 t4) = do c1 <- coerce' i t3 t1
                                       c2 <- coerce' i t2 t4
                                       return (if null c1 && null c2
                                                  then []
                                                  else [EtaExpand c1 c2])
coerce' i (Forall f)  (Forall g)  = do c <- coerce' (i + 1) (f i) (g i)
                                       return (if null c
                                                  then []
                                                  else BigEtaExpand:c)
coerce' i (And t1 t2) t           = do c1 <- coerce' i t1 t
                                       c2 <- coerce' i t2 t
                                       return [Clone c1 c2]
coerce' i _           _           = Nothing

infer :: Expr t e -> Type Int
infer = runIdentity . new infer' 0 . unsafeCoerce

infer':: Monad m => Open (Int -> Expr Int (Type Int) -> m (Type Int))
infer' this i (Var x)       = return x
infer' this i (Lit n)       = return Int
infer' this i (BLam f)      = do t <- this (i + 1) (f i)
                                 return (Forall (\a -> fsubstTT (i, TVar a) t))
infer' this i (Lam t f)     = do t' <- this i (f t)
                                 return (Fun t t')
infer' this i (TApp e t)    = do t' <- this i e
                                 case t' of
                                   Forall f -> return (fsubstTT (i, t) (f i)) -- BUG occurs here.
                                   _        -> fail ""
infer' this i (App e1 e2)   = do t1 <- this i e1
                                 t2 <- this i e2
                                 case t1 of
                                   Fun t t'| t2 `subtype` t -> return t'
                                   _                        -> fail ""
infer' this i (Merge e1 e2) = do t1 <- this i e1
                                 t2 <- this i e2
                                 return (And t1 t2)

transType :: Int -> Type Int -> PFTyp Int
transType i (TVar a)      = FTVar a
transType i Int           = FInt
transType i (a1 `Fun` a2) = transType i a1 `FFun` transType i a2
transType i (a1 `And` a2) = FProduct [transType i a1, transType i a2]
transType i (Forall f)    = FForall (\a -> transType (i + 1) (f i))

-- Judgments

newtype Judgment = Judgment ( (ValueContext Int Int, Expr Int Int, Type Int)
                            , (Int, Int))

instance Show Judgment where
  show = show . pretty

instance Pretty Judgment where
  pretty (Judgment ((g, e, t), (i,j))) =
    text (show g) <+> text "⊢" <+>
    prettyExpr basePrecEnv (i,j) e <+> text "::" <+>
    prettyType basePrecEnv i t

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return