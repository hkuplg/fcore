{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module SystemF where

import Unsafe.Coerce (unsafeCoerce)

data Type t
  = TyVar t
  | Int
  | Fun (Type t) (Type t)
  | Forall (t -> Type t)

data Term t e
  = Var e
  | Lit Integer
  | Lam (Type t) (e -> Term t e)
  | App (Term t e) (Term t e)
  | BLam (t -> Term t e)
  | TApp (Term t e) (Type t)

konst :: Term t e
konst = BLam (\a -> BLam (\b -> Lam (TyVar a) (\x -> Lam (TyVar b) (\y -> Var x))))

infer :: Int -> Term Int (Type Int) -> Maybe (Type Int)
infer i (Var x)     = return x
infer i (Lit _)     = return Int
infer i (Lam t f)
  = do t2 <- infer i (f t)
       return (Fun t t2)
infer i (App e1 e2)
  = do t1 <- infer i e1
       t2 <- infer i e2
       case t1 of
         Fun t11 t12
           | t2 `alphaEqType` t11 -> return t12
         _ -> Nothing
infer i (BLam f)
  = do t <- infer (i + 1) (f i)
       return $ Forall (\a -> t)
infer i (TApp e arg)
  = do t <- infer i e
       case t of
         Forall f -> return $ joinType (f arg)
         _        -> Nothing

alphaEqType :: Type t -> Type t -> Bool
alphaEqType t1 t2 = alphaEqType' 0 (unsafeCoerce t1 :: Type Int) (unsafeCoerce t2 :: Type Int)

alphaEqType' :: (Eq t, Num t) => t -> Type t -> Type t -> Bool
alphaEqType' i (TyVar a)   (TyVar b)   = a == b
alphaEqType' i  Int         Int        = True
alphaEqType' i (Fun t1 t2) (Fun t3 t4) = alphaEqType' i t1 t3 && alphaEqType' i t2 t4
alphaEqType' i (Forall f)  (Forall g)  = alphaEqType' (i + 1) (f i) (g i)
alphaEqType' i _            _          = False

joinType :: Type (Type t) -> Type t
joinType (TyVar t)   = t
joinType  Int        = Int
joinType (Fun t1 t2) = Fun (joinType t1) (joinType t2)
joinType (Forall f)  = Forall (\a -> joinType $ f (TyVar a))

instance Show (Type Int) where
  show = pprType 0

pprType :: Int -> Type Int -> String
pprType i (TyVar t)   = "a" ++ show t
pprType i  Int        = "Int"
pprType i (Fun t1 t2) = "(" ++ pprType i t1 ++ " -> " ++ pprType i t2 ++ ")"
pprType i (Forall f)  = "(forall a" ++ show i ++ ". " ++ pprType (i + 1) (f i) ++ ")"