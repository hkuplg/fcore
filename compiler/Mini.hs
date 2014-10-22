{-# LANGUAGE RankNTypes #-}

module Mini where

data Type t = Int | TVar t | Fun (Type t) (Type t) | Forall (t -> Type t)

newtype CType = CType { unCType :: forall t. Type t }

konstTy = Forall (\a -> Forall (\b -> Fun (TVar a) (Fun (TVar b) (TVar a))))

t1 = let Forall f = konstTy in Forall (\a -> subst' a Int (f a))

t2 = let Forall f = konstTy in join (f Int)

dedeBruijnType :: Int -> Int -> [t] -> Type Int -> Type t
dedeBruijnType s _ as (TVar i)    = TVar (reverse as !! (i-s))
dedeBruijnType s _ _  Int         = Int
dedeBruijnType s i as (Fun t1 t2) = Fun (dedeBruijnType s i as t1) (dedeBruijnType s i as t2)
dedeBruijnType s i as (Forall f)  = Forall (\a -> dedeBruijnType s (i+1) (a:as) (f i))

subst' :: Int -> Type Int -> Type Int -> Type Int
subst' x r Int         = Int
subst' x r (TVar a)
  | a == x            = r
  | otherwise         = TVar a
subst' x r (Fun t1 t2) = Fun (subst' x r t1) (subst' x r t2)
subst' x r (Forall f)  = Forall (\a -> subst' x r (f a))

-- subst :: Type Int -> Type Int -> Type t
-- subst t1 t2 = dedeBruijnType subst' 0 t1 t2

pretty :: Int -> Type Int -> String
pretty _ Int         = "Int"
pretty _ (TVar a)    = "a" ++ show a
pretty i (Fun t1 t2) = "(" ++ pretty i t1 ++ " -> " ++ pretty i t2 ++ ")"
pretty i (Forall f)  = "forall a" ++ show i ++ ". " ++ pretty (i+1) (f i)

join :: Type (Type t) -> Type t
join Int         = Int
join (TVar a)    = a
join (Fun t1 t2) = Fun (join t1) (join t2)
join (Forall f)  = Forall (\a -> join (f (TVar a)))
