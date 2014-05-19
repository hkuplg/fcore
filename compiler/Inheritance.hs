{-# OPTIONS -XTypeOperators -XMultiParamTypeClasses -XRankNTypes -XExistentialQuantification -XFlexibleContexts -XFlexibleInstances #-}

module Inheritance where

import Debug.Trace

class a :< b where
   up :: a -> b

type Base t a     = t -> a
type Mixin t s a  = t -> s -> a

(<.>) :: Mixin self b c -> Mixin self a b -> Mixin self a c
(<.>) f g = \this super -> f this (g this super)

($>) :: Mixin self a b -> Base self a -> Base self b
($>) f g = \this -> f this (g this)
 
new :: Base a a -> a
new f = let r = f r in r

data R1 = R {f :: Int -> Int}

data R2 = R2 {superR1 :: R1, g :: Int -> Int}

instance R2 :< R1 where
   up = superR1

base :: (s :< R1) => Base s R1
base this = R {f = \n -> if (n == 0) then 0 else 1 + f (up this) (n-1)}

mix :: Mixin s R1 R2
mix this super = R2 (super {f = \n -> trace "Hello!" $ f super n})  (\n -> 1 + f super n)

test = new (mix $> base)

{-
instance a :< a where
   up = id
-}

{-
baseMix ::  (a :< R1) => a -> R1
baseMix this = R {f = \n -> if (n == 0) then 0 else 1 + f (up this) (n-1)}

u :: R1 -> R2 
u super = R2 (super {f = \n -> trace "Hello!" $ f super n})  (\n -> 1 + f super n) 

mix = M (\this super -> R2 (super {f = \n -> trace "Hello!" $ f super n})  (\n -> 1 + f super n {- + g (up this) (n-1) -})) 

o = u (baseMix o)

newtype Base self t = B {proceed :: (self :< t) => self -> t} -- have subtyping here?

newtype Mixin self super t = M {pr :: (self :< t, t :< super) => self -> super -> t} -- have subtyping here?

(<.>) :: (b :< a, c :< b, self :< b) => Mixin self b c -> Mixin self a b -> Mixin self a c
(<.>) (M f) (M g) = M (\this super -> f this (g this super))

($>) :: (b :< a, self :< a) => Mixin self a b -> Base self a -> Base self b
($>) (M f) (B g) = B (\this -> f this (g this))

new :: Base a a -> a
new (B f) = let r = f r in r

baseMix2 = B (\this -> R {f = \n -> if (n == 0) then 0 else 1 + f (up this) (n-1)})

test = new (mix $> baseMix2)

o2 = u (proceed baseMix2 o2)

data Open3 t = forall sub . O3 {proceed3 :: (sub :< t) => sub -> t}

baseMix3 = O3 (\this -> R {f = \n -> 1 + f (up this) (n-1)})
-}

{-
o3 = u (proceed3 baseMix2 o3)
-}