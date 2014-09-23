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

