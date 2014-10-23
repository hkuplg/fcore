{-# LANGUAGE TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Mixin where

import Prelude hiding (log)

class a <: b where
  up :: a -> b

instance (t1 <: t2) => (t -> t1) <: (t -> t2) where
  up f = up . f

instance (t1 <: t2) => (t2 -> t) <: (t1 -> t) where
  up f = f . up

type Class t   = t -> t
type Mixin s t = s -> t -> t

new :: Class a -> a
new f = let r = f r in r

with :: (t <: s) => Class s -> Mixin s t -> Class t
klass `with` mixin = \this -> mixin (klass (up this)) this

-- The below provides an example.

fib' :: Class (Int -> Int)
fib' _    1 = 1
fib' _    2 = 1
fib' this n = this (n-1) + this (n-2)

instance (Int, String) <: Int where
  up = fst

logging :: Mixin (Int -> Int) (Int -> (Int, String))
logging super _    1 = (super 1, "1")
logging super _    2 = (super 2, "2 1")
logging super this n = (super n, show n ++ " " ++ log1 ++ " " ++ log2)
  where
    (_, log1) = this (n-1)
    (_, log2) = this (n-2)

fibWithLogging :: Int -> (Int, String)
fibWithLogging = new (fib' `with` logging)
