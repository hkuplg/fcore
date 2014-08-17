module Mixins where

type Open a = a -> a

new :: Open a -> a
new f = let r = f r in r

-- Basic mixin combinators

type Mixin s = s -> s -> s

extends :: Mixin s -> Open s -> Mixin s
f `extends` g = \super this -> f (g this) this

mixes :: Mixin s -> Mixin s -> Mixin s
f `mixes` g = \super this -> f (g super this) this

top :: Open s
top this = this

zero :: Mixin s
zero super this = super

mixin :: Mixin s -> s
mixin f = let m = mixin f in f m m

-- Examples

data G = G { value :: Int, square :: Int } deriving (Show)

g1, g2 :: Mixin G
g1 super this = G { value = 7, square = value this * value this }
g2 super this = super { value = 2 }

m1, m2 :: G
m1 = mixin g1
m2 = mixin (g2 `mixes` g1)