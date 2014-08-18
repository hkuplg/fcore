module Mixins where


type Open a = a -> a

new :: Open a -> a
new f = let r = f r in r

top :: Open s
top this = this


type Mixin s = s -> s -> s

mixin :: Mixin s -> s
mixin f = let m = mixin f in f m m

zero :: Mixin s
zero super _this = super


extends :: Mixin s -> Open s -> Mixin s
f `extends` g = \_super this -> f (g this) this

mixes :: Mixin s -> Mixin s -> Mixin s
f `mixes` g = \super this -> f (g super this) this