module Mixins where

type Open a = a -> a

new :: Open a -> a
new f = let r = f r in r

