{-# OPTIONS -XTypeOperators -XMultiParamTypeClasses #-}

module TestInh where

-- class (:<) r1 r2  where
--   to :: r1 -> r2
--   override :: r1 -> (r2 -> r2) -> r1 
   
newtype R1 = R1 {fun1 :: Int -> Int}

data R2 = R2 {toR1 :: R1, fun2 :: Int -> Int}

t1 this = R1 (\n -> if (n==0) then 1 else fun1 this (n-1))

t2 this = R2 (toR1 this) (\n -> if (n==1) then fun1 (toR1 this) n  else fun2 this (n-1))

new f = let r = f r in r

wrapT1 f this = R2 (f (toR1 this)) (fun2 this)

p = new (t2 . wrapT1 t1)