{-# LANGUAGE TemplateHaskell #-}

module ParBalance where

--1 try "runTest" in the terminal to test all the properites, just a reminder, it won't stop.
--2 try "multiTest" to test "prop_reverse_reverse_equivalence" 10 times with time showed below

import Test.QuickCheck.All
import Test.QuickCheck
import Data.List
import qualified Data.Set as Set
import MonadLib
import System.CPUTime
import System.Timeout


openPar :: Int
openPar = 1
closePar :: Int
closePar = 2

--function "balanced"
balanced :: [Int] -> Int -> Bool
balanced l co = if co < 0
                then False
                else case l of [] -> co == 0
                               (h:t) -> balanced t c
                                  where c = if h == openPar
                                            then (co + 1)
                                            else if h == closePar
                                                 then (co - 1)
                                                 else co

--function "balanced_nonEarly"
balanced_nonEarly :: [Int] -> Int -> Bool
balanced_nonEarly l co =
      case l of [] -> co == 0
                (h:t) -> if co < 0
                         then balanced_nonEarly t co
                         else balanced_nonEarly t c
                               where c = if h == openPar then (co + 1)
                                         else if h == closePar
                                              then (co - 1)
                                              else co

----test for balanced_nonEarly
prop_balanced_nonEarly :: [Int] -> Int -> Bool
prop_balanced_nonEarly l c = balanced_nonEarly l c == balanced l c


--function "balanced_withFailure"
balanced_withFailure :: [Int] -> Int -> Bool -> Bool
balanced_withFailure l co fa
   | co >= 0 || fa =
       case l of [] -> not fa && co == 0
                 (h:t) -> balanced_withFailure t c (fa || c < 0)
                   where c = if h == openPar then (co + 1)
                                             else if h == closePar
                                                  then (co - 1)
                                                  else co

----test for balance_withFailure
prop_balance_withFailure :: [Int] -> Int -> Bool -> Property
prop_balance_withFailure l co fa = co >= 0 || fa ==>
     if fa then res == balanced_nonEarly l (-1)
           else res == balanced_nonEarly l co
             where res = balanced_withFailure l co fa



--function "balanced_withReduce"
balanced_withReduce :: [Int] -> (Int,Int) -> Bool
balanced_withReduce l p@(p1,p2)
 | p1 >= 0 && p2 >= 0 =
    case l of [] -> p1 == 0 && p2 == 0
              (h:t) -> balanced_withReduce t (reduce p (parPair h))
 | otherwise = error "pair is less then 0"

----test for balanced_withReduce
prop_balanced_withReduce :: [Int] -> (Int,Int) -> Property
prop_balanced_withReduce l (p1,p2) = p1 >= 0 && p2 >= 0 ==>
     balanced_withReduce l (p1,p2) ==
     balanced_withFailure l (p1 - p2) (p2 > 0)


--function "balanced_foldLeft_equivalence"
balanced_foldLeft_equivalence :: [Int] -> (Int,Int) -> Bool
balanced_foldLeft_equivalence l (p1,p2)
 | p1 >= 0 && p2 >= 0 =
   (foldl f (p1,p2) l == (0,0)) == balanced_withReduce l (p1,p2)
   && case l of [] -> True
                (h:t) -> balanced_foldLeft_equivalence t pp
                  where pp = f (p1,p2) h
 | otherwise = error " "
 where f = (\s-> \x -> reduce s (parPair x))

----test for balanced_foldLeft_equivalence
prop_balanced_foldLeft_equivalence :: [Int] -> (Int,Int) -> Property
prop_balanced_foldLeft_equivalence l (p1,p2) = p1 >= 0 && p2 >= 0 ==>
    balanced_foldLeft_equivalence l (p1,p2)



--function "reduce"
reduce :: (Int,Int) -> (Int,Int) -> (Int,Int)
reduce p1@(p11,p12) p2@(p21,p22) =
       if p11 >= p22 then ((p11 - p22 + p21), p12)
                     else (p21, (p22 - p11 + p12))


--property "reduce_associative"
prop_reduce_associative :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
prop_reduce_associative p1 p2 p3 = reduce p1 (reduce p2 p3) ==
                                   reduce (reduce p1 p2) p3


--function "swap"
swap :: (Int, Int) -> (Int, Int)
swap (p1,p2) = (p2,p1)


--property "reduce_inverse"
prop_reduce_inverse :: (Int,Int) -> (Int,Int) -> Bool
prop_reduce_inverse p1 p2 = reduce p1 p2 ==
                            swap (reduce (swap p2) (swap p1))


--property "reduce_associative_inverse"
prop_reduce_associative_inverse :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
prop_reduce_associative_inverse p1 p2 p3 =
         reduce p1 (reduce p2 p3) ==
         swap (reduce (reduce (swap p3) (swap p2)) (swap p1))


--property "reduce_associative_inverse2"
prop_reduce_associative_inverse2 :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
prop_reduce_associative_inverse2 p1 p2 p3 =
         reduce (reduce p1 p2) p3 ==
         swap (reduce (swap p3) (reduce (swap p2) (swap p1)))


--function "parPair"
parPair :: Int -> (Int,Int)
parPair x = if x == openPar then (1,0)
                            else if x == closePar then (0,1)
                                                  else (0,0)


--function "headOption"
headOption :: [Int] -> Maybe Int
headOption (h:t) = Just h
headOption [] = Nothing


--function "lastOption"
lastOption :: [Int] -> Maybe Int
lastOption [h] = Just h
lastOption (h:t) = lastOption t
lastOption [] = Nothing


--function "addLast"
addLast :: [Int] -> Int -> [Int]
addLast (h:t) x = (h:(addLast t x))
addLast [] x = [x]

--function "reversel" denoting reverese function using addLast
reversel :: [Int] -> [Int]
reversel (h:t) = addLast (reversel t) h
reversel [] = []

----test for reversel
prop_reversel :: [Int] -> Bool
prop_reversel l = lastOption res == headOption l
               && lastOption l == headOption res
                   where res = reversel l


----property "reverse_tail_equivalence"
prop_reverse_tail_equivalence :: [Int] -> Bool
prop_reverse_tail_equivalence [] = True
prop_reverse_tail_equivalence l =
     reversel (tail l) == init (reversel l)


----property "reverse_init_equivalence"
prop_reverse_init_equivalence :: [Int] -> Bool
prop_reverse_init_equivalence [] = True
prop_reverse_init_equivalence l= reversel (init l) == tail (reversel l)
                     && case l of [] -> True
                                  (h:t) -> prop_reverse_init_equivalence t


----property "reverse_equality_equivalence"
prop_reverse_equality_equivalence :: [Int] -> [Int] -> Bool
prop_reverse_equality_equivalence l1 l2 =
    (l1 == l2) == (reversel l1 == reversel l2) &&
    case l1 of (h1:t1) -> case l2 of (h2:t2) -> prop_reverse_equality_equivalence t1 t2
                                     l2 -> True
               l1 -> True


{- this property is commented becasue it can't stop, actually the below property has the same
   problem

prop_reverses :: [Int] -> Bool
prop_reverses l = reverse (reverse l) == l
    && case (l, (reverse l)) of ((h1:t1),(h2:t2)) -> prop_reverses t1 && prop_reverses t2
                                (l, l2) -> True
-}

----property "reverse_reverse_equivalence"
reverse_reverse_equivalence :: [Int] -> Bool
reverse_reverse_equivalence l = reversel (reversel l) == l
      && case (l, (reversel l)) of ((h1:t1),(h2:t2)) -> reverse_reverse_equivalence t1 && reverse_reverse_equivalence t2
                                   (l, l2) -> True



prop_reverse_reverse_equivalence :: Property
prop_reverse_reverse_equivalence =
  forAll shortList $ \l -> reverse_reverse_equivalence l

----short list data generator
shortList :: Gen [Int]
shortList = (take 15) `fmap` arbitrary





------------------------------timer------------------------------------
timeIt :: (Fractional c) => (a -> IO b) -> a -> IO c
timeIt action arg = do
       startTime <- getCPUTime
       action arg
       finishTime <- getCPUTime
       return $ fromIntegral (finishTime - startTime) / 1000000000

--timeIt' :: (Fractional c) => (a -> b) -> a -> IO c
--timeIt' f = timeIt (\x -> f x `seq` return())

oneTest = timeout 5000000 $ timeIt quickCheck prop_reverse_reverse_equivalence >>= print

multiTest = do
            startTime <- getCPUTime
            oneTest
            oneTest
            oneTest
            oneTest
            oneTest
            oneTest
            oneTest
            oneTest
            oneTest
            oneTest
            finishTime <- getCPUTime
            return $ fromIntegral (finishTime - startTime) / 1000000000

----------------------------------------------------------------------




------for-quickCheck-test------
return []                    --
runTests = $quickCheckAll    --
-------------------------------










