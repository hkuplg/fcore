{-# LANGUAGE TemplateHaskell #-}

module SearchLinkedList where

-- 1 try "runTests" to test all the properties
-- 2 try "mulitTest" to test "prop_pseudo_goal" 10 times with time showed below

import Test.QuickCheck.All
import Test.QuickCheck
import Data.List
import Text.Printf
import System.CPUTime
import System.Timeout


--function "contains"
contains :: [Int] -> Int -> Bool
contains [] e = False
contains (x:xs) e = x == e || contains xs e

--function "firstZero"
firstZero :: [Int] -> Int
firstZero [] = 0
firstZero (x:xs) = if x == 0 then 0
                             else (firstZero xs) + 1


--function "firstZeroAtPos"
firstZeroAtPos :: [Int] -> Int -> Bool
firstZeroAtPos [] e = False
firstZeroAtPos (x:xs) e = if e ==0 then x == 0
                                   else x /= 0 &&
                                        firstZeroAtPos xs (e-1)

----Property "goal"
prop_goal :: [Int] -> Int -> Bool
prop_goal l i = if (firstZero l) == i
                then if contains l 0
                     then firstZeroAtPos l i
                     else i == length l
                else True

---prop pseudo goal
prop_pseudo_goal :: [Int] -> Int -> Bool
prop_pseudo_goal l i = if (firstZero l) == i
                     then if contains l 10
                     then firstZeroAtPos l i
                     else i == length l
                    else True



----timer
timeIt :: (Fractional c) => (a -> IO b) -> a -> IO c
timeIt action arg = do
       startTime <- getCPUTime
       action arg
       finishTime <- getCPUTime
       return $ fromIntegral (finishTime - startTime) / 1000000000

--timeIt' :: (Fractional c) => (a -> b) -> a -> IO c
--timeIt' f = timeIt (\x -> f x `seq` return())

oneTest = timeout 1000000 $ timeIt quickCheck prop_pseudo_goal >>= print

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





------for-quickCheck-test------
return []                    --
runTests = $quickCheckAll    --
-------------------------------





