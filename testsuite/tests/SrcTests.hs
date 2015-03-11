{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module SrcTests where

import Test.Tasty
import Test.Tasty.HUnit

import Src

import qualified Data.Map as Map

srcTests = testGroup "Src" [compatibleTests, groupForallTests]

compatibleTests = testGroup "compatible" 
  [ testCase "A = A" $ 
      compatible (Map.fromList [("A", (Star, TerminalType))]) a a @?= True
  ]

groupForallTests = testGroup "groupForall" 
  [ testCase "forall A. forall B. A -> B" $
      groupForall (forallAB (a `Fun` b)) @?= (["A", "B"], a `Fun` b)

  , testCase "forall A. forall B. A -> (forall A. A)" $
      groupForall (forallAB (a `Fun` forallA a)) @?= (["A", "B"], a `Fun` forallA a)
  ] 

forallA  = Forall "A"
forallAB = Forall "A" . Forall "B" 
a = TVar "A"
b = TVar "B"
