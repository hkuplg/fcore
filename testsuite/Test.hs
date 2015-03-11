{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Test.Tasty
import Test.Tasty.HUnit

import SrcTests

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [dummyTests, srcTests]

dummyTests = testGroup "Dummy tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT
  ]
