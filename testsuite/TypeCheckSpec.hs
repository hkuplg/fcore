module TypeCheckSpec where

import Test.Hspec
import SpecHelper

import Src
import Parser    (reader)
import TypeCheck (typeCheck)

import Control.Monad             (forM_)
import Control.Monad.Trans.Error (runErrorT)

main :: IO ()
main = hspec spec

hasError :: Either a b -> Bool
hasError (Left _)  = True
hasError (Right _) = False

spec :: Spec

spec =
  describe "typeCheck" $ do
    failingCases <- runIO (discoverTestCases "testsuite/tests/typecheck/should_fail")
    forM_ failingCases (\(name, source) ->
      it ("should reject " ++ name) $ do
        (typeCheck . reader) source >>= (\either -> either `shouldSatisfy` hasError))