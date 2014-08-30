module Src.TypeCheckSpec where

import Test.Hspec
import SpecHelper

import Src.Syntax    hiding (Name)
import Src.Parser    (reader)
import Src.TypeCheck (infer)

import Control.Monad             (forM_)
import Control.Monad.Trans.Error (runErrorT)

main :: IO ()
main = hspec spec

hasError :: Either a b -> Bool
hasError (Left _)  = True
hasError (Right _) = False

spec :: Spec
spec = describe "*** Notice: Tests for `infer' is skipped" $ return ()

{-
spec :: Spec
spec =
  describe "infer" $ do
    failingCases <- runIO (discoverTestCases "testsuite/tests/esf/typecheck/should_fail")
    forM_ failingCases (\(name, source) ->
      it ("should reject " ++ name) $ do
        (runErrorT . infer . reader) source >>= (\either -> either `shouldSatisfy` hasError))
-}