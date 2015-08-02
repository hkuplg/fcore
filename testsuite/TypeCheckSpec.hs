module TypeCheckSpec where

import Test.Tasty.Hspec
import Control.Monad (forM_)

import SpecHelper
import Parser    (reader, P(..))
import TypeCheck (typeCheck)


hasError :: Either a b -> Bool
hasError (Left _)  = True
hasError (Right _) = False

tcSpec :: Spec

tcSpec =
  describe "Should fail to typecheck" $ do
    failingCases <- runIO (discoverTestCases "testsuite/tests/shouldntTypecheck")
    forM_ failingCases
      (\(name, source) -> it ("should reject " ++ name) $
         let POk parsed = reader source
         in typeCheck parsed >>= (\checked -> checked `shouldSatisfy` hasError))
