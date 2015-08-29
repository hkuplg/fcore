module TypeCheckSpec where

import Control.Monad (forM_)
import System.Directory
import System.FilePath
import Test.Tasty.Hspec

import Parser (reader, P(..))
import SpecHelper
import TypeCheck (typeCheck)


hasError :: Either a b -> Bool
hasError (Left _)  = True
hasError (Right _) = False


testCasesPath = "testsuite/tests/shouldntTypecheck"

tcSpec :: Spec
tcSpec = do
  failingCases <- runIO (discoverTestCases testCasesPath)

  curr <- runIO (getCurrentDirectory)
  runIO (setCurrentDirectory $ curr </> testCasesPath)

  describe "Should fail to typecheck" $
    forM_ failingCases
      (\(name, filePath) -> do
         do source <- runIO (readFile filePath)
            it ("should reject " ++ name) $
              let POk parsed = reader source
              in typeCheck parsed >>= ((`shouldSatisfy` hasError)))

  runIO (setCurrentDirectory curr)
