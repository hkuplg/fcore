
import Test.Tasty
import Test.Tasty.Hspec

import TypeCheckSpec (tcSpec)
import TransCFSpec (transSpec)

main :: IO ()
main = do
  tcTests <- testSpec "Typecheck" tcSpec
  transTests <- testSpec "Translations" transSpec
  tests <- return $ testGroup "fcore tests" [tcTests, transTests]
  defaultMain tests
