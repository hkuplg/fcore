
import Test.Tasty
import Test.Tasty.Hspec

import ModuleSpec
import TransCFSpec (transSpec)
import TypeCheckSpec (tcSpec)

main :: IO ()
main = do
  tcTests <- testSpec "Typecheck" tcSpec
  transTests <- testSpec "Translations" transSpec
  let tests = testGroup "fcore tests" [tcTests, transTests]
  defaultMain tests
