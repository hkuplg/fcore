
import Test.Tasty
import Test.Tasty.Hspec

import ModuleSpec
import TransCFSpec (transSpec)
import TypeCheckSpec (tcSpec)

main :: IO ()
main = do
  tcTests <- testSpec "Typecheck" tcSpec
  moduleTests <- testSpec "Module" moduleSpec
  transTests <- testSpec "Translations" transSpec
  let tests = testGroup "fcore tests" [tcTests, moduleTests, transTests]
  defaultMain tests
