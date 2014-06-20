module SystemF.Spec where

import Test.Hspec
import Language.Java.Syntax as J

import SystemF.Syntax
import SystemF.Parser
import SystemF.Pretty

import qualified TestSuite

parseAndPrettyPrint :: String -> String
parseAndPrettyPrint = prettyPrintPFExp . reader

e1 = "(10 - 1) * (10 + 1) - (10 * 10 - 1 * 1)"

main :: IO ()
main = hspec $ do
    describe "reader" $ do
        it e1 $ parseAndPrettyPrint e1 `shouldBe` e1
