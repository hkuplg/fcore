module TransCFSpec where

import ESF.Parser       (reader)
import ESF.TypeCheck    (infer)
import ESF.Syntax
import Desugar          (desugarTcExpr)
import SystemF.Syntax
import ClosureF

import ApplyTransCFJava
import BaseTransCFJava

import Inheritance
import MonadLib
import Translations     (compileN, compileAO, compileS)

import Language.Java.Syntax as J
import Language.Java.Pretty
import System.Directory
import System.Process
import Text.PrettyPrint.Leijen
import Test.Hspec

import TestTerms

import Data.Map
import qualified Data.List as List      (isSuffixOf)
import qualified Data.Set  as Set

import Prelude hiding (const)

import Control.Monad.Trans.Error (runErrorT)

-- java compilation + run
compileAndRun name compileF exp =
   do let source = prettyPrint (fst $ (compileF name exp))
      let jname = name ++ ".java"
      writeFile jname source
      readProcess "javac" [jname] ""
      result <- readProcess "java" [name] ""
      -- readProcess "rm" [jname] ""
      x <- getDirectoryContents "."
      readProcess "rm" [y | y <- x, ".class" `List.isSuffixOf` y, y /= "TypeServer.class"] ""
      return result

test1 = \c -> it "Should compile factorial 10" (compileAndRun "FactApp" c factApp `shouldReturn` "3628800\n")

test2 = \c -> it "Should compile fibonacci 10" (compileAndRun "FiboApp" c fiboApp `shouldReturn` "55\n")

test3 = \c -> it "Should infer type of intapp" $ "(forall (_ : java.lang.Integer) . java.lang.Integer)" `shouldBe` ( let (cu, t) = (c "Main" intapp) in show t )

test4 = \c -> it "Should compile idF int 10" (compileAndRun "IdF" c idfNum `shouldReturn` "10\n")

test5 = \c -> it "Should compile const int 10 20" (compileAndRun "Const" c constNum `shouldReturn` "10\n")

test6 = \c -> it "Should compile program1 int 5" (compileAndRun "Program1" c program1Num `shouldReturn` "5\n")

test7 = \c -> it "Should compile program2" (compileAndRun "Program2" c program2 `shouldReturn` "5\n")

test8 = \c -> it "Should compile program4" (compileAndRun "Program4" c program4 `shouldReturn` "11\n")

suite = [test1, test2, test3, test4, test5, test6, test7, test8,
        ioTest "testsuite/tests/pipeline/lit_java.sf" "1\n",
        ioTest "testsuite/tests/pipeline/fact_big.sf" "87178291200\n",
        ioTest "testsuite/tests/pipeline/charat.sf" "c\n",
        ioTest "testsuite/tests/pipeline/substr.sf" "bc\n",
        ioTest "testsuite/tests/pipeline/always_true.sf" "true\n",
        ioTest "testsuite/tests/pipeline/always_false.sf" "false\n",
        ioTest "testsuite/tests/pipeline/hello_world.sf" "hello, world\n",
        ioTest "testsuite/tests/pipeline/hello_world2.sf" "HELLO, WORLD\n",
        ioTest "testsuite/tests/pipeline/min.sf" "4\n",
        ioTest "testsuite/tests/pipeline/max.sf" "7\n",
        ioTest "testsuite/tests/pipeline/comb.sf" "220\n",
        ioTest "testsuite/tests/pipeline/interfact.sf" "1\n",
        ioTest "testsuite/tests/pipeline/tailfact.sf" "3628800\n",
        ioTest "testsuite/tests/pipeline/fact.sf" "3628800\n",
        ioTest "testsuite/tests/pipeline/fibo.sf" "55\n",
        ioTest "testsuite/tests/pipeline/id.sf" "3\n",
      --ioTest "testsuite/tests/pipeline/mutual.sf" "1\n", --parse error
      --ioTest "testsuite/tests/pipeline/mutual2.sf" "1\n", --parse error
        ioTest "testsuite/tests/pipeline/mutualrec.sf" "1\n",
        ioTest "testsuite/tests/pipeline/Mutualrec2.sf" "0\n",
      --ioTest "testsuite/tests/pipeline/Mutualrec3.sf" "1\n", --patternmatch fail
        ioTest "testsuite/tests/pipeline/Products.sf" "1\n",
        ioTest "testsuite/tests/pipeline/Products4.sf" "3\n"]

testSf srcPath compileF = do
        src <- readFile srcPath
        let expr = ESF.Parser.reader src
        res <- runErrorT $ ESF.TypeCheck.infer expr
        case res of
          Left typeError -> error $ show (Text.PrettyPrint.Leijen.pretty typeError)
          Right (tcExpr, _t) ->
            let sf = desugarTcExpr tcExpr in
            compileAndRun "Main" compileF sf

naivesuite = describe "Naive compilation (BaseTransCF)" $ forM_ suite (\t -> t compileN)
aosuite = describe "ApplyOpt compilation (ApplyTransCF)" $ forM_ suite (\t -> t compileAO)
stacksuite = describe "Stack compilation (StackTransCF)" $ forM_ suite (\t -> t compileS)

ioTest = \src exp c  -> it ("Should compile " ++ src) $ ((testSf src c) `shouldReturn` exp)

evenOdd :: String
evenOdd = "let rec even = \\n -> n == 0 || odd (n-1) and odd = \\n -> if n == 0 then 0 else even (n-1) in odd"

spec = describe "Compile and run the result" $
        do naivesuite
           aosuite
           stacksuite

main = hspec spec
