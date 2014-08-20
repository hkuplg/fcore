{-# OPTIONS_GHC -fwarn-unused-imports #-}

module TransCFSpec where

import Test.Hspec
import SpecHelper
import TestTerms

import ESF.Parser       (reader)
import ESF.TypeCheck    (infer)
import Desugar          (desugarTcExpr)

import Translations     (compileN, compileAO, compileS)

import MonadLib

import Language.Java.Pretty
import Text.PrettyPrint.Leijen

import System.Directory
import System.Process

import Control.Monad.Trans.Error (runErrorT)

import qualified Data.List as List      (isSuffixOf)

testCasesPath = "testsuite/tests/pipeline"

-- java compilation + run
compileAndRun name compileF exp =
  do let source = prettyPrint (fst $ (compileF name exp))
     let jname = name ++ ".java"
     writeFile jname source
     readProcess "javac" ["-cp", "runtime/runtime.jar:.",jname] ""
     result <- readProcess "java" ["-cp", "runtime/runtime.jar:.",name] ""
     -- readProcess "rm" [jname] ""
     x <- getDirectoryContents "."
     readProcess "rm" [y | y <- x, ".class" `List.isSuffixOf` y, y /= "TypeServer.class"] ""
     return result

esf2sf expr =
  do res <- runErrorT (ESF.TypeCheck.infer expr)
     case res of
       Left typeError     -> error $ show (Text.PrettyPrint.Leijen.pretty typeError)
       Right (tcExpr, _t) -> return (desugarTcExpr tcExpr)

testAbstractSyn compilation (name, ast, expectedOutput) =
  it ("should compile and run " ++ name ++ " and get \"" ++ expectedOutput ++ "\"") $
     compileAndRun "Main" compilation ast `shouldReturn` (expectedOutput ++ "\n")

testConcreteSyn compilation (name, filePath) =
  do source <- runIO (readFile filePath)
     case parseExpectedOutput source of
       Nothing -> error (filePath ++ ": " ++
                         "The integration test file should start with '-->', \
                         \followed by the expected output")
       Just expectedOutput ->
         do ast <- runIO (esf2sf (ESF.Parser.reader source))
            testAbstractSyn compilation (name, ast, expectedOutput)

abstractCases =
  [("factorial 10", factApp, "3628800")
  ,("fibonacci 10", fiboApp, "55")
  ,("idF Int 10", idfNum, "10")
  ,("const Int 10 20", constNum, "10")
  ,("program1 Int 5", program1Num, "5")
  ,("program2", program2, "5")
  ,("program4", program4, "11")
  ]

-- intappCase = \c -> it "Should infer type of intapp" $ "(forall (_ : java.lang.Integer) . java.lang.Integer)" `shouldBe` ( let (cu, t) = (c "Main" intapp) in show t)

spec =
  do concreteCases <- runIO (discoverTestCases testCasesPath)
     forM_
       [("BaseTransCF" , compileN)
       ,("ApplyTransCF", compileAO)
       ,("StackTransCF", compileS)]
       (\(name, compilation) ->
         describe name $
           do forM_ abstractCases (testAbstractSyn compilation)
              forM_ concreteCases (testConcreteSyn compilation))

main :: IO ()
main = hspec spec
