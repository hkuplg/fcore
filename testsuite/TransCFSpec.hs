{-# OPTIONS_GHC -fwarn-unused-imports #-}
{-# LANGUAGE TemplateHaskell #-}

module TransCFSpec where

import Test.Tasty.Hspec
import SpecHelper
import TestTerms

import Parser    (reader, P(..))
import TypeCheck    (typeCheck)
import Desugar      (desugar)
import Simplify     (simplify)
import qualified OptiUtils (Exp(Hide))
import PartialEvaluator
import BackEnd (compileN, compileAO, compileS)

import qualified SystemFI as FI

import MonadLib

import Language.Java.Pretty

import System.Directory
import System.Process
import System.FilePath  ((</>), dropExtension, takeFileName)
import System.IO

import Data.FileEmbed   (embedFile)
import qualified Data.ByteString as B
import JavaUtils
import FileIO
import StringPrefixes   (namespace)

runtimeBytes :: B.ByteString
runtimeBytes = $(embedFile "runtime/runtime.jar")

writeRuntimeToTemp :: IO ()
writeRuntimeToTemp =
  do tempdir <- getTemporaryDirectory
     let tempFile = tempdir </> "runtime.jar"
     B.writeFile tempFile runtimeBytes

testCasesPath = "testsuite/tests/shouldRun"

-- java compilation + run
compileAndRun inP outP name compileF exp =
  do let source = prettyPrint (fst $ (compileF name exp))
     let jname = name ++ ".java"
     sendMsg inP jname
     sendFile inP (source ++ "\n" ++ "//end of file")
     result <- receiveMsg3 outP
     return result

esf2sf source =
  do case source of
      PError msg -> error $ show msg
      POk parsed -> do
        result <- typeCheck parsed
        case result of
          Left typeError -> error $ show typeError
          Right (_, checked) ->
            let fiExpr = desugar checked
            in return (rewriteAndEval (OptiUtils.Hide (simplify (FI.HideF fiExpr))))

testAbstractSyn inP outP compilation (name, filePath, ast, expectedOutput) = do
  let className = getClassName $ dropExtension (takeFileName filePath)
  output <- runIO (compileAndRun inP outP className compilation ast)
  it ("should compile and run " ++ name ++ " and get \"" ++ expectedOutput ++ "\"") $
     (return output) `shouldReturn` expectedOutput

testConcreteSyn inP outP compilation (name, filePath) =
  do source <- runIO (readFile filePath)
     case parseExpectedOutput source of
       Nothing -> error (filePath ++ ": " ++
                         "The integration test file should start with '-->', \
                         \followed by the expected output")
       Just expectedOutput ->
         do ast <- runIO (esf2sf (Parser.reader source))
            testAbstractSyn inP outP compilation (name, filePath, ast, expectedOutput)

abstractCases =
  [("factorial 10", "main_1", factApp, "3628800")
  ,("fibonacci 10", "main_2", fiboApp, "55")
  ,("idF Int 10", "main_3", idfNum, "10")
  ,("const Int 10 20", "main_4", constNum, "10")
  ,("program1 Int 5", "main_5", program1Num, "5")
  ,("program2", "main_6", program2, "5")
  ,("program4", "main_7", program4, "11")
  ]

-- intappCase = \c -> it "Should infer type of intapp" $ "(forall (_ : java.lang.Integer) . java.lang.Integer)" `shouldBe` ( let (cu, t) = (c "Main" intapp) in show t)

transSpec =
  do concreteCases <- runIO (discoverTestCases testCasesPath)
     forM_
       [("BaseTransCF" , compileN)
       ,("ApplyTransCF", compileAO)
       ,("StackTransCF", compileS)]
       (\(name, compilation) ->
         describe name $
           do cp <- runIO getClassPath
              let p = (proc "java" ["-cp", cp, (namespace ++ "FileServer"), cp])
                          {std_in = CreatePipe, std_out = CreatePipe}
              (Just inP, Just outP, _, proch) <- runIO $ createProcess p
              runIO $ hSetBuffering inP NoBuffering
              runIO $ hSetBuffering outP NoBuffering
              forM_ abstractCases (testAbstractSyn inP outP compilation)
              forM_ concreteCases (testConcreteSyn inP outP compilation))
              --terminateProcess proch
