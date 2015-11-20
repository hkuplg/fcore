module TransCFSpec (transSpec, testConcreteSyn) where

import BackEnd
import FrontEnd (source2core)
import JavaUtils (getClassPath)
import MonadLib
import OptiUtils (Exp(Hide))
import PartialEvaluator (rewriteAndEval)
import SpecHelper
import StringPrefixes (namespace)
import StringUtils (capitalize)
import TestTerms

import Language.Java.Pretty (prettyPrint)
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Test.Tasty.Hspec


testCasesPath = "testsuite/tests/shouldRun"

fetchResult :: Handle -> IO String
fetchResult outP = do
  msg <- hGetLine outP
  if msg == "exit" then fetchResult outP else return msg

-- java compilation + run
compileAndRun inP outP name compileF exp =
  do let source = prettyPrint (fst (compileF name (rewriteAndEval exp)))
     let jname = name ++ ".java"
     hPutStrLn inP jname
     hPutStrLn inP (source ++ "\n" ++ "//end of file")
     fetchResult outP

testAbstractSyn inP outP compilation (name, filePath, ast, expectedOutput) = do
  let className = capitalize $ dropExtension (takeFileName filePath)
  output <- runIO (compileAndRun inP outP className compilation ast)
  it ("should compile and run " ++ name ++ " and get \"" ++ expectedOutput ++ "\"") $
     return output `shouldReturn` expectedOutput

testConcreteSyn inP outP compilation (name, filePath) =
  do source <- runIO (readFile filePath)
     case parseExpectedOutput source of
       Nothing -> error (filePath ++ ": " ++
                         "The integration test file should start with '-->', \
                         \followed by the expected output")
       Just expectedOutput ->
         do ast <- runIO (source2core NoDump "" (filePath, source))
            testAbstractSyn inP outP compilation (name, filePath, ast, expectedOutput)

abstractCases =
  [ ("factorial 10", "main_1", Hide factApp, "3628800")
  , ("fibonacci 10", "main_2", Hide fiboApp, "55")
  , ("idF Int 10", "main_3", Hide idfNum, "10")
  , ("const Int 10 20", "main_4", Hide constNum, "10")
  , ("program1 Int 5", "main_5", Hide program1Num, "5")
  , ("program2", "main_6", Hide program2, "5")
  , ("program4", "main_7", Hide program4, "11")
  ]

transSpec =
  do concreteCases <- runIO (discoverTestCases testCasesPath)

     -- change to testing directory for module testing
     curr <- runIO (getCurrentDirectory)
     runIO (setCurrentDirectory $ curr </> testCasesPath)

     forM_
       [("BaseTransCF" , compileN)
       ,("ApplyTransCF", compileAO)
       ,("StackTransCF", compileS)]
       (\(name, compilation) ->
         describe name $
           do cp <- runIO getClassPath
              let p = (proc "java" ["-cp", cp, namespace ++ "FileServer", cp])
                          {std_in = CreatePipe, std_out = CreatePipe}
              (Just inP, Just outP, _, proch) <- runIO $ createProcess p
              runIO $ hSetBuffering inP NoBuffering
              runIO $ hSetBuffering outP NoBuffering
              forM_ abstractCases (testAbstractSyn inP outP compilation)
              forM_ concreteCases (testConcreteSyn inP outP compilation))

     runIO (setCurrentDirectory curr)

