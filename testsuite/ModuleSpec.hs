module ModuleSpec where

import BackEnd
import JavaUtils (getClassPath)
import SpecHelper
import StringPrefixes (namespace)
import TransCFSpec

import Control.Monad (forM_)
import System.Directory
import System.FilePath
import System.IO
import System.Process
import Test.Tasty.Hspec


testCasesPath = "testsuite/tests/should_run/module"

moduleSpec :: Spec
moduleSpec =
  describe "Testing module" $ do
    cases <- runIO (discoverTestCases testCasesPath)

    let compilation = compileN

    curr <- runIO (getCurrentDirectory)
    runIO (setCurrentDirectory $ curr </> testCasesPath)

    cp <- runIO getClassPath
    let p = (proc "java" ["-cp", cp, namespace ++ "FileServer", cp])
              {std_in = CreatePipe, std_out = CreatePipe}
    (Just inP, Just outP, _, proch) <- runIO $ createProcess p
    runIO $ hSetBuffering inP NoBuffering
    runIO $ hSetBuffering outP NoBuffering

    forM_ cases (testConcreteSyn inP outP compilation)

    runIO (setCurrentDirectory curr)
