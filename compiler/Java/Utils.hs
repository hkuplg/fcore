{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Java.Utils
    ( runtimeJarPath
    , classpath
    , compileJava
    , runJava
    , inferOutputPath
    , inferClassName
    , ClassName(..)
    ) where

import System.Process           (system)
import System.Directory         (setCurrentDirectory, getCurrentDirectory)
import System.FilePath          (takeDirectory, takeBaseName, (</>))
------
import Data.String.Utils        (capitalize)

newtype ClassName = ClassName String deriving (Eq, Show)

runtimeJarPath = "~/.cabal/share/systemfcompiler-0.1.0.0/runtime/runtime.jar"
classpath = runtimeJarPath ++ ":./runtime.jar:. "

inferOutputPath :: FilePath -> FilePath
inferOutputPath srcPath = directory </> className ++ ".java"
    where directory = takeDirectory srcPath
          className = capitalize $ takeBaseName srcPath

inferClassName :: FilePath -> String
inferClassName outputPath = capitalize $ takeBaseName outputPath

compileJava :: FilePath -> IO ()
compileJava srcPath = system ("javac -cp " ++ classpath ++ srcPath) >> return ()

runJava :: FilePath -> IO ()
runJava srcPath = do
    currDir <- getCurrentDirectory
    let workDir = takeDirectory srcPath
    setCurrentDirectory workDir
    system $ "java -cp " ++ currDir ++ "/runtime.jar:" ++ classpath ++ takeBaseName srcPath
    system $ "rm *.class"
    setCurrentDirectory currDir
