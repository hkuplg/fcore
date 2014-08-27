{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module JavaUtils
    ( runtimeJarPath
    , classpath
    , compileJava
    , runJava
    , inferOutputPath
    , inferClassName
    , ClassName(..)
    ) where

import System.Process           (system)
import System.Directory         (setCurrentDirectory, getCurrentDirectory, getHomeDirectory)
import System.FilePath          (takeDirectory, takeBaseName, (</>))
------
import Data.String.Utils        (capitalize)

newtype ClassName = ClassName String deriving (Eq, Show)

runtimeJarPath :: IO FilePath
runtimeJarPath = do h <- getHomeDirectory
                    return $ h ++ "/.cabal/share/systemfcompiler-0.1.0.0/runtime/runtime.jar"

classpath :: IO FilePath
classpath = do r <- runtimeJarPath
               return $ r ++ ":./runtime.jar:. "

inferOutputPath :: FilePath -> FilePath
inferOutputPath srcPath = directory </> className ++ ".java"
    where directory = takeDirectory srcPath
          className = capitalize $ takeBaseName srcPath

inferClassName :: FilePath -> String
inferClassName outputPath = capitalize $ takeBaseName outputPath

compileJava :: FilePath -> IO ()
compileJava srcPath = do cp <- classpath
                         system ("javac -cp " ++ cp ++ srcPath)
                         return ()

runJava :: FilePath -> IO ()
runJava srcPath = do
    currDir <- getCurrentDirectory
    let workDir = takeDirectory srcPath
    setCurrentDirectory workDir
    cp <- classpath
    system $ "java -cp " ++ currDir ++ "/runtime.jar:" ++ cp ++ takeBaseName srcPath
    system "rm *.class"
    setCurrentDirectory currDir
