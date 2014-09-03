{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module JavaUtils
  ( runtimeJarPath
  , getClassPath
  , compileJava, runJava
  , inferOutputPath, inferClassName
  , ClassName, MethodName, FieldName
  ) where

import StringUtils       (capitalize)

import System.FilePath   (takeDirectory, takeBaseName, (</>))
import System.Directory  (setCurrentDirectory, getCurrentDirectory, getHomeDirectory)
import System.Process    (system)

type ClassName  = String
type MethodName = String
type FieldName  = String

runtimeJarPath :: IO FilePath
runtimeJarPath
  = do h <- getHomeDirectory
       return $ h ++ "/.cabal/share/systemfcompiler-0.1.0.0/runtime/runtime.jar"

getClassPath :: IO FilePath
getClassPath = do r <- runtimeJarPath
                  return $ r ++ ":./runtime.jar:. "

inferOutputPath :: FilePath -> FilePath
inferOutputPath srcPath = directory </> className ++ ".java"
    where directory = takeDirectory srcPath
          className = capitalize $ takeBaseName srcPath

inferClassName :: FilePath -> String
inferClassName outputPath = capitalize $ takeBaseName outputPath

compileJava :: FilePath -> IO ()
compileJava srcPath
  = do cp <- getClassPath
       system ("javac -cp " ++ cp ++ srcPath)
       return ()

runJava :: FilePath -> IO ()
runJava srcPath = do
    currDir <- getCurrentDirectory
    let workDir = takeDirectory srcPath
    setCurrentDirectory workDir
    cp <- getClassPath
    system $ "java -cp " ++ currDir ++ "/runtime.jar:" ++ cp ++ takeBaseName srcPath
    system "rm *.class"
    setCurrentDirectory currDir