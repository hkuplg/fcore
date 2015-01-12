{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module JavaUtils
  ( getRuntimeJarPath
  , getClassPath
  , compileJava, runJava
  , inferOutputPath, inferClassName
  , writeRuntimeToTemp
  , ClassName, MethodName, FieldName
  ) where

import StringUtils       (capitalize)

import System.FilePath   (takeDirectory, takeFileName, takeBaseName, replaceExtension, (</>))
import System.Directory  (setCurrentDirectory, getCurrentDirectory, getHomeDirectory, getTemporaryDirectory)
import System.Process    (system)
import Data.ByteString as B

type ClassName  = String
type MethodName = String
type FieldName  = String


writeRuntimeToTemp :: B.ByteString -> IO ()
writeRuntimeToTemp bytes = do tempdir <- getTemporaryDirectory
                              let tempFile = tempdir </> "runtime.jar"
                              B.writeFile tempFile bytes

getRuntimeJarPath :: IO FilePath
getRuntimeJarPath =
  do tempdir <- getTemporaryDirectory
     return (tempdir </> "runtime.jar")

getClassPath :: IO FilePath
getClassPath = do r <- getRuntimeJarPath
                  return $ r ++ ":."

-- Given the path to the source file,
-- infer the output path for the corresponding Java source.
-- "tests/pinepine/even_odd.sf" => "tests/pinepine/Even_odd.java"
inferOutputPath :: FilePath -> FilePath
inferOutputPath source_path
  = takeDirectory source_path </> capitalize (replaceExtension (takeFileName source_path) "java")

inferClassName :: FilePath -> String
inferClassName outputPath = capitalize $ takeBaseName outputPath

compileJava :: FilePath -> IO ()
compileJava srcPath
  = do cp <- getClassPath
       system ("javac -cp " ++ cp ++ " " ++ srcPath)
       return ()

runJava :: FilePath -> IO ()
runJava srcPath = do
    currDir <- getCurrentDirectory
    let workDir = takeDirectory srcPath
    setCurrentDirectory workDir
    cp <- getClassPath
    system $ "java -cp " ++ cp ++ " " ++ takeBaseName srcPath
    system "rm *.class"
    setCurrentDirectory currDir
