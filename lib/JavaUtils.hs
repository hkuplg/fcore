{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{- |
Module      :  JavaUtils
Description :  Utilities for compiling and running Java from Haskell, etc.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>
Stability   :  experimental
Portability :  portable
-}

module JavaUtils
  ( getRuntimeJarPath
  , getClassPath
  , compileJava, runJava
  , inferOutputPath, inferClassName
  , ClassName, MethodName, FieldName
  ) where

import StringUtils (capitalize)

import System.Directory (setCurrentDirectory, getCurrentDirectory, getTemporaryDirectory)
import System.FilePath (takeDirectory, takeFileName, takeBaseName, replaceExtension, (</>), (<.>), dropExtension)
import System.Process (system)

type ClassName  = String
type MethodName = String
type FieldName  = String

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
inferOutputPath source_path =
  let fileName = (dropExtension . takeFileName $ source_path) ++ "s" -- avoid name clash
  in takeDirectory source_path </>
     (capitalize fileName) <.> "java"

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
