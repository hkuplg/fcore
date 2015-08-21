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
  , inferOutputPath, inferClassName, predefList
  , ClassName, MethodName, FieldName
  , ModuleName
  ) where

import Data.List (isSuffixOf)
import StringUtils (capitalize)
import System.IO.Unsafe (unsafePerformIO)

import Language.Haskell.TH.Syntax
import Paths_fcore
import System.Directory (setCurrentDirectory, getCurrentDirectory, getDirectoryContents)
import System.FilePath (takeDirectory, takeFileName, takeBaseName, (</>), (<.>), dropExtension, searchPathSeparator)
import System.Process.Extra (system_)


type ClassName  = String
type MethodName = String
type FieldName  = String
type ModuleName = String

getRuntimeJarPath :: IO FilePath
getRuntimeJarPath = getDataFileName "runtime/runtime.jar"

getClassPath :: IO FilePath
getClassPath = do r <- getRuntimeJarPath
                  return $ r ++ [searchPathSeparator] ++ "."

-- Given the path to the source file,
-- infer the output path for the corresponding Java source.
-- "tests/pinepine/even_odd.sf" => "tests/pinepine/Even_odd.java"
inferOutputPath :: FilePath -> FilePath
inferOutputPath source_path =
  let fileName = (dropExtension . takeFileName $ source_path) ++ "$" -- avoid name clash
  in takeDirectory source_path </>
     capitalize fileName <.> "java"

inferClassName :: FilePath -> String
inferClassName outputPath = capitalize $ takeBaseName outputPath

compileJava :: FilePath -> IO ()
compileJava srcPath
  = do cp <- getClassPath
       system_ ("javac -cp " ++ cp ++ " " ++ srcPath)

runJava :: FilePath -> IO ()
runJava srcPath = do
    currDir <- getCurrentDirectory
    let workDir = takeDirectory srcPath
    setCurrentDirectory workDir
    cp <- getClassPath
    system_ $ "java -cp " ++ cp ++ " " ++ takeBaseName srcPath
    system_ "rm *.class"
    setCurrentDirectory currDir

-- Here we want to do "safe" compile-time computation to get the list
-- of pre-defined functions
getPredef :: [(Maybe String, String)]
getPredef =
  let filePaths = unsafePerformIO $ getDirectoryContents "lib/predef"
  in ((map (\path -> (Just "f2j.prelude", takeBaseName path)) (filter (isSuffixOf "sf") filePaths)))

predefList = [| $(lift getPredef) |]
