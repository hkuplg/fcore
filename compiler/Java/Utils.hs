{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Java.Utils
    ( compileJava
    , runJava
    , inferOutputPath
    , inferClassName
    , ClassName(..)
    ) where

import System.Cmd               (system)
import System.Directory         (setCurrentDirectory)
import System.FilePath          (takeDirectory, takeBaseName, (</>))
------
import Data.String.Utils        (capitalize)

newtype ClassName = ClassName String deriving (Eq, Show)

inferOutputPath :: FilePath -> FilePath
inferOutputPath srcPath = directory </> className ++ ".java"
    where directory = takeDirectory srcPath
          className = capitalize $ takeBaseName srcPath

inferClassName :: FilePath -> String
inferClassName outputPath = capitalize $ takeBaseName outputPath

compileJava :: FilePath -> IO ()
compileJava srcPath = system ("javac " ++ srcPath) >> return ()

runJava :: FilePath -> IO ()
runJava srcPath = do
    -- Must "cd" into that directory in order to run the compiled Java code
    setCurrentDirectory (takeDirectory srcPath)
    system $ "java "  ++ takeBaseName srcPath
    system $ "rm *.class"
    setCurrentDirectory ".."
