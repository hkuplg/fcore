{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , OverlappingInstances
           , RankNTypes
           , TypeOperators
           , RecordWildCards
           , TemplateHaskell
           #-}

module Main (main, TransMethod) where

import Assertions () -- Import this just to run static assertions at compile time.

import JavaUtils
import MonadLib
import Translations

import System.Console.CmdArgs -- Neil Mitchell's CmdArgs library
import System.Directory          (doesFileExist)
import System.Environment        (getArgs, withArgs)
import System.FilePath           (takeBaseName)
import System.IO

import Data.FileEmbed            (embedFile)
import qualified Data.ByteString (ByteString, writeFile)

data Options = Options
    { optCompile       :: Bool
    , optCompileAndRun :: Bool
    , optSourceFiles   :: [String]
    , optDump          :: DumpOption
    , optTransMethod   :: TransMethod
    , optVerbose       :: Bool
    } deriving (Eq, Show, Data, Typeable)

data TransMethod = Naive
                 | ApplyOpt
                 | ApplyU
                 | ApplyNew
                 | Stack
                 | Stack2
                 | Unbox
                 | StackU
                 | StackN
                 | StackAU
                 | StackAU1
                 | StackAU2
                 | BenchN
                 | BenchS
                 | BenchNA
                 | BenchSA
                 | BenchSAU
                 | BenchSAU1
                 | BenchSAU2
                 | BenchSAI1
                 | BenchSAI2
                 deriving (Eq, Show, Data, Typeable)

optionsSpec :: Options
optionsSpec = Options
  { optCompile = False &= explicit &= name "c" &= name "compile" &= help "Compile Java source"
  , optCompileAndRun = False &= explicit &= name "r" &= name "run" &= help "Compile & run Java source"
  , optDump = NoDump &= explicit &= name "d" &= name "dump" &= typ "TYPE"
           &= help ("Dump intermediate representations; " ++
                    "options: `core`, `simplecore`, `closuref`")
  , optSourceFiles = [] &= args &= typ "SOURCE FILES"
  , optTransMethod = ApplyOpt &= explicit &= name "m" &= name "method" &= typ "METHOD"
                  &= help ("Translations method; " ++
                           "options: `naive`, `applyopt` (default), `stack`")
  , optVerbose = False &= explicit &= name "v" &= name "verbose" &= help "Verbose"
  }
  &= helpArg [explicit, name "help", name "h"]
  &= program "f2j"
  &= summary "SystemF to Java compiler"

getOpts :: IO Options
getOpts = cmdArgs optionsSpec -- cmdArgs :: Data a => a -> IO a

runtimeBytes :: Data.ByteString.ByteString
runtimeBytes = $(embedFile "runtime/runtime.jar")

main :: IO ()
main = do
  rawArgs <- getArgs
  -- If the user did not specify any arguments, pretend as "--help" was given
  Options{..} <- (if null rawArgs then withArgs ["--help"] else id) getOpts

  -- Write the bytes of runtime.jar to file
  exists <- doesFileExist =<< getRuntimeJarPath
  existsCur <- doesFileExist "./runtime.jar"
  unless (exists || existsCur) $ Data.ByteString.writeFile "./runtime.jar" runtimeBytes
  forM_ optSourceFiles (\source_path ->
    do let output_path      = inferOutputPath source_path
           translate_method = optTransMethod
       when optVerbose $ do
         putStrLn (takeBaseName source_path ++ " using " ++ show translate_method)
         putStrLn ("  Compiling to Java source code ( " ++ output_path ++ " )")
       case translate_method of Naive    -> compilesf2java 0 optDump compileN source_path output_path
                                ApplyOpt -> compilesf2java 0 optDump compileAO source_path output_path
                                ApplyNew -> compilesf2java 0 optDump compileAONew source_path output_path
                                ApplyU -> compilesf2java 0 optDump compileAoptUnbox source_path output_path
                                Stack    -> compilesf2java 0 optDump compileS source_path output_path
                                Stack2    -> compilesf2java 0 optDump compileS2 source_path output_path
                                StackAU -> compilesf2java 0 optDump compileSAU source_path output_path
                                StackAU1 -> compilesf2java 1 optDump compileSAU source_path output_path
                                StackAU2 -> compilesf2java 2 optDump compileSAU source_path output_path
                                StackN    -> compilesf2java 0 optDump compileSN source_path output_path
                                StackU    -> compilesf2java 0 optDump compileSU source_path output_path
                                Unbox -> compilesf2java 0 optDump compileUnbox source_path output_path
                                BenchN    -> compilesf2java 0 optDump  (compileBN False) source_path output_path
                                BenchS    -> compilesf2java 0 optDump (compileBS False) source_path output_path
                                BenchNA   -> compilesf2java 0 optDump (compileBN True) source_path output_path
                                BenchSA   -> compilesf2java 0 optDump (compileBS True) source_path output_path
                                BenchSAI1 -> compilesf2java 1 optDump (compileBS True) source_path output_path
                                BenchSAI2 -> compilesf2java 2 optDump (compileBS True) source_path output_path
                                BenchSAU   -> compilesf2java 0 optDump (compileBSAU) source_path output_path
                                BenchSAU1   -> compilesf2java 1 optDump (compileBSAU) source_path output_path
                                BenchSAU2   -> compilesf2java 2 optDump (compileBSAU) source_path output_path


       when (optCompile || optCompileAndRun) $
         do when optVerbose (putStrLn "  Compiling to Java bytecode")
            compileJava output_path
       when optCompileAndRun $
         do when optVerbose $ do { putStr "  Running Java\n  Output: "; hFlush stdout }
            runJava output_path)
