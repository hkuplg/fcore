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
    , optDump          :: Bool
    , optTransMethod   :: TransMethod
    } deriving (Eq, Show, Data, Typeable)

data TransMethod = Naive | ApplyOpt | Stack | BenchN | BenchS | BenchNA | BenchSA deriving (Eq, Show, Data, Typeable)

optionsSpec :: Options
optionsSpec = Options
  { optCompile = False &= explicit &= name "c" &= name "compile" &= help "Compile Java source"
  , optCompileAndRun = False &= explicit &= name "r" &= name "run" &= help "Compile & run Java source"
  , optDump = False &= explicit &= name "d" &= name "dump" &= help "Dump intermediate representations"
  , optSourceFiles = [] &= args &= typ "SOURCE FILES"
  , optTransMethod = ApplyOpt &= explicit &= name "m" &= name "method" &= typ "METHOD"
                  &= help ("Translations method." ++
                           "Can be either 'naive', 'applyopt', or 'stack'" ++
                           "(use without quotes)." ++
                           "The default is 'applyopt'.")
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
       putStrLn (takeBaseName source_path ++ " using " ++ show translate_method)
       putStrLn ("  Compiling to Java source code ( " ++ output_path ++ " )")
       compilesf2java optDump
         (case translate_method of Naive    -> compileN
                                   ApplyOpt -> compileAO
                                   Stack    -> compileS
                                   BenchN    -> compileBN False
                                   BenchS    -> compileBS False
                                   BenchNA   -> compileBN True
                                   BenchSA   -> compileBS True)
         source_path output_path

       when (optCompile || optCompileAndRun) $
         do putStrLn "  Compiling to Java bytecode"
            compileJava output_path
       when optCompileAndRun $
         do putStr "  Running Java\n  Output: "; hFlush stdout
            runJava output_path)
