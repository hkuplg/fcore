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
import Data.List                 (sort, group)
import qualified Data.ByteString (ByteString, writeFile)

type CompileOpt = (Int, Compilation)

data Options = Options
    { optCompile       :: Bool
    , optCompileAndRun :: Bool
    , optSourceFiles   :: [String]
    , optDump          :: DumpOption
    , optTransMethod   :: [TransMethod]
    , optVerbose       :: Bool
    } deriving (Eq, Show, Data, Typeable)

data TransMethod = Apply
                 | Naive
                 | Stack
                 | Unbox
                 | StackAU1
                 | StackAU2
                 | BenchN
                 | BenchS
                 | BenchNA
                 | BenchSA
                 | BenchSAI1
                 | BenchSAI2
                 deriving (Eq, Show, Data, Typeable, Ord)

optionsSpec :: Options
optionsSpec = Options
  { optCompile = False &= explicit &= name "c" &= name "compile" &= help "Compile Java source"
  , optCompileAndRun = False &= explicit &= name "r" &= name "run" &= help "Compile & run Java source"
  , optDump = NoDump &= explicit &= name "d" &= name "dump" &= typ "TYPE"
           &= help ("Dump intermediate representations; " ++
                    "options: `core`, `simplecore`, `closuref`")
  , optSourceFiles = [] &= args &= typ "SOURCE FILES"
  , optTransMethod = [] &= explicit &= name "m" &= name "method" &= typ "METHOD"
                  &= help ("Translations method." ++
                           "Can be either 'naive', 'apply', 'stack', and/or 'unbox'" ++
                           "(use without quotes)." ++
                           "The default is 'naive'.")
  , optVerbose = False &= explicit &= name "v" &= name "verbose" &= help "Verbose"
  }
  &= helpArg [explicit, name "help", name "h"]
  &= program "f2j"
  &= summary "SystemF to Java compiler"

getOpts :: IO Options
getOpts = cmdArgs optionsSpec -- cmdArgs :: Data a => a -> IO a

-- runtimeBytes :: Data.ByteString.ByteString
-- runtimeBytes = $(embedFile "runtime/runtime.jar")

main :: IO ()
main = do
  rawArgs <- getArgs
  -- If the user did not specify any arguments, pretend as "--help" was given
  Options{..} <- (if null rawArgs then withArgs ["--help"] else id) getOpts

  -- Write the bytes of runtime.jar to file
  -- exists <- doesFileExist =<< getRuntimeJarPath
  -- existsCur <- doesFileExist "./runtime.jar"
  -- unless (exists || existsCur) $ Data.ByteString.writeFile "./runtime.jar" runtimeBytes
  forM_ optSourceFiles (\source_path ->
    do let output_path      = inferOutputPath source_path
           translate_method = optTransMethod
           sort_and_rmdups  = map head . group . sort . ((++) [Naive])
       putStrLn (takeBaseName source_path ++ " using " ++ show (sort_and_rmdups translate_method))
       putStrLn ("  Compiling to Java source code ( " ++ output_path ++ " )")
       (num, opt) <- getOpt (sort_and_rmdups translate_method)
       compilesf2java num optDump opt source_path output_path
       when (optCompile || optCompileAndRun) $
         do when optVerbose (putStrLn "  Compiling to Java bytecode")
            compileJava output_path
       when optCompileAndRun $
         do when optVerbose $ do { putStr "  Running Java\n  Output: "; hFlush stdout }
            runJava output_path)

getOpt :: [TransMethod] -> IO CompileOpt
getOpt translate_method = case translate_method of
  [Apply, Naive]               -> return (0, compileAO)
  [Apply, Naive, Unbox]        -> return (0, compileAoptUnbox)
  [Apply, Naive, Stack]        -> return (0, compileS)
  [Apply, Naive, Stack, Unbox] -> return (0, compileSAU)
  [Naive, StackAU1]            -> return (1, compileSAU)
  [Naive, StackAU2]            -> return (2, compileSAU)
  [Naive, Stack]               -> return (0, compileSN)
  [Naive, Stack, Unbox]        -> return (0, compileSU)
  [Naive, Unbox]               -> return (0, compileUnbox)
  -- [BenchS]                     -> return (0, compileBS False)
  -- [BenchNA]                    -> return (0, compileBN True)
  -- [BenchSA]                    -> return (0, compileBS True)
  -- [BenchSAI1]                  -> return (1, compileBS True)
  -- [BenchSAI2]                  -> return (2, compileBS True)
  _                            -> return (0, compileN) -- Naive is the default
