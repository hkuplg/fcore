{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts
           , TemplateHaskell
           , FlexibleInstances
           , MultiParamTypeClasses
           , RankNTypes
           , TypeOperators
           , RecordWildCards
           #-}
{- |
Module      :  Main
Description :  Main module for f2j.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>
Stability   :  experimental
Portability :  non-portable (MPTC)
-}

module Main where

-- import           Assertions () -- Import this just to run static assertions at compile time.

import           BackEnd
import           FrontEnd (source2core)
import           JavaUtils
import           MonadLib

import qualified Data.Set as Set
import           System.Console.CmdArgs -- Neil Mitchell's CmdArgs library
import           System.Environment (getArgs, withArgs)
import           System.FilePath (takeBaseName)
import           System.IO

-- type CompileOpt = (Int, Compilation)

data Options = Options
    { optCompile       :: Bool
    , optCompileAndRun :: Bool
    , optKeepClass     :: Bool
    , optSourceFiles   :: [String]
    , optDump          :: DumpOption
    , optTransMethod   :: Set.Set TransMethod
    , optVerbose       :: Bool
    , optInline        :: Bool
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
optionsSpec =
  Options {optCompile =
             False &= explicit &=
             name "c" &=
             name "compile" &=
             help "Compile Java source"
          ,optInline =
             False &= explicit &=
             name "i" &=
             name "inline" &=
             help "Inline your program"
          ,optCompileAndRun =
             False &= explicit &=
             name "r" &=
             name "run" &=
             help "Compile & run Java source"
          ,optKeepClass =
             False &= explicit &=
             name "k" &=
             name "keep" &=
             help "Keep generated .class files"
          ,optDump =
             NoDump &= explicit &=
             name "d" &=
             name "dump" &=
             typ "TYPE" &=
             help ("Dump intermediate representations; " ++
                   "options: `core`, `simplecore`, `closuref`")
          ,optSourceFiles =
             [] &= args &=
             typ "SOURCE FILES"
          ,optTransMethod =
             Set.empty &= explicit &=
             name "m" &=
             name "method" &=
             typ "METHOD" &=
             help ("Translations method." ++ "Can be either 'naive', 'apply', 'stack', and/or 'unbox'" ++
                                             "(use without quotes)." ++
                                             "The default is 'naive'.")
          ,optVerbose =
             False &= explicit &=
             name "v" &=
             name "verbose" &=
             help "Verbose"} &=
  helpArg [explicit,name "help",name "h"] &=
  program "f2j" &=
  summary "SystemF to Java compiler"

getOpts :: IO Options
getOpts = cmdArgs optionsSpec -- cmdArgs :: Data a => a -> IO a

main :: IO ()
main = do
  rawArgs <- getArgs
  -- If the user did not specify any arguments, pretend as "--help" was given
  Options{..} <- (if null rawArgs then withArgs ["--help"] else id) getOpts

  forM_ optSourceFiles (\source_path ->
    do let output_path      = inferOutputPath source_path
           translate_method = optTransMethod
           method = Set.insert Naive translate_method
       let opts = getOpt method

       putStrLn (takeBaseName source_path ++ " using " ++ (show . Set.toList $ method))
       putStrLn ("Compiling to Java source code ( " ++ output_path ++ " )")

       source     <- readFile source_path
       coreExpr   <- source2core optDump source
       javaSource <- core2java False optInline optDump opts (inferClassName output_path) coreExpr
       writeFile output_path javaSource

       when (optCompile || optCompileAndRun) $
         do when optVerbose (putStrLn "  Compiling to Java bytecode")
            compileJava output_path
       when optCompileAndRun $
         do when optVerbose $ do { putStr "  Running Java\n  Output: "; hFlush stdout }
            runJava optKeepClass output_path)

getOpt :: Set.Set TransMethod -> Compilation
getOpt translate_method
       | translate_method == Set.fromList [Apply, Naive] = compileAO
       | translate_method == Set.fromList [Apply, Naive, Stack] = compileS
       | translate_method == Set.fromList [Naive, Stack] = compileSN
       | otherwise = compileN
