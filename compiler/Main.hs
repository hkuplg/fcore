{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , RankNTypes
           , RecordWildCards
           #-}
{- |
Module      :  Main
Description :  Main module for f2j.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>, Jeremy <bixuanxbi@gmail.com>
Stability   :  experimental
Portability :  non-portable (MPTC)
-}

module Main where

-- import           Assertions () -- Import this just to run static assertions at compile time.

import           BackEnd
import           FrontEnd (source2core)
import           JavaUtils
import           MonadLib

import           Data.Char (toLower)
import           Data.List (intercalate)
import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import           Options.Applicative
import           System.Environment (getArgs, withArgs)
import           System.FilePath (takeBaseName)
import           System.IO


data Options = Options
    { optCompile       :: Bool
    , optCompileAndRun :: Bool
    , optDump          :: DumpOption
    , optVerbose       :: Bool
    , optSilent        :: Bool
    , optInline        :: Bool
    , optTransMethod   :: [TransMethod]
    , optSourceFiles   :: [String]
    } deriving (Eq, Show)

data TransMethod = Apply
                 | Naive
                 | Stack
                 -- | Unbox
                 -- | StackAU1
                 -- | StackAU2
                 -- | BenchN
                 -- | BenchS
                 -- | BenchNA
                 -- | BenchSA
                 -- | BenchSAI1
                 -- | BenchSAI2
                 deriving (Eq, Show, Ord)

transOpts = Map.fromList [("apply", Apply), ("naive", Naive), ("stack", Stack)]

dumpOpts = Map.fromList
             [ ("parsed", Parsed)
             , ("tchecked", TChecked)
             , ("systemfi", SystemFI)
             , ("core", SimpleCore)
             , ("closuref", ClosureF)
             ]

intersperseComma :: Map.Map String a -> String
intersperseComma m = intercalate ", " (Map.keys m)

parseOpts :: Map.Map String a -> String -> ReadM a
parseOpts opts str =
  let parsed = Map.lookup (map toLower str) opts
  in case parsed of
    Just p  -> return p
    Nothing -> readerError $ "expected one of: " ++ intersperseComma opts

options :: Parser Options
options = Options <$> switch (long "compile" <> short 'c' <> help "Compile Java source")
                  <*> switch (long "run" <> short 'r' <> help "Compile & run Java source")
                  <*> option (str >>= parseOpts dumpOpts)
                        (long "dump" <>
                         value NoDump <>
                         short 'd' <>
                         metavar "TYPE" <>
                         completeWith (Map.keys dumpOpts) <>
                         help ("Dump option. Can be either " ++ intersperseComma dumpOpts))
                  <*> switch (long "verbose" <> short 'v' <> help "Whether to be verbose")
                  <*> switch (long "silent" <> help "Whether to keep silent")
                  <*> switch (long "inline" <> short 'i' <> help "Inline your program")
                  <*> many
                        (option (str >>= parseOpts transOpts)
                           (long "method" <>
                            short 'm' <>
                            metavar "METHOD" <>
                            completeWith (Map.keys transOpts) <>
                            help
                              ("Translation method. Can be either " ++ intersperseComma transOpts)))
                  <*> some (argument str (metavar "FILES..." <> action "file"))

getOpts :: IO Options
getOpts = execParser opts
  where
    opts = info (helper <*> options) (fullDesc <> header "f2j - SystemF to Java compiler")


main :: IO ()
main = do
  rawArgs <- getArgs
  -- If the user did not specify any arguments, pretend as "--help" was given
  Options{..} <- (if null rawArgs then withArgs ["--help"] else id) getOpts

  forM_ optSourceFiles (\source_path ->
    do let output_path      = inferOutputPath source_path
           translate_method = optTransMethod
           method = Set.insert Naive (Set.fromList translate_method)
       let opts = getOpt method

       unless optSilent $
         do putStrLn (takeBaseName source_path ++ " using " ++ (show . Set.toList $ method))
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
            runJava output_path)

getOpt :: Set.Set TransMethod -> Compilation
getOpt translate_method
       | translate_method == Set.fromList [Apply, Naive] = compileAO
       | translate_method == Set.fromList [Apply, Naive, Stack] = compileS
       | translate_method == Set.fromList [Naive, Stack] = compileSN
       | otherwise = compileN
