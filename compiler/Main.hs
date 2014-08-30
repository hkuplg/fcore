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

-- The CmdArgs package
import System.Console.CmdArgs

import System.Environment        (getArgs, withArgs)
import System.FilePath           (takeFileName)
import System.Directory          (doesFileExist)
import System.IO
import Data.FileEmbed            (embedFile)
import qualified Data.ByteString (ByteString, writeFile)

import JavaUtils
import MonadLib
import Translations

data Options = Options
    { optCompile       :: Bool
    , optCompileAndRun :: Bool
    , optSourceFiles   :: [String]
    -- , optShowOpts      :: Bool
    , optDump          :: Bool
    , optTransMethod   :: TransMethod
    } deriving (Eq, Show, Data, Typeable)

data TransMethod = Naive | ApplyOpt | Stack deriving (Eq, Show, Data, Typeable)

optionsSpec :: Options
optionsSpec = Options
    { optCompile       = False &= explicit &= name "c" &= name "compile"         &= help "Compile Java source"
    , optCompileAndRun = False &= explicit &= name "r" &= name "compile-and-run" &= help "Compile & run Java source"
    -- , optShowOpts      = False &= explicit &= name "d" &= name "debug"           &= help "Show the parsed options"
    , optDump          = False &= explicit &= name "d" &= name "dump" &= help "Print expressions in the pipeline to stdout"
    , optSourceFiles   = []    &= args     &= typ "<source files>"
    , optTransMethod   = ApplyOpt
                      &= explicit &= name "m" &= name "method"
                      &= typ "<method>"
                      &= help (unwords [ "Translations method."
                                       , "Can be either 'naive', 'applyopt', or 'stack' (use without quotes)."
                                       , "The default is 'applyopt'."
                                       ])
    }

getOpts :: IO Options
getOpts = cmdArgs $ optionsSpec -- cmdArgs :: Data a => a -> IO a
    &= helpArg [explicit, name "help", name "h"]
    &= program "f2j"
    &= summary "SystemF to Java compiler"

-- withMessage :: String -> IO () -> IO ()
-- withMessage msg act = do { putStr msg; hFlush stdout; act }

-- withMessageLn :: String -> IO () -> IO ()
-- withMessageLn msg act = do { withMessage msg act; putStrLn "" }

runtimeBytes :: Data.ByteString.ByteString
runtimeBytes = $(embedFile "runtime/runtime.jar")

main :: IO ()
main = do
    rawArgs <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    Options{..} <- (if null rawArgs then withArgs ["--help"] else id) getOpts

    -- when (optShowOpts) $ putStrLn (show Options{..} ++ "\n")

    -- Write the bytes of runtime.jar to file
    exists <- doesFileExist =<< runtimeJarPath
    existsCur <- doesFileExist "./runtime.jar"
    unless (exists || existsCur) $ Data.ByteString.writeFile "./runtime.jar" runtimeBytes

    forM_
      (optSourceFiles)
      (\srcPath -> do
        putStrLn (takeFileName srcPath)
        let outputPath = inferOutputPath srcPath
        let method = optTransMethod
        -- putStrLn (concat ["  Compiling System F to Java using ", show method, " ( ", outputPath, " )"])
        compilesf2java optDump
          (case method of Naive    -> compileN
                          ApplyOpt -> compileAO
                          Stack    -> compileS)
          srcPath outputPath

        when (optCompile || optCompileAndRun) $
          do putStrLn "  Compiling Java"
             compileJava outputPath

        when optCompileAndRun $
          do putStrLn "  Running Java\n  Output: "
             runJava outputPath)