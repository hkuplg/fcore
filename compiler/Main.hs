{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , OverlappingInstances
           , RankNTypes
           , TypeOperators
           #-}

module Main (main, TransMethod) where

-- The CmdArgs package
import System.Console.CmdArgs

import System.Environment       (getArgs, withArgs)
import System.FilePath          (takeFileName)
import System.IO                (hFlush, stdout)

import Language.Java.Utils
import MonadLib
import Translations

data Options = Options
    { optCompile       :: Bool
    , optCompileAndRun :: Bool
    , optSourceFiles   :: [String]
    , optDebug         :: Bool
    , optTransMethod   :: TransMethod
    } deriving (Eq, Show, Data, Typeable)

data TransMethod = Naive | ApplyOpt | Stack deriving (Eq, Show, Data, Typeable)

optionsSpec :: Options
optionsSpec = Options
    { optCompile       = False &= explicit &= name "c" &= name "compile"         &= help "Compile Java source"
    , optCompileAndRun = False &= explicit &= name "r" &= name "compile-and-run" &= help "Compile & run Java source"
    , optDebug         = False &= explicit &= name "d" &= name "debug"           &= help "Show debug information"
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

withMessage :: String -> IO () -> IO ()
withMessage msg act = do { putStr msg; hFlush stdout; act }

withMessageLn :: String -> IO () -> IO ()
withMessageLn msg act = do { withMessage msg act; putStrLn "" }

main :: IO ()
main = do
    rawArgs <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null rawArgs then withArgs ["--help"] else id) getOpts
    when (optDebug opts) $ putStrLn (show opts ++ "\n")
    forM_ (optSourceFiles opts) (\srcPath -> do
        putStrLn (takeFileName srcPath)
        let outputPath = inferOutputPath srcPath

        let method = optTransMethod opts
        withMessageLn (concat ["  Compiling System F to Java using ", show method, " ( ", outputPath, " )"]) $
            compilesf2java (case method of
                                Naive    -> compileN
                                ApplyOpt -> compileAO
                                Stack -> compileS
                           ) srcPath outputPath

        when (optCompile opts || optCompileAndRun opts) $ withMessageLn "  Compiling Java" $ compileJava outputPath
        when (optCompileAndRun opts) $ withMessage "  Running Java\n  Output: " $ runJava outputPath)
