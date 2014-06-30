{-# LANGUAGE RankNTypes, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}     -- Required by Neil Mitchell's CmdArgs package
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Main where

import Control.Monad            (when)
import qualified Data.Char as Char (toUpper)
import qualified Data.Map  as Map
import Language.Java.Pretty
import Language.Java.Syntax as J
import System.Cmd               (system)
import System.Console.CmdArgs   -- The CmdArgs package
import System.Directory         (setCurrentDirectory)
import System.Environment       (getArgs, withArgs)
import System.FilePath          (takeDirectory, takeBaseName, takeFileName, (</>))
import System.IO                (hFlush, stdout)

import BaseTransCFJava
import ClosureF
import Inheritance
import MonadLib
import Translations
import Language.SystemF.Syntax
import qualified Language.SystemF.Parser (reader)

type M1 = StateT (Map.Map String Int) (State Int)

type M2 = StateT Int (State (Map.Map J.Exp Int)) 
type M3 = StateT Int (Writer Bool) 

type MAOpt = StateT Int (StateT (Map.Map J.Exp Int) (Writer Bool)) 

sopt :: Translate MAOpt  -- instantiation; all coinstraints resolved
sopt = naive

{-
sopt :: ApplyOptTranslate MAOpt 
sopt = applyopt
-}

translate ::  PCExp Int (Var, PCTyp Int) -> MAOpt ([BlockStmt], Exp, PCTyp Int)
translate = translateM (up sopt) 

compile ::  PFExp Int (Var, PCTyp Int) -> (Block, Exp, PCTyp Int)
compile e = 
  case fst $ runWriter $ evalStateT (evalStateT (translate (fexp2cexp e)) 0) Map.empty of
      (ss,e1,t) -> (J.Block ss,e1, t)

{-
type MAOpt = StateT Int (StateT (Map J.Exp Int) (Reader (Set.Set Int))) 
sopt :: Translate MAOpt  -- instantiation; all coinstraints resolved
sopt = naive

translate ::  PCExp Int (Var, PCTyp Int) -> MAOpt ([BlockStmt], Exp, PCTyp Int)
translate e = translateM (up sopt) e

compile ::  PFExp Int (Var, PCTyp Int) -> (Block, Exp, PCTyp Int)
compile e = 
  case runReader (evalStateT (evalStateT (translate (fexp2cexp e)) 0) empty) Set.empty of
      (ss,exp,t) -> (J.Block ss,exp, t)
-}
{-
sopt :: ApplyOptTranslate Translate MAOpt  -- instantiation; all coinstraints resolved
sopt = applyopt

translate e = translateM (to sopt) e
-}
{-
sopt :: TranslateStack (ApplyOptTranslate Translate) MAOpt
sopt = stack

translate e = translateM (to sopt) e
-}

prettyJ :: Pretty a => a -> IO ()
prettyJ = putStrLn . prettyPrint

compilePretty :: PFExp Int (Var, PCTyp Int) -> IO ()
compilePretty e = let (b,e1,t) = compile e in (prettyJ b >> prettyJ e1 >> print t)

compileCU :: String -> PFExp Int (Var, PCTyp Int) -> Maybe String -> IO ()
compileCU className e (Just nameStr) = let (cu,t) = createCU className (compile e) (Just nameStr) in (prettyJ cu >> print t)
compileCU className e Nothing = let (cu,t) = createCU className (compile e) Nothing in (prettyJ cu >> print t)

-- SystemF to Java
sf2java :: String -> String -> String
sf2java className src = let (cu, _) = createCU className (compile (Language.SystemF.Parser.reader src)) Nothing in prettyPrint cu

-- `compilesf2java srcPath outputPath` loads a SystemF file at `srcPath`,
-- and writes the compiled Java code to `outputPath`.
-- Example:
--      compilesf2java "id.sf" "id.java"
compilesf2java :: FilePath -> FilePath -> IO ()
compilesf2java srcPath outputPath = do
    src <- readFile srcPath
    let output = sf2java (inferClassName outputPath) src
    writeFile outputPath output

compileJava :: FilePath -> IO ()
compileJava srcPath = system ("javac " ++ srcPath) >> return ()

runJava :: FilePath -> IO ()
runJava srcPath = do
    -- Must "cd" into that directory in order to run the compiled Java code
    setCurrentDirectory (takeDirectory srcPath)
    system $ "java "  ++ takeBaseName srcPath
    system $ "rm *.class"
    setCurrentDirectory ".."

inferOutputPath :: FilePath -> FilePath
inferOutputPath srcPath = directory </> className ++ ".java"
    where directory = takeDirectory srcPath
          className = capitalize $ takeBaseName srcPath

inferClassName :: FilePath -> String
inferClassName outputPath = capitalize $ takeBaseName outputPath

capitalize :: String -> String
capitalize "" = ""
capitalize (s:ss) = Char.toUpper s : ss

data Options = Options
    { optCompile       :: Bool
    , optCompileAndRun :: Bool
    , optSourceFiles   :: [String]
    , optDebug         :: Bool
    } deriving (Eq, Show, Data, Typeable)

optionsSpec :: Options
optionsSpec = Options
    { optCompile       = False &= explicit &= name "c" &= name "compile"         &= help "Compile Java source"
    , optCompileAndRun = False &= explicit &= name "r" &= name "compile-and-run" &= help "Compile & run Java source"
    , optDebug         = False &= explicit &= name "d" &= name "debug"           &= help "Show debug information"
    , optSourceFiles   = []    &= args     &= typ "<source files>"
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
    args <- getArgs
    -- If the user did not specify any arguments, pretend as "--help" was given
    opts <- (if null args then withArgs ["--help"] else id) getOpts
    when (optDebug opts) $ putStrLn (show opts ++ "\n")
    forM_ (optSourceFiles opts) (\srcPath -> do
        putStrLn (takeFileName srcPath) 
        let outputPath = inferOutputPath srcPath
        withMessageLn ("  Compiling System F to Java (" ++ outputPath ++ ")") $ compilesf2java srcPath outputPath 
        when (optCompile opts || optCompileAndRun opts) $ withMessageLn "  Compiling Java" $ compileJava outputPath
        when (optCompileAndRun opts) $ withMessage "  Running Java\n  Output: " $ runJava outputPath)
