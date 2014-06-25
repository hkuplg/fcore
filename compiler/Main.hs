{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -XMultiParamTypeClasses -XRankNTypes -XFlexibleContexts -XTypeOperators  -XOverlappingInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Main where

import Data.List
import qualified Data.Char as Char (toUpper)
import Data.Map
import qualified Data.Set as Set
import Language.Java.Pretty
import Language.Java.Syntax as J
import Prelude hiding (const)
import System.Directory
import qualified System.Environment       (getArgs)
import System.Exit
import System.FilePath
import System.Process

-- import HMParser         (readHM)
-- import qualified HM
import ApplyTransCFJava
import BaseTransCFJava
import BaseTransCFJava (createCU)
import ClosureF
import Inheritance
import MonadLib
import SystemF.Syntax
import Translations
import qualified SystemF.Parser
import qualified TestSuite as TS

type M1 = StateT (Map String Int) (State Int)

type M2 = StateT Int (State (Map J.Exp Int)) 
type M3 = StateT Int (Writer Bool) 

type MAOpt = StateT Int (StateT (Map J.Exp Int) (Writer Bool)) 

sopt :: Translate MAOpt  -- instantiation; all coinstraints resolved
sopt = naive

translate ::  PCExp Int (Var, PCTyp Int) -> MAOpt ([BlockStmt], Exp, PCTyp Int)
translate e = translateM (up sopt) e

compile ::  PFExp Int (Var, PCTyp Int) -> (Block, Exp, PCTyp Int)
compile e = 
  case fst $ runWriter $ (evalStateT (evalStateT (translate (fexp2cexp e)) 0) empty) of
      (ss,exp,t) -> (J.Block ss,exp, t)

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




compilePretty e = let (b,exp,t) = compile e in (prettyJ b >> prettyJ exp >> putStrLn (show t))

compileCU className e (Just nameStr) = let (cu,t) = (createCU className (compile e) (Just nameStr)) in (prettyJ cu >> putStrLn (show t))

compileCU className e Nothing = let (cu,t) = (createCU className (compile e) Nothing) in (prettyJ cu >> putStrLn (show t))

-- SystemF to Java
sf2java :: String -> String -> String
sf2java className src = let (cu, _) = (createCU className (compile (SystemF.Parser.reader src)) Nothing) in prettyPrint cu

-- `compilesf2java srcPath outputPath` loads a SystemF file at `srcPath`,
-- and writes the compiled Java code to `outputPath`.
-- Example:
--      compilesf2java "id.sf" "id.java"
compilesf2java :: FilePath -> FilePath -> IO ()
compilesf2java srcPath outputPath = do
    src <- readFile srcPath
    let output = sf2java (inferClassName outputPath) src
    writeFile outputPath output

inferOutputPath :: String -> String
inferOutputPath srcPath = directory </> className ++ ".java"
    where directory  = takeDirectory srcPath
          className  = capitalize $ takeBaseName srcPath

inferClassName :: String -> String
inferClassName outputPath = capitalize $ takeBaseName outputPath

capitalize :: String -> String
capitalize "" = ""
capitalize (s:ss) = Char.toUpper s : ss

main :: IO ()
main = do 
    args <- System.Environment.getArgs
    if length args < 1
        then putStrLn "Usage: f2j <source files>"
        else do 
            let inputPaths = args 
            forM_ inputPaths (\srcPath -> do
                let outputPath = inferOutputPath srcPath
                compilesf2java srcPath outputPath 
                putStrLn $ "Wrote " ++ outputPath)
