{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , OverlappingInstances 
           , RankNTypes
           , TypeOperators 
           #-}  

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Compilations 
    ( Compilation
    , compileN
    , compileAO
    , compilesf2java
    ) where

import Control.Monad            (when)

import qualified Data.Map  as Map

import Language.Java.Pretty
import Language.Java.Syntax as J

import System.Console.CmdArgs   -- The CmdArgs package
import System.Environment       (getArgs, withArgs)
import System.FilePath          (takeDirectory, takeBaseName, takeFileName, (</>))
import System.IO                (hFlush, stdout)

------

import Language.SystemF.Syntax
import qualified Language.SystemF.Parser (reader)
import ClosureF
import Language.Java.Utils               (ClassName(..), inferClassName)

import BaseTransCFJava
import ApplyTransCFJava

import Inheritance
import MonadLib
import Translations

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

-- compilePretty :: PFExp Int (Var, PCTyp Int) -> IO ()
-- compilePretty e = let (b,e1,t) = compile e in (prettyJ b >> prettyJ e1 >> print t)

-- compileCU :: String -> PFExp Int (Var, PCTyp Int) -> Maybe String -> IO ()
-- compileCU className e (Just nameStr) = let (cu,t) = createCU className (compile e) (Just nameStr) in (prettyJ cu >> print t)
-- compileCU className e Nothing = let (cu,t) = createCU className (compile e) Nothing in (prettyJ cu >> print t)

-- SystemF to Java
sf2java :: Compilation -> ClassName -> String -> String
sf2java compilation (ClassName className) src = 
    let (cu, _) = createCU className (compilation (Language.SystemF.Parser.reader src)) Nothing in prettyPrint cu

compilesf2java :: Compilation -> FilePath -> FilePath -> IO ()
compilesf2java compilation srcPath outputPath = do
    src <- readFile srcPath
    let output = sf2java compilation (ClassName (inferClassName outputPath)) src
    writeFile outputPath output

type Compilation = PFExp Int (Var, PCTyp Int) -> (Block, Exp, PCTyp Int)

-- setting
type AOptType = StateT Int (StateT (Map.Map J.Exp Int) (Writer Bool)) 

aoptinst :: ApplyOptTranslate AOptType  -- instantiation; all coinstraints resolved
aoptinst = applyopt

translate ::  PCExp Int (Var, PCTyp Int) -> MAOpt ([BlockStmt], Exp, PCTyp Int)
translate = translateM (up sopt) 

translateAO :: PCExp Int (Var, PCTyp Int) -> AOptType ([BlockStmt], Exp, PCTyp Int)
translateAO = translateM (up aoptinst) 

compileAO :: Compilation
compileAO e = 
  case fst $ runWriter $ evalStateT (evalStateT (translateAO (fexp2cexp e)) 0) Map.empty of
      (ss,exp,t) -> (J.Block ss,exp, t)

type NType = StateT Int (State (Map.Map J.Exp Int)) 
ninst :: Translate NType  -- instantiation; all coinstraints resolved
ninst = naive

translateN ::  PCExp Int (Var, PCTyp Int) -> NType ([BlockStmt], Exp, PCTyp Int)
translateN = translateM (up ninst)

compileN :: Compilation
compileN e = 
  case evalState (evalStateT (translateN (fexp2cexp e)) 0) Map.empty of
      (ss,exp,t) -> (J.Block ss,exp, t)
