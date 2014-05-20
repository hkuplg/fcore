{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -XMultiParamTypeClasses -XRankNTypes -XFlexibleContexts -XTypeOperators  -XOverlappingInstances #-}
module Main where

import qualified HM
import HMParser         (readHM)
import SystemFParser    (readSF)
import SystemF
import ClosureF
import BaseTransCFJava (createCU)

import Control.Monad.Identity
import Language.Java.Syntax as J
import StackTransCFJava
import ApplyTransCFJava
import BaseTransCFJava
import Translations
import Language.Java.Pretty
import MonadLib
import Data.Map
import Inheritance

import Prelude hiding (const)

type M1 = StateT (Map String Int) (State Int)

type M2 = StateT Int (State (Map J.Exp Int)) 
type M3 = StateT Int (Writer Bool) 

type MAOpt = StateT Int (StateT (Map J.Exp Int) (Writer Bool)) 

sopt :: TranslateStack MAOpt  -- instantiation; all coinstraints resolved
sopt = stackNaive

translate ::  PCExp Int (Var, PCTyp Int) -> MAOpt ([BlockStmt], Exp, PCTyp Int)
translate e = translateM (up sopt) e

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

compile ::  PFExp Int (Var, PCTyp Int) -> (Block, Exp, PCTyp Int)
compile e = 
  case fst $ runWriter (evalStateT (evalStateT (translate (fexp2cexp e)) 0) empty) of
      (ss,exp,t) -> (J.Block ss,exp, t)



compilePretty e = let (b,exp,t) = compile e in (prettyJ b >> prettyJ exp >> putStrLn (show t))

compileCU e (Just nameStr) = let (cu,t) = (createCU (compile e) (Just nameStr)) in (prettyJ cu >> putStrLn (show t))

compileCU e Nothing = let (cu,t) = (createCU (compile e) Nothing) in (prettyJ cu >> putStrLn (show t))

--TODO: add to execute the full compiler (starting with parser)
main = undefined