{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , OverlappingInstances
           , RankNTypes
           , TypeOperators
           #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Translations
    ( Compilation
    , compileN
    , compileAO
    , compileS
    , compilesf2java
    ) where

import Prelude hiding (exp)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Language.Java.Pretty
import qualified Language.Java.Syntax as J

------

import Language.ESF.Parser      (readESF)
import Language.TransESF        (transESF)
import Language.SystemF.Syntax
import ClosureF
import Language.Java.Utils      (ClassName(..), inferClassName)

import BaseTransCFJava
import ApplyTransCFJava
import StackTransCFJava

import Inheritance
import MonadLib

-- Naive translation

naive :: (MonadState Int m, MonadState (Map.Map J.Exp Int) m, MonadState (Set.Set J.Exp) m) => Translate m
naive = new trans

-- Apply optimization

applyopt :: (MonadState Int m, MonadWriter Bool m, MonadState (Map.Map J.Exp Int) m, MonadState (Set.Set J.Exp) m) => ApplyOptTranslate m
applyopt = new (transApply $> trans)

-- Stack translation

--trStack1 :: (MonadState Int m, MonadWriter Bool m) => Mixin (TranslateStack m) (Translate m) (TranslateStack m) -- need to instantiate records
--trStack1 = transS

stackNaive :: (MonadState Int m, MonadReader Bool m, MonadState (Map.Map J.Exp Int) m, MonadState (Set.Set J.Exp) m) => TranslateStack m
stackNaive = new (transS $> trans)

-- Stack/Apply translation

adaptApply mix' this super = toT $ mix' this super

stackApply :: (MonadState Int m, MonadReader Bool m, MonadWriter Bool m, MonadState (Map.Map J.Exp Int) m, MonadState (Set.Set J.Exp) m) => TranslateStack m
stackApply = new ((transS <.> adaptApply transApply) $> trans)

instance (:<) (TranslateStack m) (ApplyOptTranslate m) where
  up = NT . toTS

{-
stackApply :: (MonadState Int m, MonadWriter Bool m, MonadState (Map.Map J.Exp Int) m) => TranslateStack m
stackApply = new ((transS <.> (adaptApply transApply)) $> trans)
-}

{-
trStack2 :: (MonadState Int m, MonadWriter Bool m) => Mixin (TranslateStack m) (ApplyOptTranslate m) (TranslateStack m) -- need to instantiate records
trStack2 = transS
-}

{-
-- apply() distinction
transMixA :: (MonadState Int m, MonadWriter Bool m, MonadState (Map.Map J.Exp Int) m, f :< Translate) => Open (ApplyOptTranslate f m)
transMixA this = NT (override (toT this) trans)

applyopt :: (MonadState Int m, MonadWriter Bool m, MonadState (Map.Map J.Exp Int) m, f :< Translate) => ApplyOptTranslate f m
applyopt = new (transApply . transMixA)

-- Stack-based translation

-- Adaptor mixin for trans

transMix :: (MonadState Int m, MonadWriter Bool m, MonadState (Map.Map J.Exp Int) m, f :< Translate) => Open (TranslateStack (ApplyOptTranslate f) m)
transMix this = TS (transMixA (toTS this)) (translateScheduleM this)

-- mixing in the new translation

stack :: (MonadState Int m, MonadWriter Bool m, MonadState (Map.Map J.Exp Int) m, f :< Translate) => TranslateStack (ApplyOptTranslate f) m
stack = new (transS . transMix)
-}

type M1 = StateT (Map.Map String Int) (State Int)
type M2 = StateT Int (State (Map.Map J.Exp Int))
type M3 = StateT Int (Writer Bool)

type MAOpt = StateT Int (StateT (Map.Map J.Exp Int) (StateT (Set.Set J.Exp) (Writer Bool)))

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
sf2java :: Compilation -> ClassName -> String -> Bool -> String
sf2java compilation (ClassName className) src stackTr =
    let (cu, _) = createCU className (compilation ((transESF . readESF) src)) stackTr in prettyPrint cu

compilesf2java :: Compilation -> FilePath -> FilePath -> Bool -> IO ()
compilesf2java compilation srcPath outputPath stackTr = do
    src <- readFile srcPath
    let output = sf2java compilation (ClassName (inferClassName outputPath)) src stackTr
    writeFile outputPath output

type Compilation = PFExp Int (Var, PCTyp Int) -> (J.Block, J.Exp, PCTyp Int)

-- setting
type AOptType = StateT Int (StateT (Map.Map J.Exp Int) (StateT (Set.Set J.Exp) (Writer Bool)))

aoptinst :: ApplyOptTranslate AOptType  -- instantiation; all coinstraints resolved
aoptinst = applyopt

translate ::  PCExp Int (Var, PCTyp Int) -> MAOpt ([J.BlockStmt], J.Exp, PCTyp Int)
translate = translateM (up sopt)

translateAO :: PCExp Int (Var, PCTyp Int) -> AOptType ([J.BlockStmt], J.Exp, PCTyp Int)
translateAO = translateM (up aoptinst)

compileAO :: Compilation
compileAO e =
  case fst $ runWriter $ evalStateT (evalStateT (evalStateT (translateAO (fexp2cexp e)) 0) Map.empty) Set.empty of
      (ss,exp,t) -> (J.Block ss,exp, t)

type NType = StateT Int (StateT (Map.Map J.Exp Int) (State (Set.Set J.Exp)))
ninst :: Translate NType  -- instantiation; all coinstraints resolved
ninst = naive

translateN ::  PCExp Int (Var, PCTyp Int) -> NType ([J.BlockStmt], J.Exp, PCTyp Int)
translateN = translateM (up ninst)

compileN :: Compilation
compileN e =
  case evalState (evalStateT (evalStateT (translateN (fexp2cexp e)) 0) Map.empty) Set.empty of
      (ss,exp,t) -> (J.Block ss,exp, t)

type StackType = ReaderT Bool (StateT Int (StateT (Map.Map J.Exp Int) (StateT (Set.Set J.Exp) (Writer Bool))))
stackinst :: TranslateStack StackType  -- instantiation; all coinstraints resolved
stackinst = stackNaive

translateS :: PCExp Int (Var, PCTyp Int) -> StackType ([J.BlockStmt], J.Exp, PCTyp Int)
translateS = translateM (up stackinst)

compileS :: Compilation
compileS e =
  case fst $ runWriter $ evalStateT (evalStateT (evalStateT (runReaderT (translateS (fexp2cexp e)) False) 0) Map.empty) Set.empty of
      (ss,exp,t) -> (J.Block ss,exp, t)

