{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , KindSignatures
           , MultiParamTypeClasses
           , OverlappingInstances
           , RankNTypes
           , TypeOperators
           #-}

{-# OPTIONS_GHC -fno-warn-unused-binds -fwarn-incomplete-patterns #-}

module Translations
    ( Compilation
    , compileN
    , compileAO
    , compileS
    , compileBN
    , compileBS
    , compileUnbox
    , sf2java
    , compilesf2java
    ) where

import Parser    (reader)
import TypeCheck (typeCheck)
import Desugar   (desugar)
import Simplify  (simplify)

import qualified Core
import ClosureF

import PrettyUtils
import JavaUtils      (ClassName, inferClassName)

import BaseTransCFJava
import ApplyTransCFJava
import StackTransCFJava
import BenchGenCF2J
import BenchGenStack
import UnboxTransCFJava

import Inheritance
import MonadLib

import qualified Language.Java.Syntax as J
import Language.Java.Pretty

import Text.PrettyPrint.Leijen

import qualified Data.Map as Map
import qualified Data.Set as Set

import Prelude hiding (exp)

import Control.Monad.Trans.Error (runErrorT)

-- import Debug.Trace      (trace)

-- Naive translation

naive :: (MonadState Int m) => Translate m
naive = new trans

-- Apply optimization

applyopt :: (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m) => ApplyOptTranslate m
applyopt = new (transApply $> trans)

-- Stack translation

--trStack1 :: (MonadState Int m, MonadWriter Bool m) => Mixin (TranslateStack m) (Translate m) (TranslateStack m) -- need to instantiate records
--trStack1 = transS

stackNaive :: (MonadState Int m, MonadReader Bool m) => TranslateStack m
stackNaive = new (transS $> trans)


-- Benchmark-link generation

-- bench for naive
benchGen :: (MonadState Int m) => BenchGenTranslate m
benchGen = new (transBench $> trans)



-- bench for naive + applyopt
inheritNOpt mix' this super = toT $ mix' this super

benchGenNOpt :: (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m) => BenchGenTranslateOpt m
benchGenNOpt = new ((transBenchOpt <.> inheritNOpt transApply) $> trans)

instance (:<) (BenchGenTranslateOpt m) (ApplyOptTranslate m) where
  up = NT . toTBA


-- bench for stack

benchGenStack :: (MonadState Int m, MonadReader Bool m) => BenchGenTranslateStack m
benchGenStack = new ((transBenchStack <.> adaptStack transS) $> trans)

instance (:<) (BenchGenTranslateStack m) (TranslateStack m) where
  up = TS . toTBS

-- bench for stack + apply opt
--benchGenStackOpt ::  (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m, MonadReader Bool m) => TranslateStack m


benchGenStackOpt :: (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m, MonadReader Bool m) => BenchGenTranslateStackOpt m
benchGenStackOpt = new ((transBenchStackOpt <.> (adaptApply transApply <.> adaptStack transSA)) $> trans)

instance (:<) (BenchGenTranslateStackOpt m) (ApplyOptTranslate m) where
  up = NT . toTBSA

instance (:<) (BenchGenTranslateStackOpt m) (TranslateStack m) where
  up = TS . toTBSA


-- Stack/Apply translation

adaptApply :: forall (m :: * -> *) t t1.
              (t -> t1 -> ApplyOptTranslate m) -> t -> t1 -> Translate m
adaptApply mix' this super = toT $ mix' this super

adaptStack :: forall (m :: * -> *) t t1.
              (t -> t1 -> TranslateStack m) -> t -> t1 -> Translate m
adaptStack mix' this super = toTS $ mix' this super

-- Stack + Apply + Naive

stackApply :: (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m, MonadReader Bool m) => TranslateStack m
stackApply = new ((transS <.> adaptApply transApply) $> trans)


stackApplyNew :: (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m, MonadReader Bool m) => ApplyOptTranslate m
stackApplyNew = new ((transApply <.> adaptStack transSA) $> trans)


instance (:<) (TranslateStack m) (ApplyOptTranslate m) where
  up = NT . toTS

instance (:<)  (ApplyOptTranslate m) (TranslateStack m) where
  up = TS . toT

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
type M2 = State Int
type M3 = State Int

type MAOpt = State Int

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

translate ::  Expr Int (Var, Type Int) -> MAOpt ([BlockStmt], Exp, Type Int)
translate e = translateM (up sopt) e

compile ::  PFExp Int (Var, Type Int) -> (Block, Exp, Type Int)
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

prettyJ :: Language.Java.Pretty.Pretty a => a -> IO ()
prettyJ = putStrLn . prettyPrint

-- compilePretty :: PFExp Int (Var, Type Int) -> IO ()
-- compilePretty e = let (b,e1,t) = compile e in (prettyJ b >> prettyJ e1 >> print t)

-- compileCU :: String -> PFExp Int (Var, Type Int) -> Maybe String -> IO ()
-- compileCU className e (Just nameStr) = let (cu,t) = createCU className (compile e) (Just nameStr) in (prettyJ cu >> print t)
-- compileCU className e Nothing = let (cu,t) = createCU className (compile e) Nothing in (prettyJ cu >> print t)

-- SystemF to Java
sf2java :: Bool -> Compilation -> ClassName -> String -> IO String
sf2java optDump compilation className src =
  do let readSrc = Parser.reader src
     result <- typeCheck readSrc
     case result of
       Left typeError -> error $ show ({- Text.PrettyPrint.Leijen.pretty-} typeError)
       Right (tcheckedSrc, _t)   ->
         do let core = desugar tcheckedSrc
            when optDump $ do { putStrLn "Core"; print $ Core.pprExpr basePrec (0,0) core }
            let simpleCore = simplify core
            when optDump $ do { putStrLn "Simplified Core"; print $ Core.pprExpr basePrec (0,0) simpleCore }
            let (cu, _) = compilation className simpleCore
            return $ prettyPrint cu

compilesf2java :: Bool -> Compilation -> FilePath -> FilePath -> IO ()
compilesf2java optDump compilation srcPath outputPath = do
    src <- readFile srcPath
    output <- sf2java optDump compilation (inferClassName outputPath) src
    writeFile outputPath output
    --let closureClassDef = closureClass compilation
    --writeFile "Closure.java" (prettyPrint closureClassDef)

--abstractClosureClass compilation = prettyPrint $ closureClass (up benchinst)


type Compilation = String -> Core.Expr Int (Var, Type Int) -> (J.CompilationUnit, Type Int)--PFExp Int (Var, Type Int) -> (J.Block, J.Exp, Type Int)

-- setting
type AOptType = StateT Int (StateT (Set.Set J.Exp) (Reader InitVars))

aoptinst :: ApplyOptTranslate AOptType  -- instantiation; all coinstraints resolved
aoptinst = applyopt

translate :: String -> Expr Int (Var, Type Int) -> MAOpt (J.CompilationUnit, Type Int)
translate = createWrap (up sopt)

translateAO :: String -> Expr Int (Var, Type Int) -> AOptType (J.CompilationUnit, Type Int)
translateAO = createWrap (up aoptinst)

compileAO :: Compilation
compileAO name e = runReader (evalStateT (evalStateT (translateAO name (fexp2cexp e)) 0) Set.empty) []

type NType = State Int
ninst :: Translate NType  -- instantiation; all coinstraints resolved
ninst = naive

translateN :: String -> Expr Int (Var, Type Int) -> NType (J.CompilationUnit, Type Int)
translateN = createWrap (up ninst)

compileN :: Compilation
compileN name e = evalState (translateN name (fexp2cexp e)) 0

type StackType = ReaderT Bool (ReaderT InitVars (StateT (Set.Set J.Exp) (State Int)))

stackinstOld :: TranslateStack StackType  -- instantiation; all coinstraints resolved
stackinstOld = stackApply --stackNaive

stackNaiveinst :: TranslateStack StackType  -- instantiation; all coinstraints resolved
stackNaiveinst = stackNaive

stackinst :: ApplyOptTranslate StackType  -- instantiation; all coinstraints resolved
stackinst = stackApplyNew --stackNaive

translateS :: String -> Expr Int (Var, Type Int) -> StackType (J.CompilationUnit, Type Int)
translateS = createWrap (up stackinst)

compileS :: Compilation
compileS name e = evalState (evalStateT (runReaderT (runReaderT (translateS name (fexp2cexp e)) False) []) Set.empty) 0


-- Unbox Naive

unboxopt :: MonadState Int m => UnboxTranslate m
unboxopt = new (transUnbox $> trans)


unboxinst :: UnboxTranslate NType  -- instantiation; all coinstraints resolved
unboxinst = unboxopt

translateUnbox :: String -> Expr Int (Var, Type Int) -> NType (J.CompilationUnit, Type Int)
translateUnbox = createWrap (up unboxinst)

compileUnbox :: Compilation
compileUnbox name e = evalState (translateUnbox name (fexp2cexp e)) 0

-- Bench Naive
benchinst :: BenchGenTranslate NType  -- instantiation; all coinstraints resolved
benchinst = benchGen

translateBench :: String -> Expr Int (Var, Type Int) -> NType (J.CompilationUnit, Type Int)
translateBench = createWrap (up benchinst)


--closureBench :: NType (J.TypeDecl)
--closureBench = closureClass (up benchinst)

--compileClosureBench = evalState (evalStateT (evalStateT closureBench 0) Map.empty) Set.empty--evalState (evalStateT (evalStateT (translateBench name (fexp2cexp e)) 0) Map.empty) Set.empty


--genClosure :: J.CompilationUnit
--genClosure = (J.CompilationUnit (Just (J.PackageDecl (J.Name [(J.Ident "benchmark")])))
--                                                [] [(closureClass)])

-- Bench naive+ applyopt
benchnaiveopt :: BenchGenTranslateOpt AOptType
benchnaiveopt = benchGenNOpt

translateBenchOpt :: String -> Expr Int (Var, Type Int) -> AOptType (J.CompilationUnit, Type Int)
translateBenchOpt = createWrap (up benchnaiveopt)

-- Bench stack
benchstackinst :: BenchGenTranslateStack StackType  -- instantiation; all coinstraints resolved
benchstackinst = benchGenStack -- stack naive

translateBenchStack :: String -> Expr Int (Var, Type Int) -> StackType (J.CompilationUnit, Type Int)
translateBenchStack = createWrap (up benchstackinst)



-- TODO Bench stack + applyopt
-- translateBSA
benchstackoptinst :: BenchGenTranslateStackOpt StackType
benchstackoptinst = benchGenStackOpt

translateBenchStackOpt :: String -> Expr Int (Var, Type Int) -> StackType (J.CompilationUnit, Type Int)
translateBenchStackOpt = createWrap (up benchstackoptinst)

compileBN :: Bool -> Compilation
compileBN False = \name e -> evalState (translateBench name (fexp2cexp e)) 0--evalState (evalStateT (evalStateT (translateBench name (fexp2cexp e)) 0) Map.empty) Set.empty
compileBN True = \name e -> evalState (translateBench name (fexp2cexp e)) 0--evalState (evalStateT (evalStateT (translateBench name (fexp2cexp e)) 0) Map.empty) Set.empty


-- BenchGenStack

compileBS :: Bool -> Compilation
compileBS False = \name e -> evalState (evalStateT (runReaderT (runReaderT (translateBenchStack name (fexp2cexp e)) False) []) Set.empty) 0
compileBS True = \name e -> evalState (evalStateT (runReaderT (runReaderT (translateBenchStackOpt name (fexp2cexp e)) False) []) Set.empty) 0
