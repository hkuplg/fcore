{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts
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
    , compileAONew
    , compileS
    , compileSAU
    , compileSN
    , compileBN
    , compileBS
    , compileUnbox
    , compileAoptUnbox
    , compileSU
    , compileBSAU
    , sf2java
    , compilesf2java
    , DumpOption(..)
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
import qualified ApplyTransCFJava2 as A2
import BenchGenCF2J
import BenchGenStack
import UnboxTransCFJava

import Inliner
import PartialEvaluator

import Inheritance
import MonadLib

import qualified Language.Java.Syntax as J
import Language.Java.Pretty

import Text.PrettyPrint.Leijen

import System.Exit (exitFailure)

import Data.Data

import qualified Data.Map as Map
import qualified Data.Set as Set

import Prelude hiding (exp)

-- import Control.Monad.Trans.Error (runErrorT)

-- import Debug.Trace      (trace)

data DumpOption
  = NoDump
  | DumpParsed
  | DumpTChecked
  | DumpCore
  | DumpSimpleCore
  | DumpClosureF
    deriving (Eq, Show, Data, Typeable)

-- Naive translation

naive :: (MonadState Int m) => Translate m
naive = new trans

-- Apply naive optimization

applyopt :: (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m) => ApplyOptTranslate m
applyopt = new (transApply $> trans)

applyoptNew :: (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m) => A2.ApplyOptTranslate m
applyoptNew = new (A2.transApply $> trans)

-- Stack naive optimization

stackNaive :: (MonadState Int m, MonadReader Bool m) => TranslateStack m
stackNaive = new (transS $> trans)

-- Unbox naive translation

unboxopt :: MonadState Int m => UnboxTranslate m
unboxopt = new (transUnbox $> trans)


-- Stack/Apply translation

adaptApply :: forall (m :: * -> *) t t1.
              (t -> t1 -> ApplyOptTranslate m) -> t -> t1 -> Translate m
adaptApply mix' this super = toT $ mix' this super

adaptStack :: forall (m :: * -> *) t t1.
              (t -> t1 -> TranslateStack m) -> t -> t1 -> Translate m
adaptStack mix' this super = toTS $ mix' this super

adaptUnbox :: forall (m :: * -> *) t t1. (t -> t1 -> UnboxTranslate m) -> t -> t1 -> Translate m
adaptUnbox mix' this super = toUT $ mix' this super

-- Stack + Apply + Naive

stackApply :: (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m, MonadReader Bool m) => TranslateStack m
stackApply = new ((transS <.> adaptApply transApply) $> trans)


stackApplyNew :: (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m, MonadReader Bool m) => ApplyOptTranslate m
stackApplyNew = new ((transApply <.> adaptStack transSA) $> trans)

-- Apply + Unbox + Naive
applyUnbox :: (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m) => ApplyOptTranslate m
applyUnbox = new ((transApply <.> adaptUnbox transUnbox) $> trans)

-- Stack + Unbox + Naive
stackUnbox :: (MonadState Int m, MonadReader Bool m) => TranslateStack m
stackUnbox = new ((transSU <.> adaptUnbox transUnbox) $> trans)

-- Stack + Apply + Unbox + Naive
stackApplyUnbox :: (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m, MonadReader Bool m) => ApplyOptTranslate m
stackApplyUnbox = new ((transApply <.> adaptStack transSAU <.> adaptUnbox transUnbox) $> trans)

instance (:<) (TranslateStack m) (ApplyOptTranslate m) where
  up = NT . toTS

instance (:<)  (ApplyOptTranslate m) (TranslateStack m) where
  up = TS . toT

instance (:<) (UnboxTranslate m) (TranslateStack m) where
  up = TS . toUT

instance (:<) (TranslateStack m) (UnboxTranslate m) where
  up = UT . toTS

instance (:<) (UnboxTranslate m) (ApplyOptTranslate m) where
  up = NT . toUT

instance (:<) (ApplyOptTranslate m) (UnboxTranslate m) where
  up = UT . toT


-- Benchmark-link generation

-- bench for naive
benchGen :: (MonadState Int m) => BenchGenTranslate m
benchGen = new (transBench $> trans)



-- bench for naive + applyopt
inheritNOpt :: (t -> t1 -> ApplyOptTranslate m) -> t -> t1 -> Translate m
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
benchGenStackOpt = new ((transBenchStackOpt <.> adaptApply transApply <.> adaptStack transSA) $> trans)

-- bench for stack + apply + unbox opt
benchGenStackOptUnbox :: (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m, MonadReader Bool m) => BenchGenTranslateStackOpt m
benchGenStackOptUnbox = new ((transBenchStackOpt <.> adaptApply transApply <.> adaptStack transSAU <.> adaptUnbox transUnbox) $> trans)


instance (:<) (BenchGenTranslateStackOpt m) (ApplyOptTranslate m) where
  up = NT . toTBSA

instance (:<) (BenchGenTranslateStackOpt m) (TranslateStack m) where
  up = TS . toTBSA

instance (:<) (BenchGenTranslateStackOpt m) (UnboxTranslate m) where
  up = UT . toTBSA

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
-- TODO: ugly hack to integrate number of inlings
sf2java :: Int -> DumpOption -> Compilation -> ClassName -> String -> IO String
sf2java num optDump compilation className src =
  do let readSrc = Parser.reader src
     when (optDump == DumpParsed) $ print readSrc
     result <- readSrc `seq` (typeCheck readSrc)
     case result of
       Left typeError ->
         do print (Text.PrettyPrint.Leijen.pretty typeError)
            exitFailure -- TODO: Ugly
       Right (tcheckedSrc, _t)   ->
         do when (optDump == DumpTChecked) $ print tcheckedSrc
            let core = desugar tcheckedSrc
            when (optDump == DumpCore) $ print (Core.prettyExpr core)
            let simpleCore = case num of
                               1 -> peval . inliner . simplify $ core
                               2 -> peval . inliner . inliner . simplify $ core
                               _ -> peval {-. simplify-} $ core
            -- let simpleCore = simplify core
            when (optDump == DumpSimpleCore) $ print (Core.prettyExpr simpleCore)
	    when (optDump == DumpClosureF ) $ print (ClosureF.prettyExpr basePrec (0,0) (fexp2cexp simpleCore))
            let (cu, _) = compilation className simpleCore
            return $ prettyPrint cu

compilesf2java :: Int -> DumpOption -> Compilation -> FilePath -> FilePath -> IO ()
compilesf2java num optDump compilation srcPath outputPath = do
    src <- readFile srcPath
    output <- sf2java num optDump compilation (inferClassName outputPath) src
    writeFile outputPath output
    --let closureClassDef = closureClass compilation
    --writeFile "Closure.java" (prettyPrint closureClassDef)

--abstractClosureClass compilation = prettyPrint $ closureClass (up benchinst)

type Compilation = String -> Core.Expr Int (Var, Type Int) -> (J.CompilationUnit, Type Int)--PFExp Int (Var, Type Int) -> (J.Block, J.Exp, Type Int)

-- | setting for various combination of optimization

-- Setting for naive
type NType = State Int
ninst :: Translate NType  -- instantiation; all coinstraints resolved
ninst = naive

translateN :: String -> Expr Int (Var, Type Int) -> NType (J.CompilationUnit, Type Int)
translateN = createWrap (up ninst)

compileN :: Compilation
compileN name e = evalState (translateN name (fexp2cexp e)) 0

-- Setting for apply + naive
type AOptType = StateT Int (StateT (Set.Set J.Exp) (Reader InitVars))

aoptinst :: ApplyOptTranslate AOptType  -- instantiation; all coinstraints resolved
aoptinst = applyopt

aoptinstNew :: A2.ApplyOptTranslate AOptType  -- instantiation; all coinstraints resolved
aoptinstNew = applyoptNew

-- translate :: String -> Expr Int (Var, Type Int) -> MAOpt (J.CompilationUnit, Type Int)
-- translate = createWrap (up sopt)

translateAO :: String -> Expr Int (Var, Type Int) -> AOptType (J.CompilationUnit, Type Int)
translateAO = createWrap (up aoptinst)

compileAO :: Compilation
compileAO name e = runReader (evalStateT (evalStateT (translateAO name (fexp2cexp e)) 0) Set.empty) []

translateAONew :: String -> Expr Int (Var, Type Int) -> AOptType (J.CompilationUnit, Type Int)
translateAONew = createWrap (up aoptinstNew)

compileAONew :: Compilation
compileAONew name e = runReader (evalStateT (evalStateT (translateAONew name (fexp2cexp e)) 0) Set.empty) []


-- Setting for stack + naive
type StackNaiveType = ReaderT Bool (State Int)

stackNaiveinst :: TranslateStack StackNaiveType  -- instantiation; all coinstraints resolved
stackNaiveinst = stackNaive

translateSN :: String -> Expr Int (Var, Type Int) -> StackNaiveType (J.CompilationUnit, Type Int)
translateSN = createWrap (up stackNaiveinst)

compileSN :: Compilation
compileSN name e = evalState (runReaderT (translateSN name (fexp2cexp e)) True) 0

-- Setting for unbox + naive
unboxinst :: UnboxTranslate NType  -- instantiation; all coinstraints resolved
unboxinst = unboxopt

translateUnbox :: String -> Expr Int (Var, Type Int) -> NType (J.CompilationUnit, Type Int)
translateUnbox = createWrap (up unboxinst)

compileUnbox :: Compilation
compileUnbox name e = evalState (translateUnbox name (fexp2cexp e)) 0

-- Setting for apply + unbox + naive

type AOptUnboxType = StateT Int (StateT (Set.Set J.Exp) (Reader InitVars))

aoptUnboxInst :: ApplyOptTranslate AOptUnboxType
aoptUnboxInst = applyUnbox

translateAU :: String -> Expr Int (Var, Type Int) -> AOptUnboxType (J.CompilationUnit, Type Int)
translateAU = createWrap (up aoptUnboxInst)

compileAoptUnbox :: Compilation
compileAoptUnbox name e = runReader (evalStateT (evalStateT (translateAU name (fexp2cexp e)) 0) Set.empty) []

-- Setting for stack + unbox + naive

type StackUnboxType = ReaderT Bool (State Int)

stackUnboxInst :: TranslateStack StackUnboxType
stackUnboxInst = stackUnbox

translateSU :: String -> Expr Int (Var, Type Int) -> StackUnboxType (J.CompilationUnit, Type Int)
translateSU = createWrap (up stackUnboxInst)

compileSU :: Compilation
compileSU name e =  evalState (runReaderT (translateSU name (fexp2cexp e)) True) 0

-- Setting for apply + stack + naive
type StackType = ReaderT Bool (ReaderT InitVars (StateT (Set.Set J.Exp) (State Int)))

-- stackinstOld :: TranslateStack StackType  -- instantiation; all coinstraints resolved
-- stackinstOld = stackApply --stackNaive

stackinst :: ApplyOptTranslate StackType  -- instantiation; all coinstraints resolved
stackinst = stackApplyNew --stackNaive

translateS :: String -> Expr Int (Var, Type Int) -> StackType (J.CompilationUnit, Type Int)
translateS = createWrap (up stackinst)

compileS :: Compilation
compileS name e = evalState (evalStateT (runReaderT (runReaderT (translateS name (fexp2cexp e)) True) []) Set.empty) 0

-- Setting for apply + stack + unbox + naive
stackau :: ApplyOptTranslate StackType
stackau = stackApplyUnbox

translateSAU :: String -> Expr Int (Var, Type Int) -> StackType (J.CompilationUnit, Type Int)
translateSAU = createWrap (up stackau)

compileSAU :: Compilation
compileSAU name e = evalState (evalStateT (runReaderT (runReaderT (translateSAU name (fexp2cexp e)) True) []) Set.empty) 0

-- | Setting for benchmark

-- Bench Naive
benchinst :: BenchGenTranslate NType  -- instantiation; all coinstraints resolved
benchinst = benchGen

translateBench :: String -> Expr Int (Var, Type Int) -> NType (J.CompilationUnit, Type Int)
translateBench = createWrap (up benchinst)

compileBN :: Bool -> Compilation
compileBN False = \name e -> evalState (translateBench name (fexp2cexp e)) 0--evalState (evalStateT (evalStateT (translateBench name (fexp2cexp e)) 0) Map.empty) Set.empty
compileBN True = \name e -> evalState (translateBench name (fexp2cexp e)) 0--evalState (evalStateT (evalStateT (translateBench name (fexp2cexp e)) 0) Map.empty) Set.empty


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

-- Bench stack + apply
benchstackinst :: BenchGenTranslateStack StackType  -- instantiation; all coinstraints resolved
benchstackinst = benchGenStack -- stack naive

translateBenchStack :: String -> Expr Int (Var, Type Int) -> StackType (J.CompilationUnit, Type Int)
translateBenchStack = createWrap (up benchstackinst)


-- Bench stack + applyopt
-- translateBSA
benchstackoptinst :: BenchGenTranslateStackOpt StackType
benchstackoptinst = benchGenStackOpt

translateBenchStackOpt :: String -> Expr Int (Var, Type Int) -> StackType (J.CompilationUnit, Type Int)
translateBenchStackOpt = createWrap (up benchstackoptinst)


compileBS :: Bool -> Compilation
compileBS False = \name e -> evalState (evalStateT (runReaderT (runReaderT (translateBenchStack name (fexp2cexp e)) True) []) Set.empty) 0
compileBS True = \name e -> evalState (evalStateT (runReaderT (runReaderT (translateBenchStackOpt name (fexp2cexp e)) True) []) Set.empty) 0

-- Bench stack + apply + unbox

benchstackoptunboxinst :: BenchGenTranslateStackOpt StackType
benchstackoptunboxinst = benchGenStackOptUnbox

translateBenchStackOptUnbox :: String -> Expr Int (Var, Type Int) -> StackType (J.CompilationUnit, Type Int)
translateBenchStackOptUnbox = createWrap (up benchstackoptunboxinst)

compileBSAU :: Compilation
compileBSAU = \name e -> evalState (evalStateT (runReaderT (runReaderT (translateBenchStackOptUnbox name (fexp2cexp e)) True) []) Set.empty) 0
