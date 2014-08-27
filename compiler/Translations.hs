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

import ESF.Parser      (reader)
import ESF.TypeCheck   (infer)
import Desugar         (desugar)
import DeInter         (deInter)

import qualified SystemF.Syntax as F
import qualified ClosureF       as C

import JavaUtils      (ClassName(..), inferClassName)

import BaseTransCFJava
import ApplyTransCFJava
import StackTransCFJava

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

-- Stack/Apply translation

adaptApply mix' this super = toT $ mix' this super

adaptStack mix' this super = toTS $ mix' this super

-- Stack + Apply + Naive

stackApply :: (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m, MonadReader Bool m) => TranslateStack m
stackApply = new ((transS <.> adaptApply transApply) $> trans)

-- Apply + Stack + Naive

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

translate ::  C.Expr Int (Var, C.Type Int) -> MAOpt ([BlockStmt], Exp, C.Type Int)
translate e = translateM (up sopt) e

compile ::  PFExp Int (Var, C.Type Int) -> (Block, Exp, C.Type Int)
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

-- compilePretty :: PFExp Int (Var, C.Type Int) -> IO ()
-- compilePretty e = let (b,e1,t) = compile e in (prettyJ b >> prettyJ e1 >> print t)

-- compileCU :: String -> PFExp Int (Var, C.Type Int) -> Maybe String -> IO ()
-- compileCU className e (Just nameStr) = let (cu,t) = createCU className (compile e) (Just nameStr) in (prettyJ cu >> print t)
-- compileCU className e Nothing = let (cu,t) = createCU className (compile e) Nothing in (prettyJ cu >> print t)

-- SystemF to Java
sf2java :: Compilation -> ClassName -> String -> IO String
sf2java compilation (ClassName className) src =
  do let expr = ESF.Parser.reader src
     result <- runErrorT $ ESF.TypeCheck.infer expr
     case result of
       Left typeError       -> error $ show (Text.PrettyPrint.Leijen.pretty typeError)
       Right (tcExpr, _t)   ->
         do let sf = (deInter . desugar) tcExpr
            let (cu, _) = compilation className sf
            return $ prettyPrint cu

compilesf2java :: Compilation -> FilePath -> FilePath -> IO ()
compilesf2java compilation srcPath outputPath = do
    src <- readFile srcPath
    output <- sf2java compilation (ClassName (inferClassName outputPath)) src
    writeFile outputPath output

type Compilation = String -> F.Expr Int (Var, C.Type Int) -> (J.CompilationUnit, C.Type Int)--PFExp Int (Var, C.Type Int) -> (J.Block, J.Exp, C.Type Int)

-- setting
type AOptType = StateT Int (StateT (Set.Set J.Exp) (Reader InitVars))

aoptinst :: ApplyOptTranslate AOptType  -- instantiation; all coinstraints resolved
aoptinst = applyopt

translate :: String -> C.Expr Int (Var, C.Type Int) -> MAOpt (J.CompilationUnit, C.Type Int)
translate = createWrap (up sopt)

translateAO :: String -> C.Expr Int (Var, C.Type Int) -> AOptType (J.CompilationUnit, C.Type Int)
translateAO = createWrap (up aoptinst)

compileAO :: Compilation
compileAO name e = runReader (evalStateT ((evalStateT (translateAO name (C.fexp2cexp e)) 0)) Set.empty) []

type NType = State Int
ninst :: Translate NType  -- instantiation; all coinstraints resolved
ninst = naive

translateN :: String -> C.Expr Int (Var, C.Type Int) -> NType (J.CompilationUnit, C.Type Int)
translateN = createWrap (up ninst)

compileN :: Compilation
compileN name e = evalState (translateN name (C.fexp2cexp e)) 0

type StackType = ReaderT Bool (ReaderT InitVars (StateT (Set.Set J.Exp) (State Int)))

stackinstOld :: TranslateStack StackType  -- instantiation; all coinstraints resolved
stackinstOld = stackApply --stackNaive

stackNaiveinst :: TranslateStack StackType  -- instantiation; all coinstraints resolved
stackNaiveinst = stackNaive

stackinst :: ApplyOptTranslate StackType  -- instantiation; all coinstraints resolved
stackinst = stackApplyNew --stackNaive

translateS :: String -> C.Expr Int (Var, C.Type Int) -> StackType (J.CompilationUnit, C.Type Int)
translateS = createWrap (up stackinst)

compileS :: Compilation
compileS name e = evalState (evalStateT (runReaderT (runReaderT (translateS name (C.fexp2cexp e)) False) []) Set.empty) 0
