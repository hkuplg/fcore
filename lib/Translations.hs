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
    , compileS
    -- , compileSAU
    , compileSN
    -- , compileUnbox
    -- , compileAoptUnbox
    -- , compileSU
    , sf2java
    -- , sf2java2
    , compilesf2java
    , DumpOption(..)
    ) where

import           ApplyTransCFJava
import           BaseTransCFJava
import           BenchGenCF2J
import           BenchGenStack
import           ClosureF
import qualified Core
import qualified SystemFI as FI
import           Desugar (desugar)
import           Inheritance
import           Inliner
import           JavaUtils (ClassName, inferClassName)
import           MonadLib
import           Parser
import           PartialEvaluator
import           PrettyUtils
import           Simplify (simplify)
import           StackTransCFJava
import           TypeCheck (typeCheck)
-- import           UnboxTransCFJava

import           Data.Data
import           Language.Java.Pretty
import qualified Language.Java.Syntax as J
import           Prelude hiding (exp)
import           System.Exit (exitFailure)
import           Text.PrettyPrint.ANSI.Leijen

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

naive :: MonadState Int m => Translate m
naive = new trans

-- Apply naive optimization

applyopt :: (MonadState Int m, MonadReader Int m, MonadReader InitVars m) => ApplyOptTranslate m
applyopt = new (transApply $> trans)

-- Stack naive optimization

stackNaive :: (MonadState Int m, MonadReader Bool m) => TranslateStack m
stackNaive = new (transS $> trans)

-- Unbox naive translation

-- unboxopt :: (MonadState Int m, MonadReader (Int, Bool) m) => UnboxTranslate m
-- unboxopt = new (transUnbox $> trans)


-- Stack/Apply/Unbox translation

adaptApply :: forall (m :: * -> *) t t1.
              (t -> t1 -> ApplyOptTranslate m) -> t -> t1 -> Translate m
adaptApply mix' this super = toT $ mix' this super

adaptStack :: forall (m :: * -> *) t t1.
              (t -> t1 -> TranslateStack m) -> t -> t1 -> Translate m
adaptStack mix' this super = toTS $ mix' this super

-- adaptUnbox :: forall (m :: * -> *) t t1.
--               (t -> t1 -> UnboxTranslate m) -> t -> t1 -> Translate m
-- adaptUnbox mix' this super = toUT $ mix' this super

-- Stack + Apply + Naive

stackApplyNew :: (MonadState Int m, MonadReader Int m, MonadReader Bool m, MonadReader InitVars m) => ApplyOptTranslate m
stackApplyNew = new ((transAS <.> adaptStack transSA) $> trans)

-- Apply + Unbox + Naive
-- applyUnbox :: (MonadState Int m, MonadReader (Int, Bool) m, MonadReader InitVars m) => ApplyOptTranslate m
-- applyUnbox = new ((transApply <.> adaptUnbox transUnbox) $> trans)

-- Stack + Unbox + Naive
-- stackUnbox :: (MonadState Int m, MonadReader (Int, Bool) m) => TranslateStack m
-- stackUnbox = new ((transSU <.> adaptUnbox transUnbox) $> trans)

-- Stack + Apply + Unbox + Naive
-- stackApplyUnbox :: (MonadState Int m, MonadReader (Int, Bool) m, MonadReader InitVars m) => ApplyOptTranslate m
-- stackApplyUnbox = new ((transAS <.> adaptStack transSAU <.> adaptUnbox transUnbox) $> trans)

instance (:<) (TranslateStack m) (ApplyOptTranslate m) where
  up = NT . toTS

instance (:<)  (ApplyOptTranslate m) (TranslateStack m) where
  up = TS . toT

-- instance (:<) (UnboxTranslate m) (TranslateStack m) where
--   up = TS . toUT

-- instance (:<) (TranslateStack m) (UnboxTranslate m) where
--   up = UT . toTS

-- instance (:<) (UnboxTranslate m) (ApplyOptTranslate m) where
--   up = NT . toUT

-- instance (:<) (ApplyOptTranslate m) (UnboxTranslate m) where
--   up = UT . toT

instance (:<) (BenchGenTranslateOpt m) (ApplyOptTranslate m) where
  up = NT . toTBA

instance (:<) (BenchGenTranslateStack m) (TranslateStack m) where
  up = TS . toTBS

instance (:<) (BenchGenTranslateStackOpt m) (ApplyOptTranslate m) where
  up = NT . toTBSA

instance (:<) (BenchGenTranslateStackOpt m) (TranslateStack m) where
  up = TS . toTBSA

-- instance (:<) (BenchGenTranslateStackOpt m) (UnboxTranslate m) where
--   up = UT . toTBSA

-- prettyJ :: Language.Java.Pretty.Pretty a => a -> IO ()
-- prettyJ = putStrLn . prettyPrint

-- SystemF to Java
sf2java :: Bool -> DumpOption -> Compilation -> ClassName -> String -> IO String
sf2java optInline optDump compilation className src =
  do case Parser.reader src of
       POk readSrc -> do
         when (optDump == DumpParsed) $ print readSrc
         result <- readSrc `seq` typeCheck readSrc
         case result of
           Left typeError ->
             do print (Text.PrettyPrint.ANSI.Leijen.pretty typeError)
                exitFailure -- TODO: Ugly
           Right (_, tcheckedSrc)   ->
             do when (optDump == DumpTChecked) $ print tcheckedSrc
                let core = desugar tcheckedSrc
                when (optDump == DumpCore) $ print (FI.prettyExpr core)
                let simpleCore = simplify (FI.HideF core)
                let rewrittencore = rewriteAndEval (Hide simpleCore)
                let recurNumOfCore = if optInline then recurNum rewrittencore else 0 -- inline
                let inlineNum = if recurNumOfCore > 2 then 0 else recurNumOfCore
                let inlinedCore = case inlineNum of
                                    1 -> inliner rewrittencore
                                    2 -> inliner . inliner $ rewrittencore
                                    _ -> rewrittencore
                when (optDump == DumpSimpleCore) $ print (Core.prettyExpr rewrittencore)
                when (optDump == DumpClosureF ) $ print (ClosureF.prettyExpr basePrec (0,0) (fexp2cexp inlinedCore))
                let (cu, _) = compilation className inlinedCore
                return $ prettyPrint cu
       PError msg -> do print msg
                        exitFailure

compilesf2java :: Bool -> DumpOption -> Compilation -> FilePath -> FilePath -> IO ()
compilesf2java optInline optDump compilation srcPath outputPath = do
    src <- readFile srcPath
    output <- sf2java optInline optDump compilation (inferClassName outputPath) src
    writeFile outputPath output
    --let closureClassDef = closureClass compilation
    --writeFile "Closure.java" (prettyPrint closureClassDef)


type Compilation = String -> Core.Expr Int (Var, Type Int) -> (J.CompilationUnit, Type Int)--PFExp Int (Var, Type Int) -> (J.Block, J.Exp, Type Int)

-- | setting for various combination of optimization

-- Setting for naive
type NType = State Int
ninst :: Translate NType  -- instantiation; all coinstraints resolved
ninst = naive

translateN :: String -> Expr Int (Var, Type Int) -> NType (J.CompilationUnit, Type Int)
translateN = createWrap (up ninst)

compileN :: Compilation
compileN name e = evalState (translateN name (fexp2cexp e)) 1

-- Setting for apply + naive
type AOptType = ReaderT Int (ReaderT InitVars (State Int))

aoptinst :: ApplyOptTranslate AOptType  -- instantiation; all coinstraints resolved
aoptinst = applyopt

translateAO :: String -> Expr Int (Var, Type Int) -> AOptType (J.CompilationUnit, Type Int)
translateAO = createWrap (up aoptinst)

compileAO :: Compilation
compileAO name e = evalState (runReaderT (runReaderT (translateAO name (fexp2cexp e)) 0) []) 1

-- Setting for stack + naive
type StackNaiveType = ReaderT Bool (State Int)

stackNaiveinst :: TranslateStack StackNaiveType  -- instantiation; all coinstraints resolved
stackNaiveinst = stackNaive

translateSN :: String -> Expr Int (Var, Type Int) -> StackNaiveType (J.CompilationUnit, Type Int)
translateSN = createWrap (up stackNaiveinst)

compileSN :: Compilation
compileSN name e = evalState (runReaderT (translateSN name (fexp2cexp e)) True) 1

-- Setting for unbox + naive
-- unboxinst :: UnboxTranslate NType  -- instantiation; all coinstraints resolved
-- unboxinst = unboxopt

-- translateUnbox :: String -> Expr Int (Var, Type Int) -> NType (J.CompilationUnit, Type Int)
-- translateUnbox = createWrap (up unboxinst)

-- compileUnbox :: Compilation
-- compileUnbox name e = evalState (runReaderT (translateUnbox name (fexp2cexp e)) (0, True)) 1

-- Setting for apply + unbox + naive
-- aoptUnboxInst :: ApplyOptTranslate AOptType
-- aoptUnboxInst = applyUnbox

-- translateAU :: String -> Expr Int (Var, Type Int) -> AOptType (J.CompilationUnit, Type Int)
-- translateAU = createWrap (up aoptUnboxInst)

-- compileAoptUnbox :: Compilation
-- compileAoptUnbox name e = evalState (runReaderT (runReaderT (translateAU name (fexp2cexp e)) (0, True)) []) 1

-- Setting for stack + unbox + naive
-- stackUnboxInst :: TranslateStack NType
-- stackUnboxInst = stackUnbox

-- translateSU :: String -> Expr Int (Var, Type Int) -> NType (J.CompilationUnit, Type Int)
-- translateSU = createWrap (up stackUnboxInst)

-- compileSU :: Compilation
-- compileSU name e =  evalState (runReaderT (translateSU name (fexp2cexp e)) (0, True)) 1

type ApplyStackTranslate = ReaderT Int (ReaderT Bool (ReaderT InitVars (State Int)))

-- Setting for apply + stack + naive
stackinst :: ApplyOptTranslate ApplyStackTranslate  -- instantiation; all coinstraints resolved
stackinst = stackApplyNew

translateS :: String -> Expr Int (Var, Type Int) -> ApplyStackTranslate (J.CompilationUnit, Type Int)
translateS = createWrap (up stackinst)

compileS :: Compilation
compileS name e = evalState (runReaderT (runReaderT (runReaderT (translateS name (fexp2cexp e)) 0) True) []) 1

-- Setting for apply + stack + unbox + naive
-- stackau :: ApplyOptTranslate AOptType
-- stackau = stackApplyUnbox

-- translateSAU :: String -> Expr Int (Var, Type Int) -> AOptType (J.CompilationUnit, Type Int)
-- translateSAU = createWrap (up stackau)

-- compileSAU :: Compilation
-- compileSAU name e = evalState (runReaderT (runReaderT (translateSAU name (fexp2cexp e)) (0, True)) []) 1
