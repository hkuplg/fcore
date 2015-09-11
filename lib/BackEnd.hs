{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , KindSignatures
           , MultiParamTypeClasses
           , RankNTypes
           #-}

{-# OPTIONS_GHC -fno-warn-unused-binds -fwarn-incomplete-patterns -fno-warn-orphans #-}

module BackEnd
    ( Compilation
    , compileN
    , compileN2
    , compileAO
    , compileS
    -- , compileSAU
    , compileSN
    -- , compileUnbox
    -- , compileAoptUnbox
    -- , compileSU
    , core2java
    , DumpOption(..)
    ) where

import           ApplyTransCFJava
import qualified BaseTransCFJava as OB
import qualified BaseTransCFJavaNew as NB
-- import           BenchGenCF2J
-- import           BenchGenStack
import qualified ClosureF
import qualified ClosureFNew
import qualified Core
import qualified CoreNew
import           Inheritance
import           Inliner
import           JavaUtils (ClassName)
import           OptiUtils
import           MonadLib
import           PartialEvaluator
import           PrettyUtils
import           StackTransCFJava
-- import           UnboxTransCFJava

import           Language.Java.Pretty
import qualified Language.Java.Syntax as J
import           Prelude hiding (exp)


data DumpOption
  = NoDump
  | Parsed
  | TChecked
  | SystemFI
  | SimpleCore
  | ClosureF
    deriving (Eq, Show)

-- Naive translation

naive :: MonadState Int m => OB.Translate m
naive = new OB.trans

naiveN :: MonadState Int m => NB.Translate m
naiveN = new NB.trans

-- Apply naive optimization

applyopt :: (MonadState Int m, MonadReader Int m, MonadReader OB.InitVars m) => ApplyOptTranslate m
applyopt = new (transApply $> OB.trans)

-- Stack naive optimization

stackNaive :: (MonadState Int m, MonadReader Bool m) => TranslateStack m
stackNaive = new (transS $> OB.trans)

-- Unbox naive translation

-- unboxopt :: (MonadState Int m, MonadReader (Int, Bool) m) => UnboxTranslate m
-- unboxopt = new (transUnbox $> trans)


-- Stack/Apply/Unbox translation

adaptApply :: forall (m :: * -> *) t t1.
              (t -> t1 -> ApplyOptTranslate m) -> t -> t1 -> OB.Translate m
adaptApply mix' this super = toT $ mix' this super

adaptStack :: forall (m :: * -> *) t t1.
              (t -> t1 -> TranslateStack m) -> t -> t1 -> OB.Translate m
adaptStack mix' this super = toTS $ mix' this super

-- adaptUnbox :: forall (m :: * -> *) t t1.
--               (t -> t1 -> UnboxTranslate m) -> t -> t1 -> Translate m
-- adaptUnbox mix' this super = toUT $ mix' this super

-- Stack + Apply + Naive

stackApplyNew :: (MonadState Int m, MonadReader Int m, MonadReader Bool m, MonadReader OB.InitVars m) => ApplyOptTranslate m
stackApplyNew = new ((transAS <.> adaptStack transSA) $> OB.trans)

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

-- instance (:<) (BenchGenTranslateOpt m) (ApplyOptTranslate m) where
--   up = NT . toTBA

-- instance (:<) (BenchGenTranslateStack m) (TranslateStack m) where
--   up = TS . toTBS

-- instance (:<) (BenchGenTranslateStackOpt m) (ApplyOptTranslate m) where
--   up = NT . toTBSA

-- instance (:<) (BenchGenTranslateStackOpt m) (TranslateStack m) where
--   up = TS . toTBSA

-- instance (:<) (BenchGenTranslateStackOpt m) (UnboxTranslate m) where
--   up = UT . toTBSA

-- prettyJ :: Language.Java.Pretty.Pretty a => a -> IO ()
-- prettyJ = putStrLn . prettyPrint

-- | Core expression to Java.
core2java :: Bool -> Bool -> DumpOption -> Compilation -> ClassName -> Exp -> IO String
core2java supernaive optInline optDump compilation className closedCoreExpr =
  do let rewrittenCore = rewriteAndEval closedCoreExpr
     let recurNumOfCore =
           if optInline  -- inline
              then recurNum rewrittenCore
              else 0
     let inlineNum =
           if recurNumOfCore > 2
              then 0
              else recurNumOfCore
     let inlinedCore =
           case inlineNum of
             1 -> inliner rewrittenCore
             2 -> inliner . inliner $ rewrittenCore
             _ -> rewrittenCore
     when (optDump == SimpleCore) $
       print (Core.prettyExpr rewrittenCore)
     when (optDump == ClosureF) $
       print (ClosureF.prettyExpr basePrec
                                  (0,0)
                                  (ClosureF.fexp2cexp inlinedCore))
     let (cu,_) =
           if supernaive
              then compilation className
                               (reveal closedCoreExpr)
              else compilation className inlinedCore
     return $ prettyPrint cu

type Compilation = String -> Core.Expr Int (OB.Var,ClosureF.Type Int) -> (J.CompilationUnit,ClosureF.Type Int)

type Compilation2 = String -> CoreNew.Expr NB.TransBind -> (J.CompilationUnit, ClosureFNew.Type NB.TransBind)

-- | setting for various combination of optimization
-- Setting for naive
type NType = State Int

ninst :: OB.Translate NType  -- instantiation; all coinstraints resolved
ninst = naive

ninstN :: NB.Translate NType  -- instantiation; all coinstraints resolved
ninstN = naiveN

translateN
  :: String
  -> ClosureF.Expr Int (OB.Var,ClosureF.Type Int)
  -> NType (J.CompilationUnit,ClosureF.Type Int)
translateN = OB.createWrap (up ninst)

translateN2
  :: String
  -> ClosureFNew.Expr NB.TransBind
  -> NType (J.CompilationUnit,ClosureFNew.Type NB.TransBind)
translateN2 = NB.createWrap (up ninstN)

compileN :: Compilation
compileN name e = evalState (translateN name (ClosureF.fexp2cexp e)) 1

compileN2 :: Compilation2
compileN2 name e = evalState (translateN2 name (ClosureFNew.fexp2cexp e)) 1

-- Setting for apply + naive
type AOptType = ReaderT Int (ReaderT OB.InitVars (State Int))

aoptinst :: ApplyOptTranslate AOptType  -- instantiation; all coinstraints resolved
aoptinst = applyopt

translateAO
  :: String
  -> ClosureF.Expr Int (OB.Var,ClosureF.Type Int)
  -> AOptType (J.CompilationUnit,ClosureF.Type Int)
translateAO = OB.createWrap (up aoptinst)

compileAO :: Compilation
compileAO name e = evalState (runReaderT (runReaderT (translateAO name (ClosureF.fexp2cexp e)) 0) []) 1

-- Setting for stack + naive
type StackNaiveType = ReaderT Bool (State Int)

stackNaiveinst :: TranslateStack StackNaiveType  -- instantiation; all coinstraints resolved
stackNaiveinst = stackNaive

translateSN
  :: String
  -> ClosureF.Expr Int (OB.Var,ClosureF.Type Int)
  -> StackNaiveType (J.CompilationUnit,ClosureF.Type Int)
translateSN = OB.createWrap (up stackNaiveinst)

compileSN :: Compilation
compileSN name e = evalState (runReaderT (translateSN name (ClosureF.fexp2cexp e)) True) 1

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

type ApplyStackTranslate = ReaderT Int (ReaderT Bool (ReaderT OB.InitVars (State Int)))

-- Setting for apply + stack + naive
stackinst :: ApplyOptTranslate ApplyStackTranslate  -- instantiation; all coinstraints resolved
stackinst = stackApplyNew

translateS
  :: String
  -> ClosureF.Expr Int (OB.Var,ClosureF.Type Int)
  -> ApplyStackTranslate (J.CompilationUnit,ClosureF.Type Int)
translateS = OB.createWrap (up stackinst)

compileS :: Compilation
compileS name e = evalState (runReaderT (runReaderT (runReaderT (translateS name (ClosureF.fexp2cexp e)) 0) True) []) 1

-- Setting for apply + stack + unbox + naive
-- stackau :: ApplyOptTranslate AOptType
-- stackau = stackApplyUnbox

-- translateSAU :: String -> Expr Int (Var, Type Int) -> AOptType (J.CompilationUnit, Type Int)
-- translateSAU = createWrap (up stackau)

-- compileSAU :: Compilation
-- compileSAU name e = evalState (runReaderT (runReaderT (translateSAU name (fexp2cexp e)) (0, True)) []) 1
