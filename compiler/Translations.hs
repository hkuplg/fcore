{-# OPTIONS -XFlexibleContexts #-}

module Translations where 

import Mixins
import ApplyTransCFJava
import StackTransCFJava
import BaseTransCFJava
import SubstIntVarTransCFJava

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Language.Java.Syntax as J

-- Naive translation

naive :: (MonadState Int m) => Translate m
naive = new trans

-- apply() distinction 
transMixA :: (MonadState Int m, MonadWriter Bool m) => Open (ApplyOptTranslate m)
transMixA this = NT (trans (toT this))

applyopt :: (MonadState Int m, MonadWriter Bool m) => ApplyOptTranslate m
applyopt = new (transApply . transMixA)

transMixS :: (MonadState Int m, MonadReader (Map.Map J.Exp J.Exp) m) => Open (SubstIntVarTranslate m)
transMixS this = VNT (trans (toTST this)) (translateSubst this) (translateScopeSubst this)

substopt :: (MonadState Int m, MonadReader (Map.Map J.Exp J.Exp) m) => SubstIntVarTranslate m
substopt = new (transNewVar . transMixS)
--
-- Stack-based translation 

-- Adaptor mixin for trans

transMix :: (MonadState Int m, MonadWriter Bool m) => Open (TranslateStack m)
transMix this = TS (applyopt) (translateScheduleM this)
             
-- mixing in the new translation

stack :: (MonadState Int m, MonadWriter Bool m) => TranslateStack m
stack = new (transS . transMix)
