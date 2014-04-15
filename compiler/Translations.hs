{-# OPTIONS -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures #-}

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
transMixA :: (MonadState Int m, MonadWriter Bool m, f :< Translate) => Open (ApplyOptTranslate f m)
transMixA this = NT (mapG trans (toT this))

applyopt :: (MonadState Int m, MonadWriter Bool m, f :< Translate) => ApplyOptTranslate f m
applyopt = new (transApply . transMixA)

transMixS :: (MonadState Int m, MonadReader (Map.Map J.Exp J.Exp) m, f :< Translate) => Open (SubstIntVarTranslate f m)
transMixS this = VNT (mapG trans (toTST this)) (translateSubst this) (translateScopeSubst this)

substopt :: (MonadState Int m, MonadReader (Map.Map J.Exp J.Exp) m, f :< Translate) => SubstIntVarTranslate f m
substopt = new (transNewVar . transMixS)

-- Stack-based translation 

-- Adaptor mixin for trans

transMix :: (MonadState Int m, MonadWriter Bool m, f :< Translate) => Open (TranslateStack (ApplyOptTranslate f) m)
transMix this = TS (transMixA (toTS this)) (translateScheduleM this)
             
-- mixing in the new translation

stack :: (MonadState Int m, MonadWriter Bool m, f :< Translate) => TranslateStack (ApplyOptTranslate f) m
stack = new (transS . transMix)
