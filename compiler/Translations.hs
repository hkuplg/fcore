{-# OPTIONS -XFlexibleContexts #-}

module Translations where 

import Mixins
import ApplyTransCFJava
import StackTransCFJava
import BaseTransCFJava

import Control.Monad.State
import Control.Monad.Writer

-- Naive translation

naive :: (MonadState Int m) => Translate m
naive = new trans

-- apply() distinction 
transMixA :: (MonadState Int m, MonadWriter Bool m) => Open (ApplyOptTranslate m)
transMixA this = NT (trans (toT this))

applyopt :: (MonadState Int m, MonadWriter Bool m) => ApplyOptTranslate m
applyopt = new (transApply . transMixA)

-- Stack-based translation 

-- Adaptor mixin for trans

transMix :: (MonadState Int m, MonadWriter Bool m) => Open (TranslateStack m)
transMix this = TS (transMixA (toTS this)) (translateScheduleM this)
             
-- mixing in the new translation

stack :: (MonadState Int m, MonadWriter Bool m) => TranslateStack m
stack = new (transS . transMix)

class ToTranslate f where
   to :: f m -> Translate m

instance ToTranslate Translate where
   to = id

instance ToTranslate ApplyOptTranslate where
   to = toT

instance ToTranslate TranslateStack where
   to = toT . toTS
