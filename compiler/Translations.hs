{-# OPTIONS -XFlexibleContexts #-}

module Translations where 

import Mixins
import TransCFJava
import StackTransCFJava
import UberNaiveTransCFJava

import Control.Monad.State
import Control.Monad.Writer

-- Naive translation

naive :: (MonadState Int m, MonadWriter Bool m) => Translate m
naive = new trans

-- Stack-based translation 

-- Adaptor mixin for trans

transMix :: (MonadState Int m, MonadWriter Bool m) => Open (TranslateStack m)
transMix this = TS (trans (toT this)) (translateScheduleM this)
             
-- mixing in the new translation

stack :: (MonadState Int m, MonadWriter Bool m) => TranslateStack m
stack = new (transS . transMix)

-- adaptor for the translation without apply() distinction - not sure whether really needed
transMixN :: (MonadState Int m, MonadWriter Bool m) => Open (NaiveTranslate m)
transMixN this = NT (trans (toTr this))

ubernaive :: (MonadState Int m, MonadWriter Bool m) => NaiveTranslate m
ubernaive = new (transUN . transMixN)