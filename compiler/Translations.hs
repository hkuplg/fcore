{-# OPTIONS -XFlexibleContexts #-}

module Translations where 

import Mixins
import TransCFJava
import StackTransCFJava
import UberNaiveTransCFJava

import Control.Monad.State

-- Naive translation

naive :: MonadState Int m => Translate m
naive = new trans

-- Stack-based translation 

-- Adaptor mixin for trans

transMix :: MonadState Int m => Open (TranslateStack m)
transMix this = TS (trans (toT this)) (translateScheduleM this)
             
-- mixing in the new translation

stack :: MonadState Int m => TranslateStack m
stack = new (transS . transMix)

-- adaptor for the translation without apply() distinction - not sure whether really needed
transMixN :: MonadState Int m => Open (NaiveTranslate m)
transMixN this = NT (trans (toTr this))

ubernaive :: MonadState Int m => NaiveTranslate m
ubernaive = new (transUN . transMixN)