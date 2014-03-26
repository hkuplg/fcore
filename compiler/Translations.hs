module Translations where 

import Mixins
import TransCFJava
import StackTransCFJava
import UberNaiveTransCFJava

-- Naive translation

naive :: Translate
naive = new trans

-- Stack-based translation 

-- Adaptor mixin for trans

transMix :: Open TranslateStack 
transMix this = TS (trans (toT this)) (translateScheduleM this)
             
-- mixing in the new translation

stack :: TranslateStack
stack = new (transS . transMix)

-- adaptor for the translation without apply() distinction - not sure whether really needed
transMixN :: Open NaiveTranslate 
transMixN this = NT (trans (toTr this))

ubernaive :: NaiveTranslate
ubernaive = new (transUN . transMixN)