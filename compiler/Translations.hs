module Translations where 

import Mixins
import TransCFJava
import StackTransCFJava

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
