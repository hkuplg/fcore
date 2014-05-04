{-# OPTIONS -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures #-}

module Translations (module Translations) where 

--import Mixins
import Inheritance
import ApplyTransCFJava
import StackTransCFJava
import BaseTransCFJava

import qualified Data.Map as Map
import qualified Language.Java.Syntax as J
import MonadLib

-- Naive translation

naive :: (MonadState Int m, MonadState (Map.Map J.Exp Int) m) => Translate m
naive = new trans

-- Apply optimization

applyopt :: (MonadState Int m, MonadWriter Bool m, MonadState (Map.Map J.Exp Int) m) => ApplyOptTranslate m
applyopt = new (transApply $> trans)

-- Stack translation

--trStack1 :: (MonadState Int m, MonadWriter Bool m) => Mixin (TranslateStack m) (Translate m) (TranslateStack m) -- need to instantiate records
--trStack1 = transS

stackNaive :: (MonadState Int m, MonadWriter Bool m, MonadState (Map.Map J.Exp Int) m) => TranslateStack m
stackNaive = new (transS $> trans)

-- Stack/Apply translation

adaptApply mix this super = toT $ mix this super

stackApply :: (MonadState Int m, MonadWriter Bool m, MonadState (Map.Map J.Exp Int) m) => TranslateStack m
stackApply = new ((transS <.> (adaptApply transApply)) $> trans)

instance (:<) (TranslateStack m) (ApplyOptTranslate m) where
  up = NT . toTS
  
  

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