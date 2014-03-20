module NaiveTransCFJava where

import Mixins
import TransCFJava

-- Naive translation

transNaive = new trans

translate = translateM transNaive

translateScope = translateM transNaive