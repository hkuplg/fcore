{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XConstraintKinds -XScopedTypeVariables #-}

module UnboxTransCFJava where

import Prelude hiding (init, last)

import qualified Data.Set as Set

import qualified Language.Java.Syntax as J
import ClosureF
import Inheritance
import BaseTransCFJava
import StringPrefixes
import MonadLib

data UnboxTransCFJava m = NT {toT :: Translate m}

instance (:<) (UnboxTransCFJava m) (Translate m) where
   up              = up . toT

instance (:<) (UnboxTransCFJava m) (UnboxTransCFJava m) where
   up              = id

transUnbox :: (MonadState Int m, selfType :< UnboxTransCFJava m, selfType :< Translate m) => Mixin selfType (Translate m) (UnboxTransCFJava m)
transUnbox this super = NT {toT = super {
  translateM = undefined,
  translateScopeTyp = undefined

}}
