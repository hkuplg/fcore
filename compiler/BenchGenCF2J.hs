{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses  -XScopedTypeVariables -XKindSignatures -XUndecidableInstances -XOverlappingInstances #-}

module BenchGenCF2J where

import Prelude hiding (init, last)

import qualified Language.Java.Syntax as J
import ClosureF
import Inheritance
import qualified Data.Map as Map
import qualified Data.Set as Set
import BaseTransCFJava
import StringPrefixes
import MonadLib

data BenchGenTranslate m = TB {
  toTB :: Translate m -- supertype is a subtype of Translate (later on at least)
  }

instance (:<) (BenchGenTranslate m) (Translate m) where
   up              = up . toTB

instance (:<) (BenchGenTranslate m) (BenchGenTranslate m) where -- reflexivity
  up = id

transBench :: (MonadState Int m, MonadState (Map.Map J.Exp Int) m, MonadState (Set.Set J.Exp) m, selfType :< BenchGenTranslate m, selfType :< Translate m) => Mixin selfType (Translate m) (BenchGenTranslate m)
transBench this super = TB {
  toTB = T {  translateM = \e -> translateM super e,

  translateScopeM = \e m -> 
             translateScopeM super e m,

  -- here, I guess, you will mainly do the changes: have a look at BaseTransCFJava (and StackTransCFJava) how it's done currently             
  createWrap = \name exp -> error "not implemented"
  }}
