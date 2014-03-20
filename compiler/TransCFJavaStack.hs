{-# OPTIONS -XRankNTypes -XFlexibleInstances #-}

module TransCFJavaStack where

import Prelude hiding (init)
import Debug.Trace
import Data.List hiding (init)

import Control.Monad.State

import qualified Language.Java.Syntax as J
import Language.Java.Pretty
import ClosureF
import Mixins