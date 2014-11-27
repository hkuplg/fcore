-- Static assertions that should hold at compile time.

{-# LANGUAGE TemplateHaskell #-}

module Assertions where

import StaticAssert

import qualified Src  as S
import qualified Core as C
import Simplify

import Data.Maybe

$(staticAssert "alphaEq implies subtype" (S.subtype (S.TVar "a" `S.And` S.TVar "b") (S.TVar "b" `S.And` S.TVar "a")))
$(staticAssert "alphaEq implies subtype" (subtype 0 (C.TVar 0 `C.And` C.TVar 1)   (C.TVar 1 `C.And` C.TVar 0)))
$(staticAssert "subtype implies coercion exists" (isJust (coerce 0 (C.TVar 0 `C.And` C.TVar 1) (C.TVar 1 `C.And` C.TVar 0))))
