{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
{- |
Module      :  Simplify
Description :  The simplifier turns SystemFI into Core.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>, Haoyuan Zhang <zhanghaoyuan00@gmail.com>
Stability   :  experimental
Portability :  portable

The simplifier translates System F with intersection types to vanilla System F.
-}

module Simplify
  ( simplify
  , simplify'
  ) where

import qualified SimplifyImpl         as Impl

import Core
import qualified SystemFI             as FI

import Unsafe.Coerce (unsafeCoerce)

simplify :: FI.FExp -> Expr t e
simplify = Impl.simplify

simplify' :: FI.Expr t e -> Expr t e
simplify' = Impl.dedeBruE 0 [] 0 [] . Impl.transExpr 0 0 . unsafeCoerce
