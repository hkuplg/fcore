{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

{- |
Module      :  SrcLoc
Description :  This module contains types and utility functions for tagging things with locations
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Weixin Zhang <zhangweixinxd@gmail.com>
Stability   :  experimental
Portability :  portable
-}

module SrcLoc where

import qualified Config

import Text.PrettyPrint.ANSI.Leijen

type Located a = GenLocated Loc a

data GenLocated l e = L l e
                      deriving (Eq, Ord, Show) -- Typeable, Data)

instance Functor (GenLocated l) where
  fmap f (L l e) = L l (f e)

deriving instance Foldable    (GenLocated l)
deriving instance Traversable (GenLocated l)

data Loc = Loc { line :: !Int, column :: !Int }
         | NoLoc
           deriving (Eq, Ord, Show)

instance Pretty Loc where
    pretty (Loc l c) = int l <> colon <> int c <> colon
    pretty NoLoc = empty

unLoc :: Located a -> a
unLoc (L _ x) = x

withLoc :: b -> Located a -> Located b
x `withLoc` (L loc _) = L loc x

noLoc :: a -> Located a
noLoc = L NoLoc

withLocs :: b -> [Located a] -> Located b
withLocs x [] = noLoc x
withLocs x (l:_) = x `withLoc` l
