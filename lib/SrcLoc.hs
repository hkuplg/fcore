{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module SrcLoc where

import Data.Foldable ( Foldable )
import Data.Traversable ( Traversable )

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

unLoc :: Located a -> a
unLoc (L _ x) = x

withLoc :: b -> Located a -> Located b
x `withLoc` (L l _) = L l x

noLoc :: a -> Located a
noLoc = L NoLoc

withLocs :: b -> [Located a] -> Located b
withLocs x [] = noLoc x
withLocs x (l:_) = x `withLoc` l
