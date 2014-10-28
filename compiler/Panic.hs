{-# LANGUAGE DeriveDataTypeable #-}

module Panic
  ( F2jException(..)
  , showF2jException
  , throwF2jException, throwF2jExceptionIO

  , panic, sorry
  , trueIffSameDataCons
  ) where

import Control.Exception
import Data.Data

data F2jException
  -- | The 'impossible' happened.
  = Panic String

  -- | The user tickled something that's known not to work yet,
  --   but we're not counting it as a bug.
  | Sorry String
  deriving (Typeable)

instance Exception F2jException

instance Show F2jException where
  showsPrec _ = showF2jException

showF2jException :: F2jException -> String -> String
showF2jException exception
  = case exception of
      Panic s ->
        showString $
          "panic! (the 'impossible' happened)\n" ++
          s ++ "\n\n" ++
          "Please report this as a bug:  " ++
          "https://bitbucket.org/brunosoliveira/systemfcompiler/issues/new"

      Sorry s ->
        showString $
          "sorry! (unimplemented feature or known bug)\n\t" ++
          s ++ "\n"

throwF2jException :: F2jException -> a
throwF2jException = Control.Exception.throw

throwF2jExceptionIO :: F2jException -> IO a
throwF2jExceptionIO = Control.Exception.throwIO

-- | Panic and asserts.
panic, sorry :: String -> a
panic s = throwF2jException (Panic s)
sorry s = throwF2jException (Sorry s)

trueIffSameDataCons :: (Data a, Data b) => String -> a -> b -> Bool
trueIffSameDataCons panic_msg t1 t2
  | toConstr t1 == toConstr t2 = panic panic_msg
  | otherwise                  = False
