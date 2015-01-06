{-# LANGUAGE DeriveDataTypeable #-}

module Panic
  ( F2jException(..)
  , showF2jException
  , throwF2jException, throwF2jExceptionIO

  , panic, sorry
  , prettyPanic, prettySorry
  , panicOnSameDataCons
  ) where

import Text.PrettyPrint.Leijen
import Control.Exception
import Data.Data

data F2jException
  -- | The 'impossible' happened.
  = Panic String (Maybe Doc)

  -- | The user tickled something that's known not to work yet,
  --   but we're not counting it as a bug.
  | Sorry String (Maybe Doc)
  deriving (Typeable)

instance Exception F2jException

instance Show F2jException where
  showsPrec _ = showF2jException

showF2jException :: F2jException -> String -> String
showF2jException exception
  = case exception of
      Panic s maybe_doc ->
        showString $
          "panic! (the 'impossible' happened)\n" ++
          s ++ showMaybeDoc maybe_doc ++ "\n\n" ++
          "Please report this as a bug:  " ++
          "https://bitbucket.org/brunosoliveira/systemfcompiler/issues/new"

      Sorry s maybe_doc ->
        showString $
          "sorry! (unimplemented feature or known bug)\n\t" ++
          s ++ showMaybeDoc maybe_doc
  where
    showMaybeDoc Nothing    = ""
    showMaybeDoc (Just doc) = ": " ++ show doc

throwF2jException :: F2jException -> a
throwF2jException = Control.Exception.throw

throwF2jExceptionIO :: F2jException -> IO a
throwF2jExceptionIO = Control.Exception.throwIO

-- | Panic and asserts.
panic, sorry :: String -> a
panic s = throwF2jException (Panic s Nothing)
sorry s = throwF2jException (Sorry s Nothing)

prettyPanic, prettySorry :: String -> Doc -> a
prettyPanic s doc = throwF2jException (Panic s (Just doc))
prettySorry s doc = throwF2jException (Sorry s (Just doc))

panicOnSameDataCons :: (Data a, Data b) => result -> (String, a, b) -> result
result `panicOnSameDataCons` (panic_msg, t1, t2)
  | toConstr t1 == toConstr t2 = panic panic_msg
  | otherwise                  = result
