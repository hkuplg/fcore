module StaticAssert (staticAssert) where

import Control.Monad        (unless)
import Language.Haskell.TH  (Q, reportError)

-- http://stackoverflow.com/questions/6648764/compile-time-assertions-with-ghc-haskell

staticAssert :: String -> Bool -> Q [a]
staticAssert msg cond = do
  unless cond (reportError ("Compile time assertion failed: " ++ msg))
  return []
