module JvmTypeQuery
  ( isJvmType
  , hasConstructor
  , methodTypeOf
  , fieldTypeOf
  ) where

import JavaUtils

import System.IO           (hPutStrLn, hGetLine, Handle)
import Control.Applicative ((<$>))
import Data.Char           (isSpace, toLower)

sendRecv :: (Handle, Handle) -> [String] -> IO String
sendRecv (to, from) args =
  do hPutStrLn to (unwords args)
     hGetLine from

fixRet :: String -> IO (Maybe String)
fixRet "$" = return Nothing
fixRet str = return (Just str)

isTrue :: String -> Bool
isTrue s = (map toLower . filter (not . isSpace)) s == "true"

isJvmType :: (Handle, Handle) -> ClassName -> IO Bool
isJvmType h c = isTrue <$> sendRecv h ["qType", c]

hasConstructor :: (Handle, Handle) -> ClassName -> [ClassName] -> IO Bool
hasConstructor h c args
  = isTrue <$> sendRecv h (["qConstructor", c] ++ args)

methodTypeOf
  :: (Handle, Handle)
  -> ClassName
  -> (MethodName, Bool)   -- True <=> static method
  -> [ClassName]          -- Class of the arguments
  -> IO (Maybe ClassName)
methodTypeOf h c (m, is_static) args
  = sendRecv h ([tag, c, m] ++ args) >>= fixRet
  where
    tag = if is_static
             then "qStaticMethod"
             else "qMethod"

fieldTypeOf
  :: (Handle, Handle)
  -> ClassName
  -> (FieldName, Bool)    -- True <=> static field
  -> IO (Maybe ClassName)
fieldTypeOf h c (f, is_static)
  = sendRecv h [tag, c, f] >>= fixRet
  where
    tag = if is_static
             then "qStaticField"
             else "qField"
