{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module JvmTypeQuery
  ( Connection -- Hide data constructors
  , withConnection
  , definedClass
  , definedConstructor
  , findMethodReturnType
  , findFieldType
  ) where

import RuntimeProcessManager (withRuntimeProcess)
import JavaUtils (ClassName, MethodName, FieldName)

import System.IO
import Control.Applicative ((<$>))
import Data.Char           (isSpace, toLower)

data Connection = Connection
  { -- private
    _toHandle   :: Handle
  , _fromHandle :: Handle
  }

withConnection :: (Connection -> IO a) -> IO a
withConnection action
  = withRuntimeProcess "TypeServer" NoBuffering (\(toHandle, fromHandle) ->
      action Connection { _toHandle = toHandle, _fromHandle = fromHandle }
    )

sendRecv :: Connection -> [String] -> IO String
sendRecv conn args =
  do hPutStrLn (_toHandle conn) (unwords args)
     hGetLine (_fromHandle conn)

fixRet :: String -> IO (Maybe String)
fixRet "$" = return Nothing
fixRet str = return (Just str)

isTrue :: String -> Bool
isTrue s = (map toLower . filter (not . isSpace)) s == "true"

definedClass :: Connection -> ClassName -> IO Bool
definedClass conn c = isTrue <$> sendRecv conn ["qType", c]

definedConstructor :: Connection -> ClassName -> [ClassName] -> IO Bool
definedConstructor conn c params
  = isTrue <$> sendRecv conn (["qConstructor", c] ++ params)

findMethodReturnType
  :: Connection
  -> ClassName
  -> (Bool, MethodName)   -- True <=> static method
  -> [ClassName]          -- Class of the arguments
  -> IO (Maybe ClassName)
findMethodReturnType conn c (is_static, m) args
  = sendRecv conn ([tag, c, m] ++ args) >>= fixRet
  where
    tag = if is_static
             then "qStaticMethod"
             else "qMethod"

findFieldType
  :: Connection
  -> ClassName
  -> (Bool, FieldName)    -- True <=> static field
  -> IO (Maybe ClassName)
findFieldType conn c (is_static, f)
  = sendRecv conn [tag, c, f] >>= fixRet
  where
    tag = if is_static
             then "qStaticField"
             else "qField"

-- Tests inteneded to be run by `runhaskell`

main :: IO ()
main = withConnection (\conn ->
  do definedClass conn "java.lang.String" >>= print
     definedClass conn "java.lang.Foo"  >>= print
     definedConstructor conn "java.lang.String" [] >>= print
     definedConstructor conn "java.lang.String" ["java.lang.String"] >>= print
     definedConstructor conn "java.lang.String" ["Foo"] >>= print
     findMethodReturnType conn "java.lang.String" (False, "concat") ["java.lang.String"] >>= print
     findMethodReturnType conn "java.lang.String" (False, "length") [] >>= print
     findMethodReturnType conn "java.lang.String" (True, "valueOf") ["java.lang.Integer"] >>= print
     findMethodReturnType conn "java.lang.String" (True, "valueOf") ["Foo"] >>= print
  )
