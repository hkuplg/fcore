{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}

module JvmTypeQuery
  ( Connection -- Hide data constructors
  , withConnection
  , definedClass
  , definedConstructor
  , findMethodReturnType
  , findFieldType
  , getModuleInfo
  , extractModuleInfo
  , ModuleInfo(..)
  ) where

import JavaUtils (ClassName, MethodName, FieldName, ModuleName)
import RuntimeProcessManager (withRuntimeProcess)
import Src (Type, PackageName)
import StringUtils

import Control.Exception
import Data.Char (isSpace, toLower)
import Data.Either
import Data.List
import Data.List.Split
import Data.Maybe (listToMaybe)
import System.Directory
import System.FilePath
import System.IO
import System.Process.Extra (system_)

data Connection = Connection
  { -- private
    _toHandle   :: Handle
  , _fromHandle :: Handle
  }

data ModuleInfo = ModuleInfo {
  minfoName :: String,  -- | source name
  minfoGname :: String, -- | java variable name
  minfoSignature :: Type } deriving (Show)

withConnection :: (Connection -> IO a)
               -> Bool -- ^ True for loading prelude
               -> IO a
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

getModuleInfo :: Connection
              -> (Maybe Src.PackageName, ModuleName)
              -> IO (Maybe ([ModuleInfo], ModuleName))
getModuleInfo h (p, m) = do
  s <- sendRecv h ["qModuleInfo", ((maybe "" (++ ".") p) ++ capitalize m)] >>= fixRet
  case s of
    Nothing -> return Nothing
    Just xs -> return $ (maybe Nothing (Just . (,m)) (listToModuleInfo (splitOn "$" (init xs))))

  where
    maybeRead = fmap fst . listToMaybe . reads
    listToModuleInfo [] = return []
    listToModuleInfo (x:y:z:xs) = do
      xs' <- listToModuleInfo xs
      ty' <- maybeRead z :: Maybe Type
      return $ ModuleInfo x y ty' : xs'
    listToModuleInfo _ = Nothing


extractModuleInfo :: Connection
                  -> (Maybe Src.PackageName, ModuleName)
                  -> IO (Maybe ([ModuleInfo], ModuleName))
-- Also automatically compile imported modules
extractModuleInfo h (p, m) = do
  currDir <- getCurrentDirectory
  let moduleDir = maybe "" (\name -> currDir </> intercalate [pathSeparator] (splitOn "." name)) p
  res <- (try . system_ $ "f2j --compile --silent " ++ moduleDir </> m ++ ".sf") :: IO (Either SomeException ())
  if (isLeft res)  -- should catch IO exception if any
    then return Nothing
    else getModuleInfo h (p, m)

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
  ) False
