module JvmTypeQuery
  ( isJvmType
  , hasConstructor
  , methodTypeOf
  , fieldTypeOf
  , getModuleInfo
  , ModuleInfo(..)
  ) where

import JavaUtils (ClassName, MethodName, FieldName, ModuleName)
import Src (Type, PackageName)
import StringUtils

import Control.Monad (unless)
import Data.Char (isSpace, toLower)
import Data.List
import Data.List.Split
import Data.Maybe (listToMaybe)
import System.Directory
import System.FilePath
import System.IO (hPutStrLn, hGetLine, Handle)
import System.Process.Extra (system_)

data ModuleInfo = ModuleInfo {
  minfoName :: String,  -- | source name
  minfoGname :: String, -- | java variable name
  minfoSignature :: Type } deriving (Show)

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

getModuleInfo
  :: (Handle, Handle)
  -> (Maybe Src.PackageName, ModuleName)
  -> Bool -- ^ True for loading prelude
  -> IO (Maybe ([ModuleInfo], ModuleName))
-- Also automatically compile imported modules
-- FIXME: cause error if a module has import and package declaration
getModuleInfo h (p, m) loadPrelude
  = do currDir <- getCurrentDirectory
       let moduleDir = maybe "" (\name -> currDir </> intercalate [pathSeparator] (splitOn "." name)) p
       unless loadPrelude $ system_ $ "f2j --compile " ++ moduleDir </> m ++ ".sf"
       s <- sendRecv h ["qModuleInfo", ((maybe "" (++ ".") p) ++ capitalize m)] >>= fixRet
       case s of
        Nothing -> return Nothing
        Just xs -> return $ (maybe Nothing (\info -> Just (info, m)) (listToModuleInfo (wordsWhen (== '$') xs)))
    where wordsWhen p s =  case dropWhile p s of
                            "" -> []
                            s' -> w : wordsWhen p s''
                              where (w, s'') = break p s'
          maybeRead = fmap fst . listToMaybe . reads
          listToModuleInfo [] = return []
          listToModuleInfo (x:y:z:xs) = do xs' <- listToModuleInfo xs
                                           ty' <- maybeRead z :: Maybe Type
                                           return $ ModuleInfo x y ty' : xs'
          listToModuleInfo _ = Nothing
