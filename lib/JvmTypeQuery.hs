module JvmTypeQuery
  ( isJvmType
  , hasConstructor
  , methodTypeOf
  , fieldTypeOf
  , getModuleInfo
  , ModuleInfo(..)
  ) where

import JavaUtils (ClassName, MethodName, FieldName, ModuleName)
import Src (Type)

import Data.Char (isSpace, toLower)
import Data.Maybe (listToMaybe)
import System.IO (hPutStrLn, hGetLine, Handle)
import System.Process (callCommand)

data ModuleInfo = ModuleInfo {
  minfo_name :: String,
  minfo_gname :: String,
  minfo_signature :: Type } deriving (Show)

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
  -> ModuleName
  -> IO (Maybe [ModuleInfo])
-- Also automatically compile imported modules
getModuleInfo h m
  = do callCommand $ "f2j -c -k " ++ m ++ ".sf" -- FIXME: hackish
       s <- sendRecv h ["qModuleInfo", m] >>= fixRet
       case s of
        Nothing -> return Nothing
        Just xs -> return $ listToModuleInfo (wordsWhen (== '$') xs)
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
