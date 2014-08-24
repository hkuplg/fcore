module JVMTypeQuery 
( isJVMType
, hasConstructor
, methodRetType
, staticMethodRetType
, fieldType
, staticFieldType
) where 

import Data.Char (isSpace, toLower)
import System.IO


-- send data & get result back
doQuery :: (Handle, Handle) -> [String] -> IO String
doQuery (inp, out) args = do hPutStrLn inp $ unwords args
                             hGetLine out

--

isTrue :: IO String -> IO Bool
isTrue str = str >>= (\s -> return $ "true" == map toLower (filter (not . isSpace) s))

--

isJVMType :: (Handle, Handle) -> String -> IO Bool
isJVMType io name = isTrue $ doQuery io ["qType", name]


--

hasConstructor :: (Handle, Handle) -> String -> [String] -> IO Bool
hasConstructor io className args = isTrue $ doQuery io $ ["qConstructor", className] ++ args


--
fixRet :: String -> IO (Maybe String)
fixRet "$" = return Nothing
fixRet str = return $ Just str


methodRetType :: (Handle, Handle) -> String -> String -> [String] -> IO (Maybe String)
methodRetType io className methodName args = 
  doQuery io (["qMethod", className, methodName] ++ args) >>= fixRet


staticMethodRetType :: (Handle, Handle) -> String -> String -> [String] -> IO (Maybe String)
staticMethodRetType io className methodName args =
  doQuery io (["qStaticMethod", className, methodName] ++ args) >>= fixRet


fieldType :: (Handle, Handle) -> String -> String -> IO (Maybe String)
fieldType io className fieldName =
  doQuery io ["qField", className, fieldName] >>= fixRet


staticFieldType :: (Handle, Handle) -> String -> String -> IO (Maybe String)
staticFieldType io className fieldName = 
  doQuery io ["qStaticField", className, fieldName] >>= fixRet

