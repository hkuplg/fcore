module JVMTypeQuery (isJVMType, hasConstructor, methodRetType) where 

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

methodRetType :: (Handle, Handle) -> String -> String -> [String] -> IO (Maybe String)
methodRetType io className methodName args = 
  do ret <- doQuery io $ ["qMethod", className, methodName] ++ args
     return $ if ret == "$" then Nothing else Just ret

