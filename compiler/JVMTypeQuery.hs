module JVMTypeQuery (isJVMType, hasConstructor, hasMethod) where 


import System.Process (readProcess)
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (isSpace, toLower)


typeServerName :: String
typeServerName = "TypeServer"


isTrue :: String -> Bool
isTrue s = "true" == (map toLower $ filter (not . isSpace) s)


doQuery :: [String] -> String
doQuery args = unsafePerformIO $ readProcess "java" (typeServerName : args) []



-- isJVMType "java.lang.String"

isJVMType :: String -> Bool
isJVMType name = isTrue $ doQuery ["queryType", name]


-- "java.lang.String" `hasConstructor` ["java.lang.String"]

hasConstructor :: String -> [String] -> Bool
hasConstructor className args = isTrue $ doQuery $ ["queryNew", className] ++ args


-- hasMethod "java.lang.Integer" "toString" []

hasMethod :: String -> String -> [String] -> Bool
hasMethod className methodName args = isTrue $ doQuery $ ["queryMethod", className, methodName] ++ args
