module JVMTypeQuery (isJVMType, hasConstructor, hasMethod) where 

-- 
import System.IO.Unsafe (unsafePerformIO)
import Data.Char (isSpace, toLower)
-- 
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C


-- 
client :: String   -- addr
       -> Int      -- port
       -> String   -- data
       -> String   -- result
client host port dat = unsafePerformIO $ withSocketsDo $ 
  do  addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
      let serverAddr = head addrInfo
      sock <- socket (addrFamily serverAddr) Stream defaultProtocol
      connect sock (addrAddress serverAddr)
      sendAll sock (C.pack dat)
      ret <- recv sock 1024
      sClose sock
      --putStr "Received "
      --C.putStrLn ret
      return (C.unpack ret)


-- if a string is true
isTrue :: String -> Bool
isTrue s = "true" == (map toLower $ filter (not . isSpace) s)


-- send data & get result back
doQuery :: [String] -> String
doQuery args = client "127.0.0.1" 12345 (unwords args ++ "$")


-- isJVMType "java.lang.String"

isJVMType :: String -> Bool
isJVMType name = isTrue $ doQuery ["queryType", name]


-- "java.lang.String" `hasConstructor` ["java.lang.String"]

hasConstructor :: String -> [String] -> Bool
hasConstructor className args = isTrue $ doQuery $ ["queryNew", className] ++ args


-- hasMethod "java.lang.Integer" "toString" []

hasMethod :: String -> String -> [String] -> Bool
hasMethod className methodName args = isTrue $ doQuery $ ["queryMethod", className, methodName] ++ args
