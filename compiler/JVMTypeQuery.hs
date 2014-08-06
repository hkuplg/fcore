module JVMTypeQuery (getConnection, isJVMType, hasConstructor, hasMethod) where 

import Data.Char (isSpace, toLower)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C


--
getConnection :: IO Socket
getConnection = withSocketsDo $ 
  do  addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just "12345")
      let serverAddr = head addrInfo
      sock <- socket (addrFamily serverAddr) Stream defaultProtocol
      connect sock (addrAddress serverAddr)
      return sock


-- send data & get result back
doQuery :: Socket -> [String] -> IO Bool
doQuery sock args =
  let dat = (unwords args ++ "$") in
  do  sendAll sock (C.pack dat)
      ret <- recv sock 1024
      return ("true" == (map toLower $ filter (not . isSpace) $ C.unpack ret))


-- isJVMType "java.lang.String"

isJVMType :: Socket -> String -> IO Bool
isJVMType sock name = doQuery sock ["queryType", name]


-- "java.lang.String" `hasConstructor` ["java.lang.String"]

hasConstructor :: Socket -> String -> [String] -> IO Bool
hasConstructor sock className args = doQuery sock $ ["queryNew", className] ++ args


-- hasMethod "java.lang.Integer" "toString" []

hasMethod :: Socket -> String -> String -> [String] -> IO Bool
hasMethod sock className methodName args = doQuery sock $ ["queryMethod", className, methodName] ++ args
