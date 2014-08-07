module JVMTypeQuery (getConnection, isJVMType, hasConstructor, methodRetType) where 

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
doQuery :: Socket -> [String] -> IO String
doQuery sock args =
  let dat = (unwords args ++ "$") in
  do  sendAll sock (C.pack dat)
      ret <- recv sock 1024
      return $ filter (not . isSpace) $ C.unpack ret
      --return ("true" == (map toLower $ filter (not . isSpace) $ C.unpack ret))

--

isTrue :: IO String -> IO Bool
isTrue str = str >>= (\s -> return $ "true" == (map toLower s))

--

isJVMType :: Socket -> String -> IO Bool
isJVMType sock name = isTrue $ doQuery sock ["qType", name]


--

hasConstructor :: Socket -> String -> [String] -> IO Bool
hasConstructor sock className args = isTrue $ doQuery sock $ ["qConstructor", className] ++ args


--

methodRetType :: Socket -> String -> String -> [String] -> IO (Maybe String)
methodRetType sock className methodName args = 
  do ret <- doQuery sock $ ["qMethod", className, methodName] ++ args
     if ret == "$"
       then return $ Nothing
       else return $ Just ret

