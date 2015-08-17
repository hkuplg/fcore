
module Predef (injectPredef) where

-- import Data.ByteString       (ByteString, unpack)
-- import Data.Char             (chr)
-- import Data.List             (intercalate)

injectPredef :: String -> String
injectPredef s = s -- predef ++ s

-- predef :: String
-- predef = intercalate "\n" (map toString [predefIO, predefList])

-- predefIO :: ByteString
-- predefIO = $(embedFile "lib/predef/IO.sf")

-- predefList :: ByteString
-- predefList = $(embedFile "lib/predef/List.sf")

-- toString :: ByteString -> String
-- toString = map (chr . fromEnum) . unpack
