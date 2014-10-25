module StringPrefixes where

import Data.List

namespace :: String
namespace = "f2j."

listlast :: [a] -> a
listlast = Data.List.last
-- field and closures

localvarstr :: String
localvarstr = "x"


ifresultstr :: String
ifresultstr = "ifres"

-- temporary vars
tempvarstr :: String
tempvarstr = "temp"

-- Closure input field
closureInput :: String
closureInput = "x"
