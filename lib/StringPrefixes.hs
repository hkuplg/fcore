module StringPrefixes where

namespace :: String
namespace = "f2j."

-- field and closures

closureClass :: String
closureClass = namespace ++ "Closure"

closureTransName :: String
closureTransName = "F"

letTransName :: String
letTransName = "L"

localvarstr :: String
localvarstr = "x"

ifresultstr :: String
ifresultstr = "ifres"

-- temporary vars
tempvarstr :: String
tempvarstr = "temp"

-- Closure input field
closureInput :: String
closureInput = "arg"

-- Closure out field
closureOutput :: String
closureOutput = "res"

-- Datatype tag
datatypetag :: String
datatypetag = "tag"

fieldtag :: String
fieldtag = "field"

-- type annotation
annoName = namespace ++ "ModuleFunction"
annoSrcName = "name"
annoGenName = "gname"
annoSig = "signature"
