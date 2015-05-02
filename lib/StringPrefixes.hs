module StringPrefixes where

namespace :: String
namespace = "f2j."

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
closureInput = "arg"

-- Closure out field
closureOutput :: String
closureOutput = "res"

-- Unboxed
closureInputO :: String
closureInputO = "oarg"

closureOutputO :: String
closureOutputO = "ores"

closureInputL :: String
closureInputL = "larg"

closureOutputL :: String
closureOutputL = "lres"

cuClassName :: String
cuClassName = "f2j.unbox.Closure"

letClass :: String
letClass = "Let"

-- Datatype tag
datatypetag :: String
datatypetag = "tag"

fieldtag :: String
fieldtag = "field"
