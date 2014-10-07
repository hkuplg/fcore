{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-unused-imports -fno-warn-unused-matches #-}

module F2J where

import qualified Src
import qualified Core
import Parser
import TypeCheck
import Desugar
import Simplify

import PrettyUtils

import qualified Language.Java.Syntax as J (Op(..))

import System.Directory
import Control.Monad
import Prelude hiding (read)

p = Core.pprExpr basePrec (0,0)

rep :: String -> IO ()
rep s =
  do let parsed = reader s
     r <- typeCheck parsed
     case r of
       Left typeError       -> print typeError
       Right (tchecked, _t) -> do
         let desugared  = desugar tchecked
         let simplified = simplify desugared
         print (p simplified)

mike = "{ name = \"Mike\", age = 21, gender = \"M\" }"
nameOfMike = "{ name = \"Mike\", age = 21, gender = \"M\" }.name"
nameOfMike2 = "{ firstName = { name = \"Mike\", age = 21 }.name, age = 38 }.firstName"

mike2 = "{ name = \"Mike\", age = 21 } with { age = 41 }"

time = "\\(person : { age : Int }). person with { age = person.age + 20 }"
mike3 = "(" ++ time ++ ") " ++ mike

time2 = "/\\ R. \\(person : ({ age : Int } & R)). person with { age = (person R).age + 20 }"
mike4 = "(" ++ time2 ++ ") {age:Int} " ++ mike