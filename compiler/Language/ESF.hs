module Language.ESF where

import Language.ESF.Syntax
import Language.ESF.Parser
import Language.ESF.TypeCheck

import Text.PrettyPrint.Leijen

toptype :: String -> IO ()
toptype s =
  case infer (reader s) of
    Left err -> print (pretty err)
    Right t  -> print (pretty t)

id1 = "let rec f : forall A. A -> A = /\\B. \\(x:B). x in f"

id2 = "let rec f A : A -> A = \\(x:A). x in f"

id2_1 = "let rec f A : A -> A = \\(x:B). x in f"

id3 = "let rec f A (x:A) : A = x in f"

even_odd =
  "let rec even (n:Int) : Int = if0 n then 1 else odd  (n-1) \
      \and odd  (n:Int) : Int = if0 n then 0 else even (n-1) \
  \in \
  \odd"

even_odd_1 =
  "let rec even (n:Int) : Int = if0 n then 1 else odd  (n-1) \
      \and odd  (n:Int) : Int = if0 n then even else even \
  \in \
  \odd"

even_odd_2 =
  "let rec even (n:Int) : Int = if0 n then 1 else odd  (n-1) \
      \and odd  (n:Int) = if0 n then even else even \
  \in \
  \odd"
