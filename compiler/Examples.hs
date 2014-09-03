module Examples where

import qualified Src
import Core
import Simplify

import PrettyUtils

import Unsafe.Coerce

import qualified Language.Java.Syntax as J (Op(..))

instance Show (Expr t e) where
  show e = show $ pprExpr basePrec (0,0) (unsafeCoerce e)

-- let rec
--   even : Int -> Int = \(n : Int) -> if n == 0 then True  else odd  (n - 1)
--   odd  : Int -> Int = \(n : Int) -> if n == 0 then False else even (n - 1)
-- in
-- even 42
evenOdd :: Expr t e
evenOdd
  = LetRec
      [(java_int, java_int), (java_int, java_int)]
      (\ids ->
         [ Lam java_int (\n -> If (Var n `eq` zero) true  (App (Var (ids !! 1)) (Var n `minus` one)))
         , Lam java_int (\n -> If (Var n `eq` zero) false (App (Var (ids !! 0)) (Var n `minus` one)))])
      (\ids ->
         App (Var (ids !! 1)) magic_number)
  where
    java_int     = JClass "java.lang.Integer"
    zero         = Lit (Src.Integer 0)
    one          = Lit (Src.Integer 1)
    magic_number = Lit (Src.Integer 42)
    true         = Lit (Src.Boolean True)
    false        = Lit (Src.Boolean False)
    x `eq` y     = PrimOp x (Src.Compare J.Equal) y
    x `minus` y  = PrimOp x (Src.Arith J.Sub) y