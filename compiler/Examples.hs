{-# OPTIONS_GHC
    -fno-warn-missing-signatures
    -fno-warn-orphans
    -fno-warn-unused-imports #-}

module Examples where

import qualified Src
import Core
import Simplify

import PrettyUtils

import Unsafe.Coerce

import qualified Language.Java.Syntax as J (Op(..))

instance Show (Expr t e) where
  show e = show $ pprExpr basePrec (0,0) (unsafeCoerce e)

instance Show (Type t) where
  show e = show $ pprType basePrec 0 (unsafeCoerce e)

-- let tailFact : Int -> Int -> Int
--   = \(acc : Int). \(n : Int). if n == 0 then acc else tailFact (acc * n) (n - 1)
-- in
-- tailFact 1 10
tailFact :: Expr t e
tailFact
  = Fix (\tail_fact acc ->
      Lam javaInt (\n ->
        If (Var n `eq` zero)
           (Var acc)
           (Var tail_fact `App` (Var acc `mult` Var n) `App` (Var n `sub` one))))
    javaInt (javaInt `Fun` javaInt)


testTail :: Expr t e
testTail = Fix (\f n -> If (Var n `eq` zero)
                           one
                           (Var f `App` (Var n `sub` one)))
               javaInt
               (javaInt `Fun` javaInt)

fact :: Expr t (Expr t e)
fact = Fix (\f n -> If (Var n `eq` zero)
                       one
                       (Var n `mult` (Var f `App` (Var n `sub` one))))
           javaInt
           (javaInt `Fun` javaInt)

tailFactLike :: Expr t e
tailFactLike
  = Fix (\tail_fact acc ->
      Lam javaInt (\n ->
        If (Var n `eq` zero)
           (Var acc)
           (Var tail_fact `App` (Var acc `mult` one) `App` one)))
    javaInt (javaInt `Fun` javaInt)

-- let rec
--   even : Int -> Int = \(n : Int). if n == 0 then True  else odd  (n - 1)
--   odd  : Int -> Int = \(n : Int). if n == 0 then False else even (n - 1)
-- in
-- even 42
evenOdd :: Expr t e
evenOdd
  = LetRec
      [(javaInt, javaInt), (javaInt, javaInt)]
      (\ids ->
         [ Lam javaInt (\n -> If (Var n `eq` zero) true  (App (Var (ids !! 1)) (Var n `sub` one)))
         , Lam javaInt (\n -> If (Var n `eq` zero) false (App (Var (ids !! 0)) (Var n `sub` one)))])
      (\ids ->
         App (Var (ids !! 1)) magicNumber)

evenOdd1 :: Expr t e
evenOdd1 = LetRec [(javaInt, javaInt), (javaInt, javaInt)]
                  (\ids -> [ Lam javaInt (\n -> If (Var n `eq` zero) true (App (Var (ids !! 1)) (Var n `sub` one)))
                           , Lam javaInt (\n -> If (Var n `eq` zero) false (App (Var (ids !! 0)) (Var n `sub` one)))])
                  (\ids -> App (Lam javaInt (\n -> If (Var n `eq` zero) false (App (Var (ids !! 0)) (Var n `sub` one)))) magicNumber)


evenOdd2 :: Expr t e
evenOdd2
  = LetRec
      [(javaInt, javaInt), (javaInt, javaInt)]
      (\ids ->
         [ Lam javaInt (\n -> If (Var n `eq` zero) true  (App (Lam javaInt (\n -> If (Var n `eq` zero) false (App (Var (ids !! 0)) (Var n `sub` one)))) (Var n `sub` one)))
         , Lam javaInt (\n -> If (Var n `eq` zero) false (App (Var (ids !! 0)) (Var n `sub` one)))])
      (\ids ->
         App (Var (ids !! 1)) magicNumber)

-- Int -> (Int -> Bool, Int -> Bool)
evenOddEncodedTy :: Type t
evenOddEncodedTy = javaInt `Fun` Product [javaInt `Fun` javaBool, javaInt `Fun` javaBool]

javaInt      = JClass "java.lang.Integer"
javaBool     = JClass "java.lang.Boolean"
zero         = Lit (Src.Integer 0)
one          = Lit (Src.Integer 1)
magicNumber  = Lit (Src.Integer 42)
true         = Lit (Src.Boolean True)
false        = Lit (Src.Boolean False)
x `eq` y     = PrimOp x (Src.Compare J.Equal) y
x `sub` y    = PrimOp x (Src.Arith J.Sub) y
x `mult` y   = PrimOp x (Src.Arith J.Mult) y
