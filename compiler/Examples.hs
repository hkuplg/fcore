{-# OPTIONS_GHC
    -fno-warn-missing-signatures
    -fno-warn-orphans
    -fno-warn-unused-imports #-}

module Examples where

import qualified Src as S
import Core
import Simplify
import PartialEvaluator
import Inliner
import Translations
import SymbolicEvaluator
import Z3Backend

import PrettyUtils
import OptiUtils

import Text.PrettyPrint.ANSI.Leijen

import Unsafe.Coerce
import qualified SystemFI as FI

import qualified Language.Java.Syntax as J (Op(..))

instance Show (Expr t e) where
  show = show . prettyExpr . unsafeCoerce

instance Show (Type t) where
  show = show . prettyType . unsafeCoerce

instance Show Value where
    show = show . pretty

instance Show ExecutionTree where
    show = show . pretty

-- let tailFact : Int -> Int -> Int
--   = \(acc : Int). \(n : Int). if n == 0 then acc else tailFact (acc * n) (n - 1)
-- in
-- tailFact 1 10
tailFact :: Expr t e
tailFact
  = fix (\tail_fact acc ->
      lam javaInt (\n ->
        If (var n `eq` zero)
           (var acc)
           (var tail_fact `App` (var acc `mult` var n) `App` (var n `sub` one))))
    javaInt (javaInt `Fun` javaInt)


testTail :: Expr t e
testTail = fix (\f n -> If (var n `eq` zero)
                           one
                           (var f `App` (var n `sub` one)))
               javaInt
               (javaInt `Fun` javaInt)

fact :: Expr t (Expr t e)
fact = fix (\f n -> If (var n `eq` zero)
                       one
                       (var n `mult` (var f `App` (var n `sub` one))))
           javaInt
           (javaInt `Fun` javaInt)

tailFactLike :: Expr t e
tailFactLike
  = fix (\tail_fact acc ->
      lam javaInt (\n ->
        If (var n `eq` zero)
           (var acc)
           (var tail_fact `App` (var acc `mult` one) `App` one)))
    javaInt (javaInt `Fun` javaInt)


plus2 :: Expr t e
plus2 = (App (lam (Fun javaInt (Fun javaInt javaInt))
                  (\e -> (App (App (var e) one) zero)))
             (lam javaInt (\e -> (lam javaInt (\f -> (var e) `mult` (var f))))))

-- let rec
--   even : Int -> Int = \(n : Int). if n == 0 then True  else odd  (n - 1)
--   odd  : Int -> Int = \(n : Int). if n == 0 then False else even (n - 1)
-- in
-- even 42
evenOdd :: Expr t e
evenOdd
  = LetRec
      ["even", "odd"]
      [(Fun javaInt javaBool), (Fun javaInt javaBool)]
      (\ids ->
         [ lam javaInt (\n -> If (var n `eq` zero) true  (App (var (ids !! 1)) (var n `sub` one)))
         , lam javaInt (\n -> If (var n `eq` zero) false (App (var (ids !! 0)) (var n `sub` one)))])
      (\ids ->
         App (var (ids !! 1)) magicNumber)


-- Int -> (Int -> Bool, Int -> Bool)
evenOddEncodedTy :: Type t
evenOddEncodedTy = javaInt `Fun` Product [javaInt `Fun` javaBool, javaInt `Fun` javaBool]

-- konstTy :: Type t
-- konstTy = Forall "konst" (\a -> Forall (\b -> Fun (tVar a) (Fun (tVar b) (tVar a))))

callByValue = lam javaInt (\x -> Seq [println (var x), intLit 0])
callByName  = lam (Fun Unit javaInt) (\x -> Seq [println (var x), intLit 0])
something   = Seq [println (Lit (S.String "called!")), intLit 1]
println x   = JMethod (S.Static "java.lang.System.out") "println" [x] undefined
intLit      = Lit . S.Int

-----------------------
-- peval tests
-----------------------
identity :: Expr t e
identity = lam javaInt (\x -> var x)
id_1 = App identity one

id_twice = lam javaInt (\x -> App identity (var x))
app_id_twice_1 = App id_twice one

--id_4times = lam javaInt (\x -> App id_twice

app_lam_if = App (lam javaInt (\x -> If ((var x) `eq` one)
                                 ((var x) `sub` one)
                                 (zero `sub` one)))
             (zero `sub` one)
app_lam_app = App (lam javaInt (\x -> App (lam javaInt (\y -> (var x) `sub` (var y))) zero))
              one

fix' = fix (\f n -> If (((one `sub` zero) `eq` zero))
                   one
                   (var n `mult` (var f `App` (var n `sub` one))))
      javaInt
      (javaInt `Fun` javaInt)
app_fix = App fix' (Lit (S.Int 10))

-- test App e1 e2, where e1 can be partially evaluated to a Lam
minus = lam javaInt (\x -> lam javaInt (\y -> var x `sub` var y))
minus_1 = App minus one
minus_1_0 = App minus_1 zero

if_lam = If (one `eq` one) (App minus one) (App minus zero)
app_if_lam = App if_lam one

-- complex test
complex_eq_zero = If (((App identity minus_1_0) `sub` one) `eq` zero)
                  zero
                  one


-- tests for seval
p1 = lam javaInt (\n -> If ((var n) `lt` ten) negOne one)

p2 = lam javaInt (\n -> If ((var n) `lt` five) negOne one)

prop_p1_p2 = lam javaInt (\n -> (App p1 (var n)) `eq` (App p2 (var n)))


if_bool = lam javaBool (\n -> If (var n `eq` true) negOne one)

inverse = lam javaBool (\_ -> If (true) false true)

intfun = lam (Fun javaInt javaInt) (\n -> If ((App (var n) one) `neq` (App (var n) one)) one negOne)

intfun2 = lam (Fun javaInt javaInt) (\n -> If (false `bAnd` ((App (var n) one) `neq` (App (var n) one))) one negOne)

multiple_if = lam (javaInt) (\n -> If ((var n) `eq` zero) (If ((App identity (var n)) `eq` one) one zero) negOne)

boolfun = lam (Fun javaBool javaBool) (\n -> If ((App (var n) true) `neq` (App (var n) true)) one negOne)

app_bool_fun = App boolfun inverse

-- interface to symbolic evaluator
se str = src2fi str >>= solve

javaBool     = JClass "java.lang.Boolean"
zero         = Lit (S.Int 0)
one          = Lit (S.Int 1)
two          = Lit (S.Int 2)
five         = Lit (S.Int 5)
ten          = Lit (S.Int 10)
negOne       = Lit (S.Int (-1))
magicNumber  = Lit (S.Int 42)
true         = Lit (S.Bool True)
false        = Lit (S.Bool False)
x `eq` y     = PrimOp x (S.Compare J.Equal) y
x `neq` y    = PrimOp x (S.Compare J.NotEq) y
x `lt` y     = PrimOp x (S.Compare J.LThan) y
x `gt` y     = PrimOp x (S.Compare J.GThan) y
x `bAnd` y    = PrimOp x (S.Logic J.And) y
x `add` y    = PrimOp x (S.Arith J.Add) y
x `sub` y    = PrimOp x (S.Arith J.Sub) y
x `mult` y   = PrimOp x (S.Arith J.Mult) y
