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

import Text.PrettyPrint.Leijen

import Unsafe.Coerce

import qualified Language.Java.Syntax as J (Op(..))

instance Show (Expr t e) where
  show = show . prettyExpr . unsafeCoerce

instance Show (Type t) where
  show = show . prettyType . unsafeCoerce

-- let tailFact : Int -> Int -> Int
--   = \(acc : Int). \(n : Int). if n == 0 then acc else tailFact (acc * n) (n - 1)
-- in
-- tailFact 1 10
tailFact :: Expr t e
tailFact
  = Fix (\tail_fact acc ->
      Lam "" javaInt (\n ->
        If (Var "" n `eq` zero)
           (Var "" acc)
           (Var "" tail_fact `App` (Var "" acc `mult` Var "" n) `App` (Var "" n `sub` one))))
    javaInt (javaInt `Fun` javaInt)


testTail :: Expr t e
testTail = Fix (\f n -> If (Var "" n `eq` zero)
                           one
                           (Var "" f `App` (Var "" n `sub` one)))
               javaInt
               (javaInt `Fun` javaInt)

fact :: Expr t (Expr t e)
fact = Fix (\f n -> If (Var "" n `eq` zero)
                       one
                       (Var "" n `mult` (Var "" f `App` (Var "" n `sub` one))))
           javaInt
           (javaInt `Fun` javaInt)

tailFactLike :: Expr t e
tailFactLike
  = Fix (\tail_fact acc ->
      Lam "" javaInt (\n ->
        If (Var "" n `eq` zero)
           (Var "" acc)
           (Var "" tail_fact `App` (Var "" acc `mult` one) `App` one)))
    javaInt (javaInt `Fun` javaInt)


plus2 :: Expr t e
plus2 = (App (Lam "" (Fun javaInt (Fun javaInt javaInt))
                  (\e -> (App (App (Var "" e) one) zero)))
             (Lam "" javaInt (\e -> (Lam "" javaInt (\f -> (Var "" e) `mult` (Var "" f))))))

-- let rec
--   even : Int -> Int = \(n : Int). if n == 0 then True  else odd  (n - 1)
--   odd  : Int -> Int = \(n : Int). if n == 0 then False else even (n - 1)
-- in
-- even 42
evenOdd :: Expr t e
evenOdd
  = LetRec
      [(Fun javaInt javaBool), (Fun javaInt javaBool)]
      (\ids ->
         [ Lam "" javaInt (\n -> If (Var "" n `eq` zero) true  (App (Var "" (ids !! 1)) (Var "" n `sub` one)))
         , Lam "" javaInt (\n -> If (Var "" n `eq` zero) false (App (Var "" (ids !! 0)) (Var "" n `sub` one)))])
      (\ids ->
         App (Var "" (ids !! 1)) magicNumber)


-- Int -> (Int -> Bool, Int -> Bool)
evenOddEncodedTy :: Type t
evenOddEncodedTy = javaInt `Fun` Product [javaInt `Fun` javaBool, javaInt `Fun` javaBool]

konstTy :: Type t
konstTy = Forall (\a -> Forall (\b -> Fun (TVar a) (Fun (TVar b) (TVar a))))

callByValue = Lam "" javaInt (\x -> Seq [println (Var "" x), intLit 0])
callByName  = Lam "" (Thunk javaInt) (\x -> Seq [println (Var "" x), intLit 0])
something   = Seq [println (Lit (S.String "called!")), intLit 1]
println x   = JMethod (S.Static "java.lang.System.out") "println" [x] undefined
intLit      = Lit . S.Int

-----------------------
-- peval tests
-----------------------
identity :: Expr t e
identity = Lam "" javaInt (\x -> Var "" x)
id_1 = App identity one

id_twice = Lam "" javaInt (\x -> App identity (Var "" x))
app_id_twice_1 = App id_twice one

--id_4times = Lam "" javaInt (\x -> App id_twice

app_lam_if = App (Lam "" javaInt (\x -> If ((Var "" x) `eq` one)
                                 ((Var "" x) `sub` one)
                                 (zero `sub` one)))
             (zero `sub` one)
app_lam_app = App (Lam "" javaInt (\x -> App (Lam "" javaInt (\y -> (Var "" x) `sub` (Var "" y))) zero))
              one

fix = Fix (\f n -> If (((one `sub` zero) `eq` zero))
                   one
                   (Var "" n `mult` (Var "" f `App` (Var "" n `sub` one))))
      javaInt
      (javaInt `Fun` javaInt)
app_fix = App fix (Lit (S.Int 10))

-- test App e1 e2, where e1 can be partially evaluated to a Lam
minus = Lam "" javaInt (\x -> Lam "" javaInt (\y -> Var "" x `sub` Var "" y))
minus_1 = App minus one
minus_1_0 = App minus_1 zero

if_lam = If (one `eq` one) (App minus one) (App minus zero)
app_if_lam = App if_lam one

-- complex test
complex_eq_zero = If (((App identity minus_1_0) `sub` one) `eq` zero)
                  zero
                  one


-- tests for seval
p1 = Lam "" javaInt (\n -> If ((Var "" n) `lt` ten) negOne one)

p2 = Lam "" javaInt (\n -> If ((Var "" n) `lt` five) negOne one)

prop_p1_p2 = Lam "" javaInt (\n -> (App p1 (Var "" n)) `eq` (App p2 (Var "" n)))


if_bool = Lam "" javaBool (\n -> If (Var "" n `eq` true) negOne one)

inverse = Lam "" javaBool (\_ -> If (true) false true)

intfun = Lam "" (Fun javaInt javaInt) (\n -> If ((App (Var "" n) one) `neq` (App (Var "" n) one)) one negOne)

intfun2 = Lam "" (Fun javaInt javaInt) (\n -> If (false `bAnd` ((App (Var "" n) one) `neq` (App (Var "" n) one))) one negOne)

multiple_if = Lam "" (javaInt) (\n -> If ((Var "" n) `eq` zero) (If ((App identity (Var "" n)) `eq` one) one zero) negOne)

boolfun = Lam "" (Fun javaBool javaBool) (\n -> If ((App (Var "" n) true) `neq` (App (Var "" n) true)) one negOne)

app_bool_fun = App boolfun inverse



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
