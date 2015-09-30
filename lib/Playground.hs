
module Playground where

import           BackEnd (compileN, compileN2)
import qualified ClosureF as C
import           Core
import           Desugar (desugar)
import qualified OptiUtils (Exp(Hide))
import           Parser (reader, P(..))
import           PartialEvaluator
import           PrettyUtils
import           Simplify (simplify)
import qualified Src as S
import           System.Exit
import qualified SystemFI as FI
import           TypeCheck (typeCheck)

import qualified Language.Java.Syntax as J (Op(..))
import           Language.Java.Pretty (prettyPrint)
import           Text.PrettyPrint.ANSI.Leijen
import           Unsafe.Coerce (unsafeCoerce)

import qualified CoreNew as CN
import qualified ClosureFNew as CFN
import qualified ClosureF

instance Show (Expr t e) where
  show = show . prettyExpr . unsafeCoerce

instance Show (Type t) where
  show = show . prettyType . unsafeCoerce

core2java core = do
  -- let newcore = CN.coreExprToNew core
  let (cu,_) = compileN2 "M1" core
  putStrLn $ prettyPrint cu


typeId :: CN.Expr e
typeId = CN.Lam "" CN.Star (\x -> CN.Var "" x)

-- (\ x : ⋆ . (\ y : x . y)) Int 3 + 3
testPoly :: CN.Expr e
testPoly = (CN.App
                (CN.App (CN.Lam "" CN.Star (\x -> CN.Lam "" (CN.Var "" x) (\y -> CN.Var "" y)))
                   (CN.javaInt))
                (CN.Lit (S.Int 3))) `addn` (CN.Lit (S.Int 3))

-- (\x : (\ y : ⋆ . y) Int . x) (castup [(\ y : ⋆ . y) Int] 3)
testCastUp :: CN.Expr e
testCastUp = CN.App (CN.Lam "" (CN.App typeId (CN.JClass "java.lang.Integer")) (\x -> CN.Var "" x))
               (CN.CastUp (CN.App typeId CN.javaInt) (CN.Lit (S.Int 3)))

-- (\e : (\y : ⋆ . x) Int . (\x : Int . x) (castdown e))
testCastDown :: CN.Expr e
testCastDown = CN.Lam "" (CN.App typeId CN.javaInt)
                 (\e ->
                    CN.App (CN.Lam "" CN.javaInt (\x -> CN.Var "" x)) (CN.CastDown CN.javaInt (CN.Var "" e)))

-- let x = Int in (\y : x . y) 3
-- testLet :: CN.Expr e
-- testLet = CN.Let "" CN.javaInt
--             (\x -> CN.App (CN.Lam "" (CN.Var "" x) (\y -> CN.Var "" y)) (CN.Lit (S.Int 3)))

-- pi x : * . x -> x
testPi :: CN.Expr e
testPi = CN.Pi "" (CN.Star) (\x -> CN.Pi "" (CN.Var "" x) (\y -> CN.Var "" x))


m1src = "package P.k module {rec even (n : Int) : Bool = if n == 0 then True  else odd  (n - 1) and odd  (n : Int) : Bool = if n == 0 then False else even (n - 1)} "

m2src = "import a.m; println \"hello\""

{-
module M1 {

rec even (n : Int) : Bool = if n == 0 then True  else odd  (n - 1)
and
odd  (n : Int) : Bool = if n == 0 then False else even (n - 1);

rec fact (n : Int) : Int = ...;

add (n : Int) (m : Int) = n + m

}

-}

-- m1 :: Expr t e
-- m1 = Module "_"
--        (DefRec
--           ["even", "odd"]
--           [ (S.Fun javaIntS javaBoolS, javaInt `Fun` javaBool)
--           , (S.Fun javaIntS javaBoolS, javaInt `Fun` javaBool)
--           ]
--           (\ids ->
--              [ lam javaInt
--                  (\n -> If (var n `eq` zero) true (App (var (ids !! 1)) (var n `sub` one)))
--              , lam javaInt
--                  (\n -> If (var n `eq` zero) false (App (var (ids !! 0)) (var n `sub` one)))
--              ])
--           (\ids ->
--              (Def "f1" (javaIntS `S.Fun` javaIntS) fact
--                 (\f1 ->
--                    Def
--                      "f2"
--                      (javaIntS `S.Fun` javaIntS)
--                      (lam javaInt (\n -> ((var f1 `App` (var n)) `add` one)))
--                      (\f2 -> Null)))))

-- New Core Test

s2n :: String -> IO ()
s2n source
  = case reader source of
      PError msg -> do putStrLn msg
                       exitFailure
      POk parsed -> do
        result <- typeCheck parsed
        case result of
          Left typeError -> exitFailure
          Right (_, checked) ->
            do let fiExpr = desugar checked
               let exp' = OptiUtils.Hide (simplify (FI.HideF fiExpr))
               let exp = rewriteAndEval exp'
               putStrLn "-- Core:"
               print (Core.prettyExpr exp)
               let expcn = CN.coreExprToNew exp
               putStrLn "\n-- New Core:"
               print (CN.pretty expcn)
               putStrLn "\n*****\n-- ClosureF:"
               print (ClosureF.prettyExpr basePrec (0, 0) (ClosureF.fexp2cexp exp))
               putStrLn "\n-- New ClosureF:"
               print (CFN.pretty (CFN.fexp2cexp expcn))
               putStrLn ""

s2Java :: String -> IO ()
s2Java source
  = case reader source of
      PError msg -> do putStrLn msg
                       exitFailure
      POk parsed -> do
        result <- typeCheck parsed
        case result of
          Left typeError -> exitFailure
          Right (_, checked) ->
            do let fiExpr = desugar checked
               let exp' = OptiUtils.Hide (simplify (FI.HideF fiExpr))
               let exp = rewriteAndEval exp'
               let core = CN.coreExprToNew exp
               let (cu,_) = compileN2 "test" core
               putStrLn $ prettyPrint cu

testnc :: IO ()
testnc =
  do
    -- s2n "let rec fact (n : Int) : Int = if n == 0 then 1 else n * fact (n - 1); fact 5"
    s2n "let id[A] (x : A) = x; id [forall A. A -> A] id 10"
    s2n "let get [D, E, F] (x : E) (y : D) : D = y; get [Int, String, Int] \"abc\" 5"
    return ()

testB :: String
testB = "(/\\B -> \\(f : B) -> f) [Int] 3"

testA :: String
testA = "if True then 1 else 2"

-- javaIntS = (S.JType (S.JClass "java.lang.Integer"))
-- javaBoolS = (S.JType (S.JClass "java.lang.Boolean"))


tailFact :: Expr t e
tailFact
  = fix (\tail_fact acc ->
      lam javaInt (\n ->
        If (var n `eq` zero)
           (var acc)
           (var tail_fact `App` (var acc `mult` var n) `App` (var n `sub` one))))
    javaInt (javaInt `Fun` javaInt)

testTail :: Expr t e
testTail = App (fix (\f n -> If (var n `eq` zero)
                           one
                           (var f `App` (var n `sub` one)))
               javaInt
               (javaInt `Fun` javaInt)) one

fact :: Expr t e
fact = App (fix (\f n -> If (var n `eq` zero)
                       one
                       (var n `mult` (var f `App` (var n `sub` one))))
           javaInt
           javaInt) one

testJNew :: String
testJNew = "let x = new java.lang.Integer(10) in x"

testJMethod = "let x = new java.lang.Integer(10) in x.toString()"

-- testLet :: String
-- testLet = "let id[A] (x : A) = x in id[Int] 5 + 1"

test1 :: Expr t e
test1 =
  lam javaInt (\f ->
                lam javaInt (\g -> App (var f)
                                   (lam javaInt (\x -> Let "" (App (var g) (var x)) (\t -> var t)))))

tailFactLike :: Expr t e
tailFactLike
  = fix (\tail_fact acc ->
      lam javaInt (\n ->
                    If (var n `eq` zero)
                    (var acc)
                    (var tail_fact `App` (var acc `mult` one) `App` one)))
    javaInt (javaInt `Fun` javaInt)


plus2 :: Expr t e
plus2 = (App (lam (Fun javaInt (Fun javaInt javaInt)) (\e -> (App (App (var e) one) zero)))
             (lam javaInt (\e -> (lam javaInt (\f -> (var e) `mult` (var f))))))

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


javaBool     = JClass "java.lang.Boolean"
zero         = Lit (S.Int 0)
one          = Lit (S.Int 1)
five         = Lit (S.Int 5)
ten          = Lit (S.Int 10)
negOne       = Lit (S.Int (-1))
magicNumber  = Lit (S.Int 42)
true         = Lit (S.Bool True)
false        = Lit (S.Bool False)
x `eq` y     = PrimOp x (S.Compare J.Equal) y
x `neq` y    = PrimOp x (S.Compare J.NotEq) y
x `lt` y     = PrimOp x (S.Compare J.LThan) y
x `bAnd` y    = PrimOp x (S.Logic J.And) y
x `add` y    = PrimOp x (S.Arith J.Add) y
x `sub` y    = PrimOp x (S.Arith J.Sub) y
x `mult` y   = PrimOp x (S.Arith J.Mult) y

x `addn` y    = CN.PrimOp x (S.Arith J.Add) y

mconst =
  (bLam (\a ->
    lam (tVar a) (\x ->
       lam (tVar a) (\y ->
          var x
       )
    )
  ))

notail2 =
  bLam (\a ->
    lam (Fun (tVar a) (Fun (tVar a) (tVar a))) (\f ->
      lam (tVar a) (\x ->
        lam (tVar a) (\y ->
          App (App (var f) (var x)) (App (App (var f) (var y)) (var y)) ))))

program2 = App (App (App (TApp notail2 (JClass "java.lang.Integer")) (TApp mconst (JClass "java.lang.Integer"))) (Lit (S.Int 5))) (Lit (S.Int 6))

notail4 =
  bLam (\a ->
    lam ( Fun (Fun (tVar a) (tVar a)) (Fun (Fun (tVar a) (tVar a)) (tVar a))) (\g ->
      lam (Fun (tVar a) (Fun (tVar a) (tVar a))) (\f ->
        lam (tVar a) (\x ->
          lam (tVar a) (\y ->
            App (App (var g) (App (var f) (var x))) (App (var f) (var y)))))))

summa =
    lam (Fun (JClass "java.lang.Integer") (JClass "java.lang.Integer")) (\x ->
       lam (Fun (JClass "java.lang.Integer") (JClass "java.lang.Integer")) (\y ->
          PrimOp (App (var x) (Lit (S.Int 0))) (S.Arith J.Add) (App (var y) (Lit (S.Int 0)))
       )
    )

program4 = App (App (App (App (TApp notail4 (JClass "java.lang.Integer")) summa) (TApp mconst (JClass "java.lang.Integer"))) (Lit (S.Int 5))) (Lit (S.Int 6))
