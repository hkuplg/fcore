{-# OPTIONS_GHC
    -fno-warn-missing-signatures
    -fno-warn-orphans
    -fno-warn-unused-imports #-}

module Examples where

import           Core
import           Desugar (desugar)
import           Inliner (inliner)
import           Parser (reader)
import           PartialEvaluator (peval)
import           Simplify (simplify)
import qualified Src as S
import           System.Directory (getCurrentDirectory)
import           TypeCheck (typeCheck)
import           Unsafe.Coerce (unsafeCoerce)

import qualified Language.Java.Syntax as J (Op(..))

instance Show (Expr t e) where
  show = show . prettyExpr . unsafeCoerce

instance Show (Type t) where
  show = show . prettyType . unsafeCoerce


tailFact :: Expr t e
tailFact
  = Fix (\tail_fact acc ->
      lam javaInt (\n ->
        If (var n `eq` zero)
           (var acc)
           (var tail_fact `App` (var acc `mult` var n) `App` (var n `sub` one))))
    javaInt (javaInt `Fun` javaInt)

testTail :: Expr t e
testTail = Fix (\f n -> If (var n `eq` zero)
                           one
                           (var f `App` (var n `sub` one)))
               javaInt
               (javaInt `Fun` javaInt)

fact :: Expr t (Expr t e)
fact = Fix (\f n -> If (var n `eq` zero)
                       one
                       (var n `mult` (var f `App` (var n `sub` one))))
           javaInt
           (javaInt `Fun` javaInt)

tailFactLike :: Expr t e
tailFactLike
  = Fix (\tail_fact acc ->
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

sf2c :: Int -> String -> IO (Expr t e)
sf2c n fname = do
  path <- {-"/Users/weixin/Project/systemfcompiler/compiler/"-} getCurrentDirectory
  string <- readFile (path ++ "/" ++ fname)
  result <- typeCheck . reader $ string
  case result of
   Left typeError -> error $ show typeError
   Right (tcheckedSrc, _) ->
     case n of
      1 -> return (peval . simplify . desugar $ tcheckedSrc)
      2 -> return (simplify . desugar $ tcheckedSrc)
      3 -> return (desugar $ tcheckedSrc)
      _ -> return (peval . desugar $ tcheckedSrc)
