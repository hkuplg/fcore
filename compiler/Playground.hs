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
import           System.Directory
import           Translations
import           TypeCheck (typeCheck)
-- import SymbolicEvaluator
-- import Z3Backend

import           PrettyUtils
import           OptiUtils

import           Text.PrettyPrint.Leijen

import           Unsafe.Coerce

import qualified Language.Java.Syntax as J (Op(..))

instance Show (Expr t e) where
  show = show . prettyExpr . unsafeCoerce

instance Show (Type t) where
  show = show . prettyType . unsafeCoerce


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

evenOdd :: Expr t e
evenOdd
  = LetRec
      [(Fun javaInt javaBool), (Fun javaInt javaBool)]
      (\ids ->
         [ Lam "" javaInt (\n -> If (Var "" n `eq` zero) true  (App (Var "" (ids !! 1)) (Var "" n `sub` one)))
         , Lam "" javaInt (\n -> If (Var "" n `eq` zero) false (App (Var "" (ids !! 0)) (Var "" n `sub` one)))])
      (\ids ->
         App (Var "" (ids !! 1)) magicNumber)


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

sf2core :: Int -> String -> IO (Expr t e)
sf2core n fname = do
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

fCore f str = sf2core str >>= (\e -> return $ f e)
