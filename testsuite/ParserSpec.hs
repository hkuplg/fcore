module ParserSpec where

import Control.Monad    (forM_)
import Test.Hspec

import Src
import Parser

import qualified Language.Java.Syntax as J (Op(..))

testCases :: [(String, ReaderExpr)]
testCases =
    [ ("((1,2),3)._0._1", Proj (Proj (Tuple [Tuple [Lit (Int 1), Lit (Int 2)], Lit (Int 3)]) 0) 1)
    , ("1 / 2 / (3 / 4)", PrimOp
                            (PrimOp (Lit (Int 1)) (Arith J.Div) (Lit (Int 2)))
                            (Arith J.Div)
                            (PrimOp (Lit (Int 3)) (Arith J.Div) (Lit (Int 4))))
    , ("1 2 3 4", App (App (App (Lit (Int 1)) (Lit (Int 2))) (Lit (Int 3))) (Lit (Int 4)))
    , ("1 2 (3 4)", App (App (Lit (Int 1)) (Lit (Int 2))) (App (Lit (Int 3)) (Lit (Int 4))))
    , ("1 (2 3) 4", App (App (Lit (Int 1)) (App (Lit (Int 2)) (Lit (Int 3)))) (Lit (Int 4)))
    , ("1 (2 3 4)", App (Lit (Int 1)) (App (App (Lit (Int 2)) (Lit (Int 3))) (Lit (Int 4))))
    , ("1 (2 (3 4))", App (Lit (Int 1)) (App (Lit (Int 2)) (App (Lit (Int 3)) (Lit (Int 4)))))
    , ("if 1 == 0 then 2 else 3 + 4", If (PrimOp (Lit (Int 1)) (Compare J.Equal) (Lit (Int 0))) (Lit (Int 2)) (PrimOp (Lit (Int 3)) (Arith J.Add) (Lit (Int 4))))
    , ("True", Lit (Bool True))
    , ("False", Lit (Bool False))
    , ("True && True", PrimOp (Lit (Bool True)) (Logic J.CAnd) (Lit (Bool True)))
    , ("False || True", PrimOp (Lit (Bool False)) (Logic J.COr) (Lit (Bool True)))
    , ("\"hello\"", Lit (String "hello"))
    ]

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "reader" $
    forM_ testCases (\(concr, abstr) ->
      it ("should parse \"" ++ concr ++ "\"") $
        reader concr `shouldBe` abstr)
