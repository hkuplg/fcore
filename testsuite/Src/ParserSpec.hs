module Src.ParserSpec where

import Control.Monad    (forM_)
import Test.Hspec

import Src.Syntax
import Src.Parser

import qualified Language.Java.Syntax as J (Op(..))

testCases :: [(String, Expr String)]
testCases =
    [ ("((1,2),3)._0._1", Proj (Proj (Tuple [Tuple [Lit (Integer 1), Lit (Integer 2)], Lit (Integer 3)]) 0) 1)
    , ("1 / 2 / (3 / 4)", PrimOp
                            (PrimOp (Lit (Integer 1)) (Arith J.Div) (Lit (Integer 2)))
                            (Arith J.Div)
                            (PrimOp (Lit (Integer 3)) (Arith J.Div) (Lit (Integer 4))))
    , ("1 2 3 4", App (App (App (Lit (Integer 1)) (Lit (Integer 2))) (Lit (Integer 3))) (Lit (Integer 4)))
    , ("1 2 (3 4)", App (App (Lit (Integer 1)) (Lit (Integer 2))) (App (Lit (Integer 3)) (Lit (Integer 4))))
    , ("1 (2 3) 4", App (App (Lit (Integer 1)) (App (Lit (Integer 2)) (Lit (Integer 3)))) (Lit (Integer 4)))
    , ("1 (2 3 4)", App (Lit (Integer 1)) (App (App (Lit (Integer 2)) (Lit (Integer 3))) (Lit (Integer 4))))
    , ("1 (2 (3 4))", App (Lit (Integer 1)) (App (Lit (Integer 2)) (App (Lit (Integer 3)) (Lit (Integer 4)))))
    , ("if 1 == 0 then 2 else 3 + 4", If (PrimOp (Lit (Integer 1)) (Compare J.Equal) (Lit (Integer 0))) (Lit (Integer 2)) (PrimOp (Lit (Integer 3)) (Arith J.Add) (Lit (Integer 4))))
    , ("true", Lit (Boolean True))
    , ("false", Lit (Boolean False))
    , ("true && true", PrimOp (Lit (Boolean True)) (Logic J.CAnd) (Lit (Boolean True)))
    , ("false || true", PrimOp (Lit (Boolean False)) (Logic J.COr) (Lit (Boolean True)))
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
