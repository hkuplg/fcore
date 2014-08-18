module SystemFISpec where

import Test.Hspec

import SystemFI.Syntax
import SystemFI.TypeCheck

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- /\A. /\B. \(x : A). \(y : B). x
konst :: Expr Int Int
konst = BLam (\a -> BLam (\b ->
          Lam (TVar a) (\x -> Lam (TVar b) (\y -> Var x))))

-- forall A. forall B. A -> B -> A
typeOfKonst :: Type Int
typeOfKonst = Forall (\a -> Forall (\b -> TVar a `Fun` (TVar b `Fun` TVar a)))

-- /\A. (x : A). x
ident :: Expr Int Int
ident = BLam (\a -> Lam (TVar a) (\x -> Var x))

spec :: Spec
spec =
  describe "infer" $ do
    describe "type application" $ do
      mapM_ (\(name, input, expectedOutput) ->
        it ("should infer " ++ name) $ (show . infer) input `shouldBe` expectedOutput)
        [("konst", konst, "forall A. forall B. A -> B -> A")
        ,("konst Int", konst `TApp` Int, "forall B. Int -> B -> Int")
        ,("konst Int (Int & Int)", konst `TApp` Int `TApp` (Int `And` Int), "Int -> (Int & Int) -> Int")
        ,("konst typeOfKonst", konst `TApp` typeOfKonst, "forall B. (forall C. forall D. C -> D -> C) -> B -> (forall C. forall D. C -> D -> C)")
        ,("konst typeOfKonst typeOfKonst", konst `TApp` typeOfKonst `TApp` typeOfKonst, "(forall C. forall D. C -> D -> C) -> (forall C. forall D. C -> D -> C) -> (forall C. forall D. C -> D -> C)")
        ]

    describe "application" $ do
      mapM_ (\(name, input, expectedOutput) ->
        it ("should infer " ++ name) $ (show. infer) input `shouldBe` expectedOutput)
        [("(\\(x : Int). x) (ident ,, 1)", Lam Int (\x -> Var x) `App` (ident `Merge` Lit 1), "Int")]
