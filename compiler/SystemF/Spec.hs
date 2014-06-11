module SystemF.Spec where

import SystemF.Syntax
import SystemF.Pretty

import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "instance Pretty (PFTyp Int)" $ do
        it "prettyprints A0" $
            prettyPrint (FTVar 0 :: PFTyp Int) `shouldBe` "A0"

        it "prettyprints Int" $ 
            prettyPrint (PFInt :: PFTyp Int) `shouldBe` "Int"

        it "prettyprints Int -> Int" $
            prettyPrint (FFun PFInt PFInt :: PFTyp Int) `shouldBe` "Int -> Int"

        it "prettyprints Int -> Int -> Int" $
            prettyPrint (FFun PFInt (FFun PFInt PFInt) :: PFTyp Int) `shouldBe` "Int -> Int -> Int"

        it "prettyprints forall A1. A1" $
            prettyPrint (FForall (\a1 -> FTVar a1) :: PFTyp Int) `shouldBe` "forall A1. A1"

        it "prettyprints forall A1. (A1 -> A1) -> (A1 -> A1) -> A1" $
            prettyPrint (FForall (\a1 -> (FFun (FFun (FTVar a1) (FTVar a1))) (FFun (FFun (FTVar a1) (FTVar a1)) (FTVar a1))) :: PFTyp Int)
                `shouldBe` "forall A1. (A1 -> A1) -> (A1 -> A1) -> A1" 

        it "prettyprints forall A1. forall A2. A2" $
            prettyPrint (FForall (\a1 -> (FForall (\a2 -> FTVar a2))) :: PFTyp Int) 
                `shouldBe` "forall A1. forall A2. A2"

        it "prettyprints forall A1. forall A2. (A1 -> A2) -> A1 -> A2" $
            prettyPrint (FForall (\a1 -> FForall (\a2 -> 
                        FFun (FFun (FTVar a1) (FTVar a2)) (FFun (FTVar a1) (FTVar a2)))) :: PFTyp Int) 
                `shouldBe` "forall A1. forall A2. (A1 -> A2) -> A1 -> A2" 

        it "prettyprints forall A1. forall A2. forall A3. (A2 -> A3) -> (A1 -> A2) -> A1 -> A3" $
            prettyPrint (FForall (\a1 -> FForall (\a2 -> FForall (\a3 -> 
                        FFun (FFun (FTVar a2) (FTVar a3)) (FFun (FFun (FTVar a1) (FTVar a2)) (FFun (FTVar a1) (FTVar a3)))))) :: PFTyp Int)
                `shouldBe` "forall A1. forall A2. forall A3. (A2 -> A3) -> (A1 -> A2) -> A1 -> A3"

    describe "instance Pretty (PFExp Int Int)" $ do
        it "prettyprints x1" $
            prettyPrint (FVar 1 :: PFExp Int Int) `shouldBe` "x1"

        it "prettyprints /\\A1. \\(x1 : A1). x1" $
            prettyPrint (FBLam (\a1 -> FLam (FTVar a1) (\x1 -> FVar x1)) :: PFExp Int Int) `shouldBe` "/\\A1. \\(x1 : A1). x1" 
