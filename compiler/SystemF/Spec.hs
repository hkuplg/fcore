module SystemF.Spec where

import Test.Hspec
import Language.Java.Syntax as J

import SystemF.Syntax
import SystemF.Parser
import SystemF.Pretty

import qualified TestSuite

main :: IO ()
main = hspec $ do
    describe "instance Pretty (PFTyp Int)" $ do
        it "prettyprints A0" $
            prettyPrintPFTyp (FTVar 0) `shouldBe` "A0"

        it "prettyprints Int" $ 
            prettyPrintPFTyp (PFInt) `shouldBe` "Int"

        it "prettyprints Int -> Int" $
            prettyPrintPFTyp (FFun PFInt PFInt) `shouldBe` "Int -> Int"

        it "prettyprints Int -> Int -> Int" $
            prettyPrintPFTyp (FFun PFInt (FFun PFInt PFInt)) `shouldBe` "Int -> Int -> Int"

        it "prettyprints forall A1. A1" $
            prettyPrintPFTyp (FForall (\a1 -> FTVar a1)) `shouldBe` "forall A1. A1"

        it "prettyprints forall A1. (A1 -> A1) -> (A1 -> A1) -> A1" $
            prettyPrintPFTyp (FForall (\a1 -> (FFun (FFun (FTVar a1) (FTVar a1))) (FFun (FFun (FTVar a1) (FTVar a1)) (FTVar a1))))
                `shouldBe` "forall A1. (A1 -> A1) -> (A1 -> A1) -> A1" 

        it "prettyprints forall A1. forall A2. A2" $
            prettyPrintPFTyp (FForall (\a1 -> (FForall (\a2 -> FTVar a2)))) 
                `shouldBe` "forall A1. forall A2. A2"

        it "prettyprints forall A1. forall A2. (A1 -> A2) -> A1 -> A2" $
            prettyPrintPFTyp (FForall (\a1 -> FForall (\a2 -> 
                        FFun (FFun (FTVar a1) (FTVar a2)) (FFun (FTVar a1) (FTVar a2))))) 
                `shouldBe` "forall A1. forall A2. (A1 -> A2) -> A1 -> A2" 

        it "prettyprints forall A1. forall A2. forall A3. (A2 -> A3) -> (A1 -> A2) -> A1 -> A3" $
            prettyPrintPFTyp (FForall (\a1 -> FForall (\a2 -> FForall (\a3 -> 
                        FFun (FFun (FTVar a2) (FTVar a3)) (FFun (FFun (FTVar a1) (FTVar a2)) (FFun (FTVar a1) (FTVar a3)))))))
                `shouldBe` "forall A1. forall A2. forall A3. (A2 -> A3) -> (A1 -> A2) -> A1 -> A3"

    describe "instance Pretty (PFExp Int Int)" $ do
        it "prettyprints x1" $
            prettyPrintPFExp (FVar 1) `shouldBe` "x1"

        it "prettyprints /\\A1. \\(x1 : A1). x1" $
            prettyPrintPFExp (FBLam (\a1 -> FLam (FTVar a1) (\x1 -> FVar x1))) `shouldBe` "/\\A1. \\(x1 : A1). x1" 

        it "prettyprints loop" $
            prettyPrintPFExp (FFix PFInt (\loop x -> FApp (FVar loop) (FVar x)) PFInt) `shouldBe` "fix x1. \\(x2 : Int). x1 x2 : Int"

        it "prettyprints fact" $
            prettyPrintPFExp 
                (FFix PFInt (\fact n -> 
                    Fif0 (FVar n) 
                    (FLit 1) 
                    (FPrimOp (FVar n) J.Mult (FApp (FVar fact) (FPrimOp (FVar n) J.Sub (FLit 1))))) PFInt
                ) `shouldBe` "fix x1. \\(x2 : Int). if0 x2 then 1 else x2 * x1 (x2 - 1) : Int" 

        it "prettyprints fibo" $
            prettyPrintPFExp 
                (FFix PFInt (\fibo n -> 
                    Fif0 (FPrimOp (FVar n) J.Sub (FLit 2))
                        (FLit 1) 
                        (Fif0 (FPrimOp (FVar n) J.Sub (FLit 1)) 
                            (FLit 1) 
                            (FPrimOp (FApp (FVar fibo) (FPrimOp (FVar n) J.Sub (FLit 1))) 
                                J.Add (FApp (FVar fibo) (FPrimOp (FVar n) J.Sub (FLit 2)))))) PFInt 
                ) `shouldBe` "fix x1. \\(x2 : Int). if0 x2 - 2 then 1 else if0 x2 - 1 then 1 else x1 (x2 - 1) + x1 (x2 - 2) : Int"

        it "prettyprints lambda applications" $
            prettyPrintPFExp (FApp (FLam PFInt (\x -> FVar x)) (FLit 1)) `shouldBe` "(\\(x : Int). x) 1"
