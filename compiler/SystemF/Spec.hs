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
        it "prettyprints a" $
            prettyPrintPFTyp (FTVar 0) `shouldBe` "a"

        it "prettyprints Int" $ 
            prettyPrintPFTyp (PFInt) `shouldBe` "Int"

        it "prettyprints Int -> Int" $
            prettyPrintPFTyp (FFun PFInt PFInt) `shouldBe` "Int -> Int"

        it "prettyprints Int -> Int -> Int" $
            prettyPrintPFTyp (FFun PFInt (FFun PFInt PFInt)) `shouldBe` "Int -> Int -> Int"

        it "prettyprints forall a. a" $
            prettyPrintPFTyp (FForall (\a -> FTVar a)) `shouldBe` "forall a. a"

        it "prettyprints forall a. (a -> a) -> (a -> a) -> a" $
            prettyPrintPFTyp (FForall (\a -> (FFun (FFun (FTVar a) (FTVar a))) (FFun (FFun (FTVar a) (FTVar a)) (FTVar a))))
                `shouldBe` "forall a. (a -> a) -> (a -> a) -> a" 

        it "prettyprints forall a. forall b. b" $
            prettyPrintPFTyp (FForall (\a1 -> (FForall (\a2 -> FTVar a2)))) 
                `shouldBe` "forall a. forall b. b"

        it "prettyprints forall a. forall b. (a -> b) -> a -> b" $
            prettyPrintPFTyp (FForall (\a1 -> FForall (\a2 -> 
                        FFun (FFun (FTVar a1) (FTVar a2)) (FFun (FTVar a1) (FTVar a2))))) 
                `shouldBe` "forall a. forall b. (a -> b) -> a -> b" 

        it "prettyprints forall a. forall b. forall c. (b -> c) -> (a -> b) -> a -> c" $
            prettyPrintPFTyp (FForall (\a1 -> FForall (\a2 -> FForall (\a3 -> 
                        FFun (FFun (FTVar a2) (FTVar a3)) (FFun (FFun (FTVar a1) (FTVar a2)) (FFun (FTVar a1) (FTVar a3)))))))
                `shouldBe` "forall a. forall b. forall c. (b -> c) -> (a -> b) -> a -> c"

    describe "instance Pretty (PFExp Int Int)" $ do
        it "prettyprints a" $
            prettyPrintPFExp (FVar 0) `shouldBe` "a"

        it "prettyprints /\\a. \\(a : a). a" $
            prettyPrintPFExp (FBLam (\a1 -> FLam (FTVar a1) (\a -> FVar a))) `shouldBe` "/\\a. \\(a : a). a" 

        it "prettyprints loop" $
            prettyPrintPFExp (FFix PFInt (\loop x -> FApp (FVar loop) (FVar x)) PFInt) `shouldBe` "fix a. \\(b : Int). a b : Int"

        it "prettyprints fact" $
            prettyPrintPFExp 
                (FFix PFInt (\fact n -> 
                    Fif0 (FVar n) 
                    (FLit 1) 
                    (FPrimOp (FVar n) J.Mult (FApp (FVar fact) (FPrimOp (FVar n) J.Sub (FLit 1))))) PFInt
                ) `shouldBe` "fix a. \\(b : Int). if0 b then 1 else b * a (b - 1) : Int" 

        it "prettyprints fibo" $
            prettyPrintPFExp 
                (FFix PFInt (\fibo n -> 
                    Fif0 (FPrimOp (FVar n) J.Sub (FLit 2))
                        (FLit 1) 
                        (Fif0 (FPrimOp (FVar n) J.Sub (FLit 1)) 
                            (FLit 1) 
                            (FPrimOp (FApp (FVar fibo) (FPrimOp (FVar n) J.Sub (FLit 1))) 
                                J.Add (FApp (FVar fibo) (FPrimOp (FVar n) J.Sub (FLit 2)))))) PFInt 
                ) `shouldBe` "fix a. \\(b : Int). if0 b - 2 then 1 else if0 b - 1 then 1 else a (b - 1) + a (b - 2) : Int"

        it "prettyprints lambda applications" $
            prettyPrintPFExp (FApp (FLam PFInt (\x -> FVar x)) (FLit 1)) `shouldBe` "(\\(a : Int). a) 1"
