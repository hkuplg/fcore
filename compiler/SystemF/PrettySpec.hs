module SystemF.Spec where

import Test.Hspec
import Language.Java.Syntax as J

import SystemF.Syntax
import SystemF.Pretty

import qualified TestSuite

main :: IO ()
main = hspec $ do
    describe "instance Pretty (PFTyp Int)" $ do
        it "a"  $ prettyPrintPFTyp (FTVar 0)  `shouldBe` "a" 

        it "z"  $ prettyPrintPFTyp (FTVar 25) `shouldBe` "z" 

        it "a0" $ prettyPrintPFTyp (FTVar 26) `shouldBe` "a0" 

        it "Int" $ prettyPrintPFTyp (PFInt) `shouldBe` "Int"

        it "Int -> Int" $ prettyPrintPFTyp (FFun PFInt PFInt) `shouldBe` "Int -> Int"

        it "Int -> Int -> Int" $ prettyPrintPFTyp (FFun PFInt (FFun PFInt PFInt)) `shouldBe` "Int -> Int -> Int"

        it "forall a. a" $ prettyPrintPFTyp (FForall (\a -> FTVar a)) `shouldBe` "forall a. a"

        it "forall a. (a -> a) -> (a -> a) -> a" $
            prettyPrintPFTyp (FForall (\a -> (FFun (FFun (FTVar a) (FTVar a))) (FFun (FFun (FTVar a) (FTVar a)) (FTVar a))))
                `shouldBe` "forall a. (a -> a) -> (a -> a) -> a" 

        it "forall a. forall b. b" $
            prettyPrintPFTyp (FForall (\a1 -> (FForall (\a2 -> FTVar a2)))) 
                `shouldBe` "forall a. forall b. b"

        it "forall a. forall b. (a -> b) -> a -> b" $
            prettyPrintPFTyp (FForall (\a1 -> FForall (\a2 -> 
                        FFun (FFun (FTVar a1) (FTVar a2)) (FFun (FTVar a1) (FTVar a2))))) 
                `shouldBe` "forall a. forall b. (a -> b) -> a -> b" 

        it "forall a. forall b. forall c. (b -> c) -> (a -> b) -> a -> c" $
            prettyPrintPFTyp (FForall (\a1 -> FForall (\a2 -> FForall (\a3 -> 
                        FFun (FFun (FTVar a2) (FTVar a3)) (FFun (FFun (FTVar a1) (FTVar a2)) (FFun (FTVar a1) (FTVar a3)))))))
                `shouldBe` "forall a. forall b. forall c. (b -> c) -> (a -> b) -> a -> c"

    describe "instance Pretty (PFExp Int Int)" $ do
        it "a" $ prettyPrintPFExp (FVar 0) `shouldBe` "a"

        it "z" $ prettyPrintPFExp (FVar 25) `shouldBe` "z"

        it "a0" $ prettyPrintPFExp (FVar 26) `shouldBe` "a0"

        it "1 - (2 - 3)" $
            prettyPrintPFExp (FPrimOp (FLit 1) J.Sub (FPrimOp (FLit 2) J.Sub (FLit 3))) `shouldBe` "1 - (2 - 3)"

        it "/\\a. \\(a : a). a" $
            prettyPrintPFExp (FBLam (\a1 -> FLam (FTVar a1) (\a -> FVar a))) `shouldBe` "/\\a. \\(a : a). a" 

        it "loop" $
            prettyPrintPFExp (FFix PFInt (\loop x -> FApp (FVar loop) (FVar x)) PFInt) `shouldBe` "fix a. \\(b : Int). a b : Int"

        it "fact" $
            prettyPrintPFExp 
                (FFix PFInt (\fact n -> 
                    Fif0 (FVar n) 
                    (FLit 1) 
                    (FPrimOp (FVar n) J.Mult (FApp (FVar fact) (FPrimOp (FVar n) J.Sub (FLit 1))))) PFInt
                ) `shouldBe` "fix a. \\(b : Int). if0 b then 1 else b * a (b - 1) : Int" 

        it "fibo" $
            prettyPrintPFExp 
                (FFix PFInt (\fibo n -> 
                    Fif0 (FPrimOp (FVar n) J.Sub (FLit 2))
                        (FLit 1) 
                        (Fif0 (FPrimOp (FVar n) J.Sub (FLit 1)) 
                            (FLit 1) 
                            (FPrimOp (FApp (FVar fibo) (FPrimOp (FVar n) J.Sub (FLit 1))) 
                                J.Add (FApp (FVar fibo) (FPrimOp (FVar n) J.Sub (FLit 2)))))) PFInt 
                ) `shouldBe` "fix a. \\(b : Int). if0 b - 2 then 1 else if0 b - 1 then 1 else a (b - 1) + a (b - 2) : Int"

        it "lambda applications" $
            prettyPrintPFExp (FApp (FLam PFInt (\x -> FVar x)) (FLit 1)) `shouldBe` "(\\(a : Int). a) 1"
