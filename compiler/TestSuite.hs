module TestSuite where

import Test.Hspec
import SystemF.Syntax
import Language.Java.Syntax as J
import Prelude hiding (const)
-- import qualified HM
-- import HMParser         (readHM)
import qualified SystemF.Parser
import BaseTransCFJava (createCU)
import Language.Java.Pretty
import MonadLib
import System.Process
import System.Directory

import Translations
import BaseTransCFJava
import ApplyTransCFJava
import Data.Map
import qualified Data.Set as Set
import Data.List
import ClosureF
import Inheritance

-- setting
type AOptType = StateT Int (StateT (Map J.Exp Int) (Writer Bool)) 

aoptinst :: ApplyOptTranslate AOptType  -- instantiation; all coinstraints resolved
aoptinst = applyopt

translateAO ::  PCExp Int (Var, PCTyp Int) -> AOptType ([BlockStmt], Exp, PCTyp Int)
translateAO e = translateM (up aoptinst) e

compileAO ::  PFExp Int (Var, PCTyp Int) -> (Block, Exp, PCTyp Int)
compileAO e = 
  case fst $ runWriter $ (evalStateT (evalStateT (translateAO (fexp2cexp e)) 0) empty) of
      (ss,exp,t) -> (J.Block ss,exp, t)


type NType = StateT Int (State (Map J.Exp Int)) 
ninst :: Translate NType  -- instantiation; all coinstraints resolved
ninst = naive

translateN ::  PCExp Int (Var, PCTyp Int) -> NType ([BlockStmt], Exp, PCTyp Int)
translateN e = translateM (up ninst) e

compileN ::  PFExp Int (Var, PCTyp Int) -> (Block, Exp, PCTyp Int)
compileN e = 
  case evalState (evalStateT (translateN (fexp2cexp e)) 0) empty of
      (ss,exp,t) -> (J.Block ss,exp, t)

-- java compilation + run
compileAndRun compileF exp = do let source = prettyPrint (fst $ createCU (compileF exp) Nothing)
                                writeFile "Main.java" source
                                readProcess "javac" ["Main.java"] ""
                                result <- readProcess "java" ["Main"] ""
                                readProcess "rm" ["Main.java"] ""
                                x <- getDirectoryContents "."
                                readProcess "rm" [y | y<- x, ".class" `isSuffixOf` y] ""
                                return result

-- Some test terms

-- mu loop. \x -> loop x
loopStr = "fix loop. \\(x : Int). loop x : Int -> Int"
loop = FFix PFInt (\loop x -> FApp (FVar loop) (FVar x)) PFInt

factStr = "fix fact. \\(n : Int). if0 n then 1 else n * fact (n-1) : Int"
fact = FFix PFInt (\fact n -> 
   Fif0  (FVar n) 
         (FLit 1) 
         (FPrimOp (FVar n) J.Mult (FApp (FVar fact) (FPrimOp (FVar n) J.Sub (FLit 1))))) PFInt
       
tfact = FFix PFInt (\fact n -> FLam PFInt (\acc ->
   Fif0  (FVar n) 
         (FVar acc) 
         (FApp (FApp (FVar fact) (FPrimOp (FVar n) J.Sub (FLit 1))) (FPrimOp (FVar n) J.Mult (FVar acc))))) (FFun PFInt PFInt)

fiboStr = "fix fibo. \\(n : Int). if0 n then 1 else (fibo (n-1)) + (fibo (n-2)) : Int"
fibo = FFix PFInt (\fibo n -> 
   Fif0  (FPrimOp (FVar n) J.Sub (FLit 2))
         (FLit 1) 
         (Fif0  (FPrimOp (FVar n) J.Sub (FLit 1)) 
               (FLit 1) 
               (FPrimOp (FApp (FVar fibo) (FPrimOp (FVar n) J.Sub (FLit 1))) J.Add (FApp (FVar fibo) (FPrimOp (FVar n) J.Sub (FLit 2)))))) PFInt
      
fact_app = FApp fact (FLit 10)

fibo_app = FApp fibo (FLit 10)
-- /\A. \(x:A) . x

idF1Str = "/\\A. \\(x:A). x"
idF = FBLam (\a -> FLam (FTVar a) (\x -> FVar x))

-- /\A . (\(f : A -> A) . \(x : A) . f x) (idF A)

idF2Str = "/\\A. (\\(f : A -> A). \\(x : A). f x) (idF A)"
idF2 = FBLam (\a -> FApp (FLam (FFun (FTVar a) (FTVar a)) (\f -> FLam (FTVar a) (\x -> FApp (FVar f) (FVar x)))) (FTApp idF (FTVar a)))

-- /\A . \(x:A) . (idF A) x

idF3Str = "/\\A . \\(x:A) . (idF A) x"
idF3 = FBLam (\a -> FLam (FTVar a) (\x -> FApp (FTApp idF (FTVar a)) (FVar x) ))

notailStr = "/\\A. \\(f : A -> (A -> A)). \\(g : A -> A). \\(x : A). (f x) (g x)"
notail =
  FBLam (\a ->
    FLam (FFun (FTVar a) (FFun (FTVar a) (FTVar a))) (\f ->
      FLam (FFun (FTVar a) (FTVar a)) (\g ->
        FLam (FTVar a) (\x ->
          FApp (FApp (FVar f) (FVar x)) (FApp (FVar g) (FVar x)) ))))

constStr = "/\\A . \\(x : A) . \\(y : A) . x"
const =
  FBLam (\a ->
    FLam (FTVar a) (\x ->
       FLam (FTVar a) (\y ->
          FVar x
       )
    )
  )
  
-- /\A . \(x : A) . (/\A . \(f : A -> A -> A) . \(g : A -> A) . \(x : A) . f x (g x)) A (const A) (idF A) x
-- /\A . \(x : A) . notail A (const A) (idF A) x
program1 =
  FBLam (\a ->
    FLam (FTVar a) (\x ->
       FApp (FApp (FApp (FTApp notail (FTVar a)) (FTApp const (FTVar a))) (FTApp idF (FTVar a))) (FVar x)
    )
  )

program1Num = FApp (FTApp program1 PFInt) (FLit 5)

-- should infer (forall (x0 : int) . int)
intapp = FTApp idF PFInt


notail2Str = "/\\A. \\(f : A -> (A -> A)). \\(x : A). \\(y : A). (f x) ((f y) y)"
notail2 =
  FBLam (\a ->
    FLam (FFun (FTVar a) (FFun (FTVar a) (FTVar a))) (\f ->
      FLam (FTVar a) (\x ->
        FLam (FTVar a) (\y ->
          FApp (FApp (FVar f) (FVar x)) (FApp (FApp (FVar f) (FVar y)) (FVar y)) ))))
  

program2 = FApp (FApp (FApp (FTApp notail2 PFInt) (FTApp const PFInt)) (FLit 5)) (FLit 6)
                  
idfNum = FApp (FTApp idF PFInt) (FLit 10)

constNum = FApp (FApp (FTApp const PFInt) (FLit 10)) (FLit 20)

notail3Str = "/\\A. \\(f : A -> (A -> A)). \\(g : A -> (A -> A)). \\(x : A). \\(y : A). (f x) ((g y) y)"
notail3 =
  FBLam (\a ->
    FLam (FFun (FTVar a) (FFun (FTVar a) (FTVar a))) (\f ->
      FLam (FFun (FTVar a) (FFun (FTVar a) (FTVar a))) (\g ->
        FLam (FTVar a) (\x ->
          FLam (FTVar a) (\y ->
            FApp (FApp (FVar f) (FVar x)) (FApp (FApp (FVar g) (FVar y)) (FVar y)) )))))

program3 = FApp (FApp (FApp (FApp (FTApp notail3 PFInt) (FTApp const PFInt)) (FTApp const PFInt)) (FLit 5)) (FLit 6)
    
notail4Str = "/\\A. \\(g : ((A -> A) -> (A -> A)) -> A). \\(f : A -> (A -> A)). \\(x : A). \\(y : A). (g (f x)) (f y)"
notail4 =
  FBLam (\a ->
    FLam ( FFun (FFun (FTVar a) (FTVar a)) (FFun (FFun (FTVar a) (FTVar a)) (FTVar a))) (\g ->
      FLam (FFun (FTVar a) (FFun (FTVar a) (FTVar a))) (\f ->
        FLam (FTVar a) (\x ->
          FLam (FTVar a) (\y ->
            FApp (FApp (FVar g) (FApp (FVar f) (FVar x))) (FApp (FVar f) (FVar y)))))))

summaStr= "\\(x : Int -> Int). \\(y : Int -> Int). (x 0) + (y 0)"
summa =
    FLam (FFun PFInt PFInt) (\x ->
       FLam (FFun PFInt PFInt) (\y ->
          FPrimOp (FApp (FVar x) (FLit 0)) J.Add (FApp (FVar y) (FLit 0))
       )
    )
            
program4 = FApp (FApp (FApp (FApp (FTApp notail4 PFInt) summa) (FTApp const PFInt)) (FLit 5)) (FLit 6)

test1 = \c -> it "Should compile factorial 10" $ ((compileAndRun c fact_app) `shouldReturn` "3628800\n")

test2 = \c -> it "Should compile fibonacci 10" $ ((compileAndRun c fibo_app) `shouldReturn` "55\n")

test3 = \c -> it "Should infeer type of intapp" $ "(forall (_ : Int) . Int)" `shouldBe` ( let (cu, t) = (createCU (c intapp) Nothing) in (show t) )

test4 = \c -> it "Should compile idF int 10" $ ((compileAndRun c idfNum) `shouldReturn` "10\n")

test5 = \c -> it "Should compile const int 10 20" $ ((compileAndRun c constNum) `shouldReturn` "10\n")

test6 = \c -> it "Should compile program1 int 5" $ ((compileAndRun c program1Num) `shouldReturn` "5\n")

test7 = \c -> it "Should compile program2" $ ((compileAndRun c program2) `shouldReturn` "5\n")

test8 = \c -> it "Should compile program4" $ ((compileAndRun c program4) `shouldReturn` "11\n")

suite = [test1, test2, test3, test4, test5, test6, test7, test8]

naivesuite = describe "Naive compilation (BaseTransCF)" $ forM_ suite (\t -> t compileN)
aosuite = describe "ApplyOpt compilation (ApplyTransCF)" $ forM_ suite (\t -> t compileAO)

-- Similar to the ":t" in GHCi
-- inferHM :: String -> IO ()
-- inferHM = putStrLn . HM.pretty . HM.infer . readHM 

evenOdd :: String
evenOdd = "let rec even = \\n -> n == 0 || odd (n-1) and odd = \\n -> if n == 0 then 0 else even (n-1) in odd"

spec = do describe "Compile and run the result" $
            do 
              naivesuite
              aosuite
main = hspec spec