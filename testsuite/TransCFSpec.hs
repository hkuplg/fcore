module TransCFSpec where

import ESF.Parser       (reader)
import ESF.TypeCheck    (infer)
import ESF.Syntax
import Desugar          (desugarTcExpr)
import SystemF.Syntax
import ClosureF

import ApplyTransCFJava
import BaseTransCFJava

import Inheritance
import MonadLib
import Translations     (compileN, compileAO, compileS)

import Language.Java.Syntax as J
import Language.Java.Pretty
import System.Directory
import System.Process
import Text.PrettyPrint.Leijen
import Test.Hspec

import Data.Map
import qualified Data.List as List      (isSuffixOf)
import qualified Data.Set  as Set

import Prelude hiding (const)

import Control.Monad.Trans.Error (runErrorT)

-- java compilation + run
compileAndRun name compileF exp =
   do let source = prettyPrint (fst $ (compileF name exp))
      let jname = name ++ ".java"
      writeFile jname source
      readProcess "javac" [jname] ""
      result <- readProcess "java" [name] ""
      -- readProcess "rm" [jname] ""
      x <- getDirectoryContents "."
      readProcess "rm" [y | y <- x, ".class" `List.isSuffixOf` y, y /= "TypeServer.class"] ""
      return result

-- Some test terms

-- mu loop. \x -> loop x
loopStr = "fix loop. \\(x : Int). loop x : Int -> Int"
loop = FFix (\loop x -> FApp (FVar "" loop) (FVar "" x)) (FJClass "java.lang.Integer") (FJClass "java.lang.Integer")

factStr = "fix fact. \\(n : Int). if n == 0 then 1 else n * fact (n-1) : Int"
fact = FFix (\fact n ->
    FIf (FPrimOp (FVar "" n) (Compare J.Equal) (FLit (Integer 0)))
        (FLit (Integer 1))
        (FPrimOp (FVar "" n) (Arith J.Mult) (FApp (FVar "" fact) (FPrimOp (FVar "" n) (Arith J.Sub) (FLit (Integer 1))))))
    (FJClass "java.lang.Integer") (FJClass "java.lang.Integer")

tfact = FFix (\fact n -> FLam (FJClass "java.lang.Integer") (\acc ->
    FIf (FPrimOp (FVar "" n) (Compare J.Equal) (FLit (Integer 0)))
        (FVar "" acc)
        (FApp (FApp (FVar "" fact) (FPrimOp (FVar "" n) (Arith J.Sub) (FLit (Integer 1)))) (FPrimOp (FVar "" n) (Arith J.Mult) (FVar "" acc)))))
    (FJClass "java.lang.Integer") (FFun (FJClass "java.lang.Integer") (FJClass "java.lang.Integer"))

fiboStr = "fix fibo. \\(n : Int). if0 n then 1 else (fibo (n-1)) + (fibo (n-2)) : Int"
fibo = FFix (\fibo n ->
    FIf (FPrimOp (FVar "" n) (Compare J.Equal) (FLit (Integer 2)))
        (FLit (Integer 1))
        (FIf (FPrimOp (FVar "" n) (Compare J.Equal) (FLit (Integer 1)))
             (FLit (Integer 1))
             (FPrimOp (FApp (FVar "" fibo) (FPrimOp (FVar "" n) (Arith J.Sub) (FLit (Integer 1)))) (Arith J.Add) (FApp (FVar "" fibo) (FPrimOp (FVar "" n) (Arith J.Sub) (FLit (Integer 2)))))))
    (FJClass "java.lang.Integer") (FJClass "java.lang.Integer")

factApp = FApp fact (FLit (Integer 10))

fiboApp = FApp fibo (FLit (Integer 10))
-- /\A. \(x:A) . x

idF1Str = "/\\A. \\(x:A). x"
idF = FBLam (\a -> FLam (FTVar a) (\x -> FVar "" x))

-- /\A . (\(f : A -> A) . \(x : A) . f x) (idF A)

idF2Str = "/\\A. (\\(f : A -> A). \\(x : A). f x) (idF A)"
idF2 = FBLam (\a -> FApp (FLam (FFun (FTVar a) (FTVar a)) (\f -> FLam (FTVar a) (\x -> FApp (FVar "" f) (FVar "" x)))) (FTApp idF (FTVar a)))

-- /\A . \(x:A) . (idF A) x

idF3Str = "/\\A . \\(x:A) . (idF A) x"
idF3 = FBLam (\a -> FLam (FTVar a) (\x -> FApp (FTApp idF (FTVar a)) (FVar "" x) ))

notailStr = "/\\A. \\(f : A -> (A -> A)). \\(g : A -> A). \\(x : A). (f x) (g x)"
notail =
  FBLam (\a ->
    FLam (FFun (FTVar a) (FFun (FTVar a) (FTVar a))) (\f ->
      FLam (FFun (FTVar a) (FTVar a)) (\g ->
        FLam (FTVar a) (\x ->
          FApp (FApp (FVar "" f) (FVar "" x)) (FApp (FVar "" g) (FVar "" x)) ))))

constStr = "/\\A . \\(x : A) . \\(y : A) . x"
const =
  FBLam (\a ->
    FLam (FTVar a) (\x ->
       FLam (FTVar a) (\y ->
          FVar "" x
       )
    )
  )

-- /\A . \(x : A) . (/\A . \(f : A -> A -> A) . \(g : A -> A) . \(x : A) . f x (g x)) A (const A) (idF A) x
-- /\A . \(x : A) . notail A (const A) (idF A) x
program1 =
  FBLam (\a ->
    FLam (FTVar a) (\x ->
       FApp (FApp (FApp (FTApp notail (FTVar a)) (FTApp const (FTVar a))) (FTApp idF (FTVar a))) (FVar "" x)
    )
  )

program1Num = FApp (FTApp program1 (FJClass "java.lang.Integer")) (FLit (Integer 5))

-- should infer (forall (x0 : int) . int)
intapp = FTApp idF (FJClass "java.lang.Integer")


notail2Str = "/\\A. \\(f : A -> (A -> A)). \\(x : A). \\(y : A). (f x) ((f y) y)"
notail2 =
  FBLam (\a ->
    FLam (FFun (FTVar a) (FFun (FTVar a) (FTVar a))) (\f ->
      FLam (FTVar a) (\x ->
        FLam (FTVar a) (\y ->
          FApp (FApp (FVar "" f) (FVar "" x)) (FApp (FApp (FVar "" f) (FVar "" y)) (FVar "" y)) ))))


program2 = FApp (FApp (FApp (FTApp notail2 (FJClass "java.lang.Integer")) (FTApp const (FJClass "java.lang.Integer"))) (FLit (Integer 5))) (FLit (Integer 6))

idfNum = FApp (FTApp idF (FJClass "java.lang.Integer")) (FLit (Integer 10))

constNum = FApp (FApp (FTApp const (FJClass "java.lang.Integer")) (FLit (Integer 10))) (FLit (Integer 20))

notail3Str = "/\\A. \\(f : A -> (A -> A)). \\(g : A -> (A -> A)). \\(x : A). \\(y : A). (f x) ((g y) y)"
notail3 =
  FBLam (\a ->
    FLam (FFun (FTVar a) (FFun (FTVar a) (FTVar a))) (\f ->
      FLam (FFun (FTVar a) (FFun (FTVar a) (FTVar a))) (\g ->
        FLam (FTVar a) (\x ->
          FLam (FTVar a) (\y ->
            FApp (FApp (FVar "" f) (FVar "" x)) (FApp (FApp (FVar "" g) (FVar "" y)) (FVar "" y)) )))))

program3 = FApp (FApp (FApp (FApp (FTApp notail3 (FJClass "java.lang.Integer")) (FTApp const (FJClass "java.lang.Integer"))) (FTApp const (FJClass "java.lang.Integer"))) (FLit (Integer 5))) (FLit (Integer 6))

notail4Str = "/\\A. \\(g : ((A -> A) -> (A -> A)) -> A). \\(f : A -> (A -> A)). \\(x : A). \\(y : A). (g (f x)) (f y)"
notail4 =
  FBLam (\a ->
    FLam ( FFun (FFun (FTVar a) (FTVar a)) (FFun (FFun (FTVar a) (FTVar a)) (FTVar a))) (\g ->
      FLam (FFun (FTVar a) (FFun (FTVar a) (FTVar a))) (\f ->
        FLam (FTVar a) (\x ->
          FLam (FTVar a) (\y ->
            FApp (FApp (FVar "" g) (FApp (FVar "" f) (FVar "" x))) (FApp (FVar "" f) (FVar "" y)))))))

summaStr= "\\(x : Int -> Int). \\(y : Int -> Int). (x 0) + (y 0)"
summa =
    FLam (FFun (FJClass "java.lang.Integer") (FJClass "java.lang.Integer")) (\x ->
       FLam (FFun (FJClass "java.lang.Integer") (FJClass "java.lang.Integer")) (\y ->
          FPrimOp (FApp (FVar "" x) (FLit (Integer 0))) (Arith J.Add) (FApp (FVar "" y) (FLit (Integer 0)))
       )
    )

program4 = FApp (FApp (FApp (FApp (FTApp notail4 (FJClass "java.lang.Integer")) summa) (FTApp const (FJClass "java.lang.Integer"))) (FLit (Integer 5))) (FLit (Integer 6))

test1 = \c -> it "Should compile factorial 10" (compileAndRun "FactApp" c factApp `shouldReturn` "3628800\n")

test2 = \c -> it "Should compile fibonacci 10" (compileAndRun "FiboApp" c fiboApp `shouldReturn` "55\n")

test3 = \c -> it "Should infer type of intapp" $ "(forall (_ : java.lang.Integer) . java.lang.Integer)" `shouldBe` ( let (cu, t) = (c "Main" intapp) in show t )

test4 = \c -> it "Should compile idF int 10" (compileAndRun "IdF" c idfNum `shouldReturn` "10\n")

test5 = \c -> it "Should compile const int 10 20" (compileAndRun "Const" c constNum `shouldReturn` "10\n")

test6 = \c -> it "Should compile program1 int 5" (compileAndRun "Program1" c program1Num `shouldReturn` "5\n")

test7 = \c -> it "Should compile program2" (compileAndRun "Program2" c program2 `shouldReturn` "5\n")

test8 = \c -> it "Should compile program4" (compileAndRun "Program4" c program4 `shouldReturn` "11\n")

suite = [test1, test2, test3, test4, test5, test6, test7, test8,
        ioTest "testsuite/tests/pipeline/lit_java.sf" "1\n",
        ioTest "testsuite/tests/pipeline/fact_big.sf" "87178291200\n",
        ioTest "testsuite/tests/pipeline/charat.sf" "c\n",
        ioTest "testsuite/tests/pipeline/substr.sf" "bc\n",
        ioTest "testsuite/tests/pipeline/always_true.sf" "true\n",
        ioTest "testsuite/tests/pipeline/always_false.sf" "false\n",
        ioTest "testsuite/tests/pipeline/hello_world.sf" "hello, world\n",
        ioTest "testsuite/tests/pipeline/hello_world2.sf" "HELLO, WORLD\n",
        ioTest "testsuite/tests/pipeline/min.sf" "4\n",
        ioTest "testsuite/tests/pipeline/max.sf" "7\n",
        ioTest "testsuite/tests/pipeline/comb.sf" "220\n",
        ioTest "testsuite/tests/pipeline/interfact.sf" "1\n",
        ioTest "testsuite/tests/pipeline/tailfact.sf" "3628800\n",
        ioTest "testsuite/tests/pipeline/fact.sf" "3628800\n",
        ioTest "testsuite/tests/pipeline/fibo.sf" "55\n",
        ioTest "testsuite/tests/pipeline/id.sf" "3\n",
      --ioTest "testsuite/tests/pipeline/mutual.sf" "1\n", --parse error
      --ioTest "testsuite/tests/pipeline/mutual2.sf" "1\n", --parse error
        ioTest "testsuite/tests/pipeline/mutualrec.sf" "1\n",
        ioTest "testsuite/tests/pipeline/Mutualrec2.sf" "0\n",
      --ioTest "testsuite/tests/pipeline/Mutualrec3.sf" "1\n", --patternmatch fail
        ioTest "testsuite/tests/pipeline/Products.sf" "1\n",
        ioTest "testsuite/tests/pipeline/Products4.sf" "3\n"]

testSf srcPath compileF = do
        src <- readFile srcPath
        let expr = ESF.Parser.reader src
        res <- runErrorT $ ESF.TypeCheck.infer expr
        case res of
          Left typeError -> error $ show (Text.PrettyPrint.Leijen.pretty typeError)
          Right (tcExpr, _t) ->
            let sf = desugarTcExpr tcExpr in
            compileAndRun "Main" compileF sf

naivesuite = describe "Naive compilation (BaseTransCF)" $ forM_ suite (\t -> t compileN)
aosuite = describe "ApplyOpt compilation (ApplyTransCF)" $ forM_ suite (\t -> t compileAO)
stacksuite = describe "Stack compilation (StackTransCF)" $ forM_ suite (\t -> t compileS)

ioTest = \src exp c  -> it ("Should compile " ++ src) $ ((testSf src c) `shouldReturn` exp)

evenOdd :: String
evenOdd = "let rec even = \\n -> n == 0 || odd (n-1) and odd = \\n -> if n == 0 then 0 else even (n-1) in odd"

spec = describe "Compile and run the result" $
        do naivesuite
           aosuite
           stacksuite

main = hspec spec
