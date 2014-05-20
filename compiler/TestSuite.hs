module TestSuite where

import Test.HUnit hiding (State)
import SystemF
import Language.Java.Syntax as J
import Prelude hiding (const)
import qualified HM
import HMParser         (readHM)
import SystemFParser    (readSF)
import BaseTransCFJava (createCU)
import Language.Java.Pretty
import MonadLib
import System.Process
import System.Directory

import Translations
import BaseTransCFJava
import ApplyTransCFJava
import Data.Map
import Data.List
import ClosureF
import Inheritance

-- setting
type MAOpt = StateT Int (StateT (Map J.Exp Int) (Writer Bool)) 
sopt :: ApplyOptTranslate MAOpt  -- instantiation; all coinstraints resolved
sopt = applyopt

translate ::  PCExp Int (Var, PCTyp Int) -> MAOpt ([BlockStmt], Exp, PCTyp Int)
translate e = translateM (up sopt) e

compile ::  PFExp Int (Var, PCTyp Int) -> (Block, Exp, PCTyp Int)
compile e = 
  case fst $ runWriter (evalStateT (evalStateT (translate (fexp2cexp e)) 0) empty) of
      (ss,exp,t) -> (J.Block ss,exp, t)

-- java compilation + run
compileAndRun exp = do let source = prettyPrint (fst $ createCU (compile exp) Nothing)
                       writeFile "Main.java" source
                       readProcess "javac" ["Main.java"] ""
                       result <- readProcess "java" ["Main"] ""
                       readProcess "rm" ["Main.java"] ""
		       x <- getDirectoryContents "."
                       readProcess "rm" [y | y<- x, ".class" `isSuffixOf` y] ""
                       return result

-- Some test terms

-- mu loop . \x -> loop x
loop = FFix PFInt (\loop x -> FApp (FVar loop) (FVar x)) PFInt

-- mu fact . \(n : Int) . if0 n then 1 else n * fact (n-1)
fact = FFix PFInt (\fact n -> 
   Fif0  (FVar n) 
         (FLit 1) 
         (FPrimOp (FVar n) J.Mult (FApp (FVar fact) (FPrimOp (FVar n) J.Sub (FLit 1))))) PFInt

-- mu fibo . \(n : Int) . if0 n then 1 else (fibo (n - 1) + fibo (n-2))
fibo = FFix PFInt (\fibo n -> 
   Fif0  (FPrimOp (FVar n) J.Sub (FLit 2))
         (FLit 1) 
         (Fif0  (FPrimOp (FVar n) J.Sub (FLit 1)) 
               (FLit 1) 
               (FPrimOp (FApp (FVar fibo) (FPrimOp (FVar n) J.Sub (FLit 1))) J.Add (FApp (FVar fibo) (FPrimOp (FVar n) J.Sub (FLit 2)))))) PFInt
      
fact_app = FApp fact (FLit 10)

fibo_app = FApp fibo (FLit 10)
-- /\A. \(x:A) . x

idF1Str = "/\\A. \\(x:A) . x"
idF = FBLam (\a -> FLam (FTVar a) (\x -> FVar x))

-- /\A . (\(f : A -> A) . \(x : A) . f x) (idF A)

idF2Str = "/\\A . (\\(f : A -> A) . \\(x : A) . f x) (idF A)"
idF2 = FBLam (\a -> FApp (FLam (FFun (FTVar a) (FTVar a)) (\f -> FLam (FTVar a) (\x -> FApp (FVar f) (FVar x)))) (FTApp idF (FTVar a)))

-- /\A . \(x:A) . (idF A) x

idF3Str = "/\\A . \\(x:A) . (idF A) x"
idF3 = FBLam (\a -> FLam (FTVar a) (\x -> FApp (FTApp idF (FTVar a)) (FVar x) ))

-- /\A . \(f : A -> A -> A) . \(g : A -> A) . \(x : A) . f x (g x)
notail =
  FBLam (\a ->
    FLam (FFun (FTVar a) (FFun (FTVar a) (FTVar a))) (\f ->
      FLam (FFun (FTVar a) (FTVar a)) (\g ->
        FLam (FTVar a) (\x ->
          FApp (FApp (FVar f) (FVar x)) (FApp (FVar g) (FVar x)) ))))

-- /\A . \(x : A) . \(y : A) . x
const =
  FBLam (\a ->
    FLam (FTVar a) (\x ->
       FLam (FTVar a) (\y ->
          FVar x
       )
    )
  )

-- /\A . \(x : A) . notail A (const A) (idF A) x
program1 =
  FBLam (\a ->
    FLam (FTVar a) (\x ->
       FApp (FApp (FApp (FTApp notail (FTVar a)) (FTApp const (FTVar a))) (FTApp idF (FTVar a))) (FVar x)
    )
  )

-- should infer (forall (x0 : int) . int)
intapp = FTApp idF PFInt


-- \(f : A -> A -> A) . \(x : A) . \(y : A) . f x (f y y)
notail2 =
  FBLam (\a ->
    FLam (FFun (FTVar a) (FFun (FTVar a) (FTVar a))) (\f ->
      FLam (FTVar a) (\x ->
        FLam (FTVar a) (\y ->
          FApp (FApp (FVar f) (FVar x)) (FApp (FApp (FVar f) (FVar y)) (FVar y)) ))))

test1 = "Should compile factorial 10" ~: assert (liftM (== "3628800\n") (compileAndRun fact_app))

test2 = "Should compile fibonacci 10" ~: assert (liftM (== "55\n") (compileAndRun fibo_app))

test3 = "Should infeer type of intapp" ~: "(forall (_ : Int) . Int)" ~=? ( let (cu, t) = (createCU (compile intapp) Nothing) in (show t) )


-- SystemF to Java
sf2java :: String -> String
sf2java src = let (cu, _) = (createCU (compile (readSF src)) Nothing) in prettyPrint cu

-- SystemF file path to Java
-- Example:
--      loadsf2java "id.sf"
loadsf2java :: FilePath -> IO String
loadsf2java = readFile >=> (return . sf2java)

-- `compilesf2java srcPath outputPath` loads a SystemF file at `srcPath`,
-- and writes the compiled Java code to `outputPath`.
-- Example:
--      compilesf2java "id.sf" "id.java"
compilesf2java :: FilePath -> FilePath -> IO ()
compilesf2java srcPath outputPath = loadsf2java srcPath >>= writeFile outputPath

-- Similar to the ":t" in GHCi
inferHM :: String -> IO ()
inferHM = putStrLn . HM.pretty . HM.infer . readHM 

evenOdd :: String
evenOdd = "let rec even = \\n -> n == 0 || odd (n-1) and odd = \\n -> if n == 0 then 0 else even (n-1) in odd"

main = runTestTT $ TestList [test1, test2, test3]
