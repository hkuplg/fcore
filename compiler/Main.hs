module Main where

import SystemFJava2
import Test.HUnit
import Language.Java.Pretty

import Prelude hiding (const)

-- Some test terms

-- /\a. \(x:a) . x

idF = FBLam (\a -> FLam (FTVar a) (\x -> FVar x))

-- /\a . (\(f : a -> a) . \(x : a) . f x) (idF a)

idF2 = FBLam (\a -> FApp (FLam (FFun (FTVar a) (FTVar a)) (\f -> FLam (FTVar a) (\x -> FApp (FVar f) (FVar x)))) (FTApp idF (FTVar a)))

-- /\a . \(x:a) . (idF a) x

idF3 = FBLam (\a -> FLam (FTVar a) (\x -> FApp (FTApp idF (FTVar a)) (FVar x) ))

-- /\a . \(f : a -> a -> a) . \(g : a -> a) . \(x : a) . f x (g x)
notail =
  FBLam (\a ->
    FLam (FFun (FTVar a) (FFun (FTVar a) (FTVar a))) (\f ->
      FLam (FFun (FTVar a) (FTVar a)) (\g ->
        FLam (FTVar a) (\x ->
          FApp (FApp (FVar f) (FVar x)) (FApp (FVar g) (FVar x)) ))))

-- /\ a . \(x : a) . \(y : a) . x
const =
  FBLam (\a ->
    FLam (FTVar a) (\x ->
       FLam (FTVar a) (\y ->
          FVar x
       )
    )
  )

-- /\a . \(x : a) . notail a (const a) (idF a) x
program1 =
  FBLam (\a ->
    FLam (FTVar a) (\x ->
       FApp (FApp (FApp (FTApp notail (FTVar a)) (FTApp const (FTVar a))) (FTApp idF (FTVar a))) (FVar x)
    )
  )

-- should infer (forall (x0 : int) . int)
intapp = FTApp idF PFInt



compiled1 = "abstract class Closure\n{\n  Object x;\n  Object out;\n  abstract void apply ()\n  ;\n}\nclass MyClosure extends Closure\n{\n  void apply ()\n  {\n    Closure x1 = new Closure()\n                 {\n                   Closure x2 = this;\n                   void apply ()\n                   {\n                     out = x2.x;\n                   }\n                 };\n    out = x1;\n  }\n}"
compiled2 = "abstract class Closure\n{\n  Object x;\n  Object out;\n  abstract void apply ()\n  ;\n}\nclass MyClosure extends Closure\n{\n  void apply ()\n  {\n    Closure x2 = new Closure()\n                 {\n                   Closure x3 = this;\n                   {\n                     out = new Closure()\n                           {\n                             Closure x5 = this;\n                             void apply ()\n                             {\n                               Closure x6 = (Closure) x3.x;\n                               x6.x = x5.x;\n                               x6.apply();\n                               out = x6.out;\n                             }\n                           };\n                   }\n                   void apply ()\n                   {\n                   }\n                 };\n    Closure x3 = new Closure()\n                 {\n                   Closure x4 = this;\n                   void apply ()\n                   {\n                     out = x4.x;\n                   }\n                 };\n    Closure x1 = (Closure) x2;\n    x1.x = x3;\n    out = x1.out;\n  }\n}"
compiled3 = "abstract class Closure\n{\n  Object x;\n  Object out;\n  abstract void apply ()\n  ;\n}\nclass MyClosure extends Closure\n{\n  void apply ()\n  {\n    Closure x1 = new Closure()\n                 {\n                   Closure x2 = this;\n                   void apply ()\n                   {\n                     Closure x5 = new Closure()\n                                  {\n                                    Closure x6 = this;\n                                    void apply ()\n                                    {\n                                      out = x6.x;\n                                    }\n                                  };\n                     Closure x3 = (Closure) x5;\n                     x3.x = x2.x;\n                     x3.apply();\n                     out = x3.out;\n                   }\n                 };\n    out = x1;\n  }\n}"

test1 = TestCase $ assertEqual
  "Should compile idF" compiled1 ( let (cu,t) = createCU $ compile idF in (prettyPrint cu) )

test2 = TestCase $ assertEqual
  "Should compile idF2" compiled2 ( let (cu,t) = createCU $ compile idF2 in (prettyPrint cu) )

test3 = TestCase $ assertEqual
  "Should compile idF3" compiled3 ( let (cu,t) = createCU $ compile idF3 in (prettyPrint cu) )

test4 = TestCase $ assertEqual
  "Should infeer type of intapp" "(forall (x0 : Int) . Int)" ( let (cu, t) = createCU $ compile intapp in (show t) )

main = runTestTT $ TestList [test1, test2, test3, test4]
