module TestTerms where

import SystemF.Syntax
import ESF.Syntax

import Language.Java.Syntax as J

import Prelude hiding (const)

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