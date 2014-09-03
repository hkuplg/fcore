module TestTerms where

import qualified Src
import Core

import Language.Java.Syntax as J (Op(..))

import Prelude hiding (const)

-- mu loop. \x -> loop x
loopStr = "fix loop. \\(x : Int). loop x : Int -> Int"
loop = Fix (\loop x -> App (Var loop) (Var x)) (JClass "java.lang.Integer") (JClass "java.lang.Integer")

factStr = "fix fact. \\(n : Int). if n == 0 then 1 else n * fact (n-1) : Int"
fact = Fix (\fact n ->
    If (PrimOp (Var n) (Src.Compare J.Equal) (Lit (Src.Integer 0)))
        (Lit (Src.Integer 1))
        (PrimOp (Var n) (Src.Arith J.Mult) (App (Var fact) (PrimOp (Var n) (Src.Arith J.Sub) (Lit (Src.Integer 1))))))
    (JClass "java.lang.Integer") (JClass "java.lang.Integer")

tfact = Fix (\fact n -> Lam (JClass "java.lang.Integer") (\acc ->
    If (PrimOp (Var n) (Src.Compare J.Equal) (Lit (Src.Integer 0)))
        (Var acc)
        (App (App (Var fact) (PrimOp (Var n) (Src.Arith J.Sub) (Lit (Src.Integer 1)))) (PrimOp (Var n) (Src.Arith J.Mult) (Var acc)))))
    (JClass "java.lang.Integer") (Fun (JClass "java.lang.Integer") (JClass "java.lang.Integer"))

fiboStr = "fix fibo. \\(n : Int). if0 n then 1 else (fibo (n-1)) + (fibo (n-2)) : Int"
fibo = Fix (\fibo n ->
    If (PrimOp (Var n) (Src.Compare J.Equal) (Lit (Src.Integer 2)))
        (Lit (Src.Integer 1))
        (If (PrimOp (Var n) (Src.Compare J.Equal) (Lit (Src.Integer 1)))
             (Lit (Src.Integer 1))
             (PrimOp (App (Var fibo) (PrimOp (Var n) (Src.Arith J.Sub) (Lit (Src.Integer 1)))) (Src.Arith J.Add) (App (Var fibo) (PrimOp (Var n) (Src.Arith J.Sub) (Lit (Src.Integer 2)))))))
    (JClass "java.lang.Integer") (JClass "java.lang.Integer")

factApp = App fact (Lit (Src.Integer 10))

fiboApp = App fibo (Lit (Src.Integer 10))
-- /\A. \(x:A) . x

idF1Str = "/\\A. \\(x:A). x"
idF = BLam (\a -> Lam (TVar a) (\x -> Var x))

-- /\A . (\(f : A -> A) . \(x : A) . f x) (idF A)

idF2Str = "/\\A. (\\(f : A -> A). \\(x : A). f x) (idF A)"
idF2 = BLam (\a -> App (Lam (Fun (TVar a) (TVar a)) (\f -> Lam (TVar a) (\x -> App (Var f) (Var x)))) (TApp idF (TVar a)))

-- /\A . \(x:A) . (idF A) x

idF3Str = "/\\A . \\(x:A) . (idF A) x"
idF3 = BLam (\a -> Lam (TVar a) (\x -> App (TApp idF (TVar a)) (Var x) ))

notailStr = "/\\A. \\(f : A -> (A -> A)). \\(g : A -> A). \\(x : A). (f x) (g x)"
notail =
  BLam (\a ->
    Lam (Fun (TVar a) (Fun (TVar a) (TVar a))) (\f ->
      Lam (Fun (TVar a) (TVar a)) (\g ->
        Lam (TVar a) (\x ->
          App (App (Var f) (Var x)) (App (Var g) (Var x)) ))))

constStr = "/\\A . \\(x : A) . \\(y : A) . x"
const =
  BLam (\a ->
    Lam (TVar a) (\x ->
       Lam (TVar a) (\y ->
          Var x
       )
    )
  )

-- /\A . \(x : A) . (/\A . \(f : A -> A -> A) . \(g : A -> A) . \(x : A) . f x (g x)) A (const A) (idF A) x
-- /\A . \(x : A) . notail A (const A) (idF A) x
program1 =
  BLam (\a ->
    Lam (TVar a) (\x ->
       App (App (App (TApp notail (TVar a)) (TApp const (TVar a))) (TApp idF (TVar a))) (Var x)
    )
  )

program1Num = App (TApp program1 (JClass "java.lang.Integer")) (Lit (Src.Integer 5))

-- should infer (forall (x0 : int) . int)
intapp = TApp idF (JClass "java.lang.Integer")


notail2Str = "/\\A. \\(f : A -> (A -> A)). \\(x : A). \\(y : A). (f x) ((f y) y)"
notail2 =
  BLam (\a ->
    Lam (Fun (TVar a) (Fun (TVar a) (TVar a))) (\f ->
      Lam (TVar a) (\x ->
        Lam (TVar a) (\y ->
          App (App (Var f) (Var x)) (App (App (Var f) (Var y)) (Var y)) ))))


program2 = App (App (App (TApp notail2 (JClass "java.lang.Integer")) (TApp const (JClass "java.lang.Integer"))) (Lit (Src.Integer 5))) (Lit (Src.Integer 6))

idfNum = App (TApp idF (JClass "java.lang.Integer")) (Lit (Src.Integer 10))

constNum = App (App (TApp const (JClass "java.lang.Integer")) (Lit (Src.Integer 10))) (Lit (Src.Integer 20))

notail3Str = "/\\A. \\(f : A -> (A -> A)). \\(g : A -> (A -> A)). \\(x : A). \\(y : A). (f x) ((g y) y)"
notail3 =
  BLam (\a ->
    Lam (Fun (TVar a) (Fun (TVar a) (TVar a))) (\f ->
      Lam (Fun (TVar a) (Fun (TVar a) (TVar a))) (\g ->
        Lam (TVar a) (\x ->
          Lam (TVar a) (\y ->
            App (App (Var f) (Var x)) (App (App (Var g) (Var y)) (Var y)) )))))

program3 = App (App (App (App (TApp notail3 (JClass "java.lang.Integer")) (TApp const (JClass "java.lang.Integer"))) (TApp const (JClass "java.lang.Integer"))) (Lit (Src.Integer 5))) (Lit (Src.Integer 6))

notail4Str = "/\\A. \\(g : ((A -> A) -> (A -> A)) -> A). \\(f : A -> (A -> A)). \\(x : A). \\(y : A). (g (f x)) (f y)"
notail4 =
  BLam (\a ->
    Lam ( Fun (Fun (TVar a) (TVar a)) (Fun (Fun (TVar a) (TVar a)) (TVar a))) (\g ->
      Lam (Fun (TVar a) (Fun (TVar a) (TVar a))) (\f ->
        Lam (TVar a) (\x ->
          Lam (TVar a) (\y ->
            App (App (Var g) (App (Var f) (Var x))) (App (Var f) (Var y)))))))

summaStr= "\\(x : Int -> Int). \\(y : Int -> Int). (x 0) + (y 0)"
summa =
    Lam (Fun (JClass "java.lang.Integer") (JClass "java.lang.Integer")) (\x ->
       Lam (Fun (JClass "java.lang.Integer") (JClass "java.lang.Integer")) (\y ->
          PrimOp (App (Var x) (Lit (Src.Integer 0))) (Src.Arith J.Add) (App (Var y) (Lit (Src.Integer 0)))
       )
    )

program4 = App (App (App (App (TApp notail4 (JClass "java.lang.Integer")) summa) (TApp const (JClass "java.lang.Integer"))) (Lit (Src.Integer 5))) (Lit (Src.Integer 6))

evenOdd :: String
evenOdd = "let rec even = \\n -> n == 0 || odd (n-1) and odd = \\n -> if n == 0 then 0 else even (n-1) in odd"