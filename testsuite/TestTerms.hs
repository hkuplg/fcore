module TestTerms where

import qualified Src as S
import Core

import Language.Java.Syntax as J (Op(..))

import Prelude hiding (const)

-- mu loop. \x -> loop x
loopStr = "fix loop. \\(x : Int). loop x : Int -> Int"
loop = Fix (\loop x -> App (var loop) (var x)) (JClass "java.lang.Integer") (JClass "java.lang.Integer")

factStr = "fix fact. \\(n : Int). if n == 0 then 1 else n * fact (n-1) : Int"
fact = Fix (\fact n ->
    If (PrimOp (var n) (S.Compare J.Equal) (Lit (S.Int 0)))
        (Lit (S.Int 1))
        (PrimOp (var n) (S.Arith J.Mult) (App (var fact) (PrimOp (var n) (S.Arith J.Sub) (Lit (S.Int 1))))))
    (JClass "java.lang.Integer") (JClass "java.lang.Integer")

tfact = Fix (\fact n -> lam (JClass "java.lang.Integer") (\acc ->
    If (PrimOp (var n) (S.Compare J.Equal) (Lit (S.Int 0)))
        (var acc)
        (App (App (var fact) (PrimOp (var n) (S.Arith J.Sub) (Lit (S.Int 1)))) (PrimOp (var n) (S.Arith J.Mult) (var acc)))))
    (JClass "java.lang.Integer") (Fun (JClass "java.lang.Integer") (JClass "java.lang.Integer"))

fiboStr = "fix fibo. \\(n : Int). if0 n then 1 else (fibo (n-1)) + (fibo (n-2)) : Int"
fibo = Fix (\fibo n ->
    If (PrimOp (var n) (S.Compare J.Equal) (Lit (S.Int 2)))
        (Lit (S.Int 1))
        (If (PrimOp (var n) (S.Compare J.Equal) (Lit (S.Int 1)))
             (Lit (S.Int 1))
             (PrimOp (App (var fibo) (PrimOp (var n) (S.Arith J.Sub) (Lit (S.Int 1)))) (S.Arith J.Add) (App (var fibo) (PrimOp (var n) (S.Arith J.Sub) (Lit (S.Int 2)))))))
    (JClass "java.lang.Integer") (JClass "java.lang.Integer")

factApp = App fact (Lit (S.Int 10))

fiboApp = App fibo (Lit (S.Int 10))
-- /\A. \(x:A) . x

idF1Str = "/\\A. \\(x:A). x"
idF = BLam (\a -> lam (TVar a) (\x -> var x))

-- /\A . (\(f : A -> A) . \(x : A) . f x) (idF A)

idF2Str = "/\\A. (\\(f : A -> A). \\(x : A). f x) (idF A)"
idF2 = BLam (\a -> App (lam (Fun (TVar a) (TVar a)) (\f -> lam (TVar a) (\x -> App (var f) (var x)))) (TApp idF (TVar a)))

-- /\A . \(x:A) . (idF A) x

idF3Str = "/\\A . \\(x:A) . (idF A) x"
idF3 = BLam (\a -> lam (TVar a) (\x -> App (TApp idF (TVar a)) (var x) ))

notailStr = "/\\A. \\(f : A -> (A -> A)). \\(g : A -> A). \\(x : A). (f x) (g x)"
notail =
  BLam (\a ->
    lam (Fun (TVar a) (Fun (TVar a) (TVar a))) (\f ->
      lam (Fun (TVar a) (TVar a)) (\g ->
        lam (TVar a) (\x ->
          App (App (var f) (var x)) (App (var g) (var x)) ))))

constStr = "/\\A . \\(x : A) . \\(y : A) . x"
const =
  BLam (\a ->
    lam (TVar a) (\x ->
       lam (TVar a) (\y ->
          var x
       )
    )
  )

-- /\A . \(x : A) . (/\A . \(f : A -> A -> A) . \(g : A -> A) . \(x : A) . f x (g x)) A (const A) (idF A) x
-- /\A . \(x : A) . notail A (const A) (idF A) x
program1 =
  BLam (\a ->
    lam (TVar a) (\x ->
       App (App (App (TApp notail (TVar a)) (TApp const (TVar a))) (TApp idF (TVar a))) (var x)
    )
  )

program1Num = App (TApp program1 (JClass "java.lang.Integer")) (Lit (S.Int 5))

-- should infer (forall (x0 : int) . int)
intapp = TApp idF (JClass "java.lang.Integer")


notail2Str = "/\\A. \\(f : A -> (A -> A)). \\(x : A). \\(y : A). (f x) ((f y) y)"
notail2 =
  BLam (\a ->
    lam (Fun (TVar a) (Fun (TVar a) (TVar a))) (\f ->
      lam (TVar a) (\x ->
        lam (TVar a) (\y ->
          App (App (var f) (var x)) (App (App (var f) (var y)) (var y)) ))))


program2 = App (App (App (TApp notail2 (JClass "java.lang.Integer")) (TApp const (JClass "java.lang.Integer"))) (Lit (S.Int 5))) (Lit (S.Int 6))

idfNum = App (TApp idF (JClass "java.lang.Integer")) (Lit (S.Int 10))

constNum = App (App (TApp const (JClass "java.lang.Integer")) (Lit (S.Int 10))) (Lit (S.Int 20))

notail3Str = "/\\A. \\(f : A -> (A -> A)). \\(g : A -> (A -> A)). \\(x : A). \\(y : A). (f x) ((g y) y)"
notail3 =
  BLam (\a ->
    lam (Fun (TVar a) (Fun (TVar a) (TVar a))) (\f ->
      lam (Fun (TVar a) (Fun (TVar a) (TVar a))) (\g ->
        lam (TVar a) (\x ->
          lam (TVar a) (\y ->
            App (App (var f) (var x)) (App (App (var g) (var y)) (var y)) )))))

program3 = App (App (App (App (TApp notail3 (JClass "java.lang.Integer")) (TApp const (JClass "java.lang.Integer"))) (TApp const (JClass "java.lang.Integer"))) (Lit (S.Int 5))) (Lit (S.Int 6))

notail4Str = "/\\A. \\(g : ((A -> A) -> (A -> A)) -> A). \\(f : A -> (A -> A)). \\(x : A). \\(y : A). (g (f x)) (f y)"
notail4 =
  BLam (\a ->
    lam ( Fun (Fun (TVar a) (TVar a)) (Fun (Fun (TVar a) (TVar a)) (TVar a))) (\g ->
      lam (Fun (TVar a) (Fun (TVar a) (TVar a))) (\f ->
        lam (TVar a) (\x ->
          lam (TVar a) (\y ->
            App (App (var g) (App (var f) (var x))) (App (var f) (var y)))))))

summaStr= "\\(x : Int -> Int). \\(y : Int -> Int). (x 0) + (y 0)"
summa =
    lam (Fun (JClass "java.lang.Integer") (JClass "java.lang.Integer")) (\x ->
       lam (Fun (JClass "java.lang.Integer") (JClass "java.lang.Integer")) (\y ->
          PrimOp (App (var x) (Lit (S.Int 0))) (S.Arith J.Add) (App (var y) (Lit (S.Int 0)))
       )
    )

program4 = App (App (App (App (TApp notail4 (JClass "java.lang.Integer")) summa) (TApp const (JClass "java.lang.Integer"))) (Lit (S.Int 5))) (Lit (S.Int 6))

evenOdd :: String
evenOdd = "let rec even = \\n -> n == 0 || odd (n-1) and odd = \\n -> if n == 0 then 0 else even (n-1) in odd"
