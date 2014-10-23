module SymbolicEvaluator where

import           Core                 hiding (eval)
import qualified Language.Java.Syntax as J (Op (..))
import           Panic
import           Prelude              hiding (EQ, GT, LT)
import qualified Src                  as S


data Value = VInt Integer
           | VBool Bool
           | VFun (Value -> Value)

-- big-step interpreter
eval :: Expr () Value -> Value
eval (Var x) = x
eval (Lit x) =
    case x of
      S.Integer n -> VInt n
      S.Boolean b -> VBool b
      _ -> panic "Lit type unknown"
eval (Lam _ f) = VFun (eval . f)
eval (Let e f) = eval . f . eval $ e
eval (App e1 e2) =
    case eval e1 of
      VFun f -> f (eval e2)
      _ -> panic "e1 is not a function"
eval (BLam f) = eval (f ())
eval (TApp e1 _) = eval e1
eval (If e1 e2 e3) =
       case eval e1 of
         VBool True -> eval e2
         VBool False -> eval e3
         _ -> panic "e1 is not a boolean"
eval (PrimOp e1 op e2) =
       case (eval e1, eval e2) of
         (VInt a, VInt b) ->
             case op of
               -- arithmetic operations
               S.Arith J.Add -> VInt $ a + b
               S.Arith J.Mult -> VInt $ a * b
               S.Arith J.Sub -> VInt $ a - b
               S.Arith J.Div -> VInt $ a `div` b
               -- comparison operations
               S.Compare J.Equal -> VBool $ a == b
               S.Compare J.NotEq -> VBool $ a /= b
               S.Compare J.LThan -> VBool $ a < b
               S.Compare J.LThanE -> VBool $ a <= b
               S.Compare J.GThan -> VBool $ a > b
               S.Compare J.GThanE -> VBool $ a >= b
               -- _ -> simplified
         (VBool a, VBool b) ->
             case op of
               -- logic operations
               S.Logic J.And -> VBool $ a && b
               S.Logic J.Or -> VBool $ a || b
               -- _ -> simplified
         _ -> panic "e1 and e2 should be either Int or Boolean simutaneously"
eval _ = panic "Can not be evaled"

data ExecutionTree = Exp SymValue
                   | Fork ExecutionTree SymValue ExecutionTree

data SymValue = SFVar Int -- free variables
              | SInt Integer
              | SBool Bool
              | SApp SymValue SymValue
              | SOp Op SymValue SymValue
              | SFun (ExecutionTree -> ExecutionTree)

data Op = ADD
        | MUL
        | SUB
        | DIV
        | LT
        | LTE
        | GT
        | GTE
        | EQ
        | NEQ
        | OR
        | AND

exec :: ExecutionTree -> ExecutionTree
exec e = exec' e 0

exec' :: ExecutionTree -> Int -> ExecutionTree
exec' (Exp (SFun f)) n = exec' (f . Exp $ SFVar n) (n+1)
exec' e _ = e

-- symbolic evaluation
seval :: Expr () ExecutionTree -> ExecutionTree
seval (Var x) = x
seval (Lit x) =
    case x of
      S.Integer n -> Exp $ SInt n
      S.Boolean b -> Exp $ SBool b

seval (If e1 e2 e3) = propagate (seval e1) (seval e2) (seval e3)

seval (PrimOp e1 op e2) =
    case op of
        -- arithmetic operations
        S.Arith J.Add -> merge (SOp ADD, ADD) e1' e2'
        S.Arith J.Mult -> merge (SOp MUL, MUL) e1' e2'
        S.Arith J.Sub -> merge (SOp SUB, SUB) e1' e2'
        S.Arith J.Div -> merge (SOp DIV, DIV) e1' e2'

        -- comparison operations
        S.Compare J.Equal -> merge (SOp EQ, EQ) e1' e2'
        S.Compare J.NotEq -> merge (SOp NEQ, NEQ) e1' e2'
        S.Compare J.LThan -> merge (SOp LT, LT) e1' e2'
        S.Compare J.LThanE -> merge (SOp LTE, LTE) e1' e2'
        S.Compare J.GThan -> merge (SOp GT, GT) e1' e2'
        S.Compare J.GThanE -> merge (SOp GTE, GTE) e1' e2'

        -- logic operations
        S.Logic J.And -> merge (SOp AND, AND) e1' e2'
        S.Logic J.Or -> merge (SOp OR, OR) e1' e2'

    where e1' = seval e1
          e2' = seval e2

seval (Lam _ f) = Exp $ SFun (seval . f)
seval (Let e f) = let v = seval e in seval (f v)
seval (App e1 e2) = treeApply (seval e1) (seval e2)
seval (BLam f) =  seval $ f ()
seval (TApp e _) = seval e
seval e = panic $ "Not supported"
-- seval (Fix e1 e2 e3) = _seval_body
-- seval (LetRec e1 e2 e3) = _seval_body
-- seval (Tuple e) = _seval_body
-- seval (Proj e1 e2) = _seval_body
-- seval (JNewObj e1 e2) = _seval_body
-- seval (JMethod e1 e2 e3 e4) = _seval_body
-- seval (JField e1 e2 e3) = _seval_body
-- seval (Seq e) = _seval_body
-- seval (Merge e1 e2) = _seval_body
-- seval (Record e) = _seval_body
-- seval (RecordAccess e1 e2) = _seval_body
-- seval (RecordUpdate e1 e2) = _seval_body

propagate :: ExecutionTree -> ExecutionTree -> ExecutionTree -> ExecutionTree
propagate (Exp e) et1 et2 = Fork et1 e et2
propagate (Fork l e r) et1 et2 = Fork (propagate l et1 et2) e (propagate r et1 et2)

-- ($*$) :: SymValue -> SymValue -> SymValue
-- t1 $*$ t2 =
--     case (t1, t2) of
--       (SInt a, SInt b) -> SInt (a*b)
--       (SInt 1, _) -> t2
--       (SInt 0, _) -> t1
--       (_, SInt 1) -> t2
--       (_, SInt 0) -> t1

merge :: (SymValue -> SymValue -> SymValue, Op)
      -> ExecutionTree
      -> ExecutionTree
      -> ExecutionTree
merge (_, ADD) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SInt (a+b)
merge (_, MUL) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SInt (a*b)
merge (_, SUB) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SInt (a-b)
merge (_, DIV) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SInt (a `div` b)

merge (_, EQ) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBool (a==b)
merge (_, EQ) (Exp (SBool a)) (Exp (SBool b)) = Exp $ SBool (a==b)
merge (_, NEQ) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBool (a/=b)
merge (_, NEQ) (Exp (SBool a)) (Exp (SBool b)) = Exp $ SBool (a/=b)
merge (_, LT) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBool (a<b)
merge (_, LTE) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBool (a<=b)
merge (_, GT) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBool (a>b)
merge (_, GTE) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBool (a>=b)

merge (_, OR) (Exp (SBool a)) (Exp (SBool b)) = Exp $ SBool (a||b)
merge (_, AND) (Exp (SBool a)) (Exp (SBool b)) = Exp $ SBool (a&&b)

merge (f, _) (Exp e1) (Exp e2) = Exp (f e1 e2)
merge f (Fork l e r) t = Fork (merge f l t) e (merge f r t)
merge f t (Fork l e r) = Fork (merge f t l) e (merge f t r)


treeApply :: ExecutionTree -> ExecutionTree -> ExecutionTree
treeApply (Exp e) t =
    case e of
      SFVar n -> apply (SApp (SFVar n)) t
      SFun f -> f t
treeApply (Fork l e r) t = Fork (treeApply l t) e (treeApply r t)

apply :: (SymValue -> SymValue) -> ExecutionTree -> ExecutionTree
apply f (Exp e) = Exp (f e)
apply f (Fork l e r) = Fork (apply f l) e (apply f r)

instance Show Value where
    show (VFun _) = "<<func>>"
    show (VInt x) = show x
    show (VBool b) = show b

instance Show Op where
    show ADD = "+"
    show MUL = "*"
    show SUB = "-"
    show DIV = "/"
    show LT = "<"
    show LTE = "<="
    show GT = ">"
    show GTE = ">="
    show EQ = "=="
    show NEQ = "/="
    show OR = "||"
    show AND = "&&"

instance Show SymValue where
    show (SFVar n) = "x" ++ show n
    show (SInt i) = show i
    show (SBool b) = show b
    show (SApp e1 e2) = show e1 ++ " " ++ show e2
    show (SOp op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"
    show (SFun _) = "<<func>>"

instance Show ExecutionTree where
    show e = fst $ pp e "True" 5

pp :: ExecutionTree -> String -> Int -> (String, Int)
pp _ _ 0 = ("", 0)
pp (Exp e) s stop = (s ++ " ==> " ++ show e ++ "\n", stop - 1)
pp (Fork l e r) s stop =
    let s1 = show e
        (s2, stop2) = pp l (s ++ " && " ++ s1) stop
        (s3, stop3) = pp r (s ++ " && " ++ "not (" ++ s1 ++ ")") stop2
    in (s2 ++ s3, stop3)

fun e = pp . exec . seval $ e
