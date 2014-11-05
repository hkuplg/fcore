-- IDEA: AST ~> SBV AST ~> satisfiable
-- goal: 1. satisfiable
--       2. simplify the condition
--       3. find counter-example

-- add Leaf?  Leaf is expression

module SymbolicEvaluator where

import           Core
import qualified Language.Java.Syntax    as J (Op (..))
import           Panic
import           Prelude                 hiding (EQ, GT, LT)
import qualified Src                     as S
import Data.IntSet hiding (map)
import Data.SBV
-- import           PrettyUtils
-- import           Text.PrettyPrint.Leijen

data Value = VInt Integer
           | VBool Bool
           | VFun (Value -> Value)

-- big-step interpreter
eval :: Expr () Value -> Value
eval (Var _ x) = x
eval (Lit x) =
    case x of
      S.Int n -> VInt n
      S.Bool b -> VBool b
      _ -> panic "Lit type unknown"
eval (Lam _ _ f) = VFun (eval . f)
eval (Let _ e f) = eval . f . eval $ e
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

data SymValue = SVar Int -- free variables
              | SInt Integer
              | SBoolean Bool
              | SApp SymValue SymValue
              | SOp Op SymValue SymValue
              | SFun (ExecutionTree -> ExecutionTree)

data Op = ADD
        | MUL
        | SUB
        | DIV
        | LT
        | LE
        | GT
        | GE
        | EQ
        | NEQ
        | OR
        | AND

-- Add index to SVars
exec :: ExecutionTree -> ExecutionTree
exec e = go e 0
    where go (Exp (SFun f)) n = go (f . Exp $ SVar n) (n+1)
          go e n = e

-- symbolic evaluation
seval :: Expr () ExecutionTree -> ExecutionTree
seval (Var _ x) = x
seval (Lit x) =
    case x of
      S.Int n -> Exp $ SInt n
      S.Bool b -> Exp $ SBoolean b

seval (If e1 e2 e3) =
    case e1' of
        Exp (SBoolean True) -> seval e2
        Exp (SBoolean False) -> seval e3
        _ -> propagate e1' (seval e2) (seval e3)
    where e1' = seval e1

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
        S.Compare J.LThanE -> merge (SOp LE, LE) e1' e2'
        S.Compare J.GThan -> merge (SOp GT, GT) e1' e2'
        S.Compare J.GThanE -> merge (SOp GE, GE) e1' e2'

        -- logic operations
        S.Logic J.And -> merge (SOp AND, AND) e1' e2'
        S.Logic J.Or -> merge (SOp OR, OR) e1' e2'

    where e1' = seval e1
          e2' = seval e2

seval (Lam _ _ f) = Exp $ SFun (seval . f)
seval (Let _ e f) = let v = seval e in seval (f v)
seval (App e1 e2) = treeApply (seval e1) (seval e2)
seval (BLam f) =  seval $ f ()
seval (TApp e _) = seval e
seval e = error "Not supported"

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

merge (_, EQ) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBoolean (a==b)
merge (_, EQ) (Exp (SBoolean a)) (Exp (SBoolean b)) = Exp $ SBoolean (a==b)
merge (_, NEQ) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBoolean (a/=b)
merge (_, NEQ) (Exp (SBoolean a)) (Exp (SBoolean b)) = Exp $ SBoolean (a/=b)
merge (_, LT) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBoolean (a<b)
merge (_, LE) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBoolean (a<=b)
merge (_, GT) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBoolean (a>b)
merge (_, GE) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBoolean (a>=b)

merge (_, OR) (Exp (SBoolean a)) (Exp (SBoolean b)) = Exp $ SBoolean (a||b)
merge (_, AND) (Exp (SBoolean a)) (Exp (SBoolean b)) = Exp $ SBoolean (a&&b)

merge (f, _) (Exp e1) (Exp e2) = Exp (f e1 e2)
merge f (Fork l e r) t = Fork (merge f l t) e (merge f r t)
merge f t (Fork l e r) = Fork (merge f t l) e (merge f t r)


treeApply :: ExecutionTree -> ExecutionTree -> ExecutionTree
treeApply (Exp e) t =
    case e of
      SVar n -> apply (SApp (SVar n)) t
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
    show ADD = " + "
    show MUL = " * "
    show SUB = " - "
    show DIV = " / "
    show LT = " < "
    show LE = " <= "
    show GT = " > "
    show GE = " >= "
    show EQ = " == "
    show NEQ = " /= "
    show OR = " || "
    show AND = " && "

instance Show SymValue where
    show (SVar n) = "x" ++ show n
    show (SInt i) = show i
    show (SBoolean b) = show b
    show (SApp e1 e2) = show e1 ++ " " ++ show e2
    show (SOp op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"
    show (SFun _) = "<<func>>"

-- instance Pretty ExecutionTree where
    -- pretty t = prettyTree t

-- prettyTree :: ExecutionTree -> Doc
-- prettyTree t 0 = empty
-- prettyTree (Exp e) stop = imply <+> prettySymValue e <>

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

prettyZ3 :: ExecutionTree -> String -> IntSet -> Int -> [String]
prettyZ3 _ _ _ 0 = []
prettyZ3 (Exp e) s vars stop = [s ++ "\n(check-sat)\n" ++ simplify e ++ "\n(pop)"]
prettyZ3 (Fork l e r) s vars stop =
    let v1 = freeSVars e
        undeclared = v1 `difference` vars
        vars' = vars `union` undeclared
        declared = unlines $ map declare $ toList undeclared
        s1 = s ++ "\n" ++ declared
        s2 = prettyZ3 l (s1 ++ assert e) vars' (stop - 1)
        s3 = prettyZ3 r (s1 ++ assertNeg e) vars' (stop - 1)
    in s2 ++ s3

prettyZ3SymValue :: SymValue -> String
prettyZ3SymValue (SOp op e1 e2) =
    case op of
      ADD -> "(+ " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
      MUL -> "(* " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
      SUB -> "(- " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
      DIV -> "(/ " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
      LT -> "(< " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
      LE -> "(<= " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
      GT -> "(> " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
      GE -> "(>= " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
      EQ -> "(= " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
      NEQ -> "(not (= " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ "))"
      OR -> "(or " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
      AND -> "(and " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
prettyZ3SymValue (SFun _) = error "prettyZ3SymValue SFun"
prettyZ3SymValue e = show e

declare :: Int -> String
declare n = "(declare-const " ++ "x" ++ show n ++ "Int)"

assert, assertNeg :: SymValue -> String
assert e = "(assert " ++ prettyZ3SymValue e ++ ")"
assertNeg e = "(assert (not" ++ prettyZ3SymValue e ++ "))"

freeSVars :: SymValue -> IntSet
freeSVars (SVar i) = singleton i
freeSVars (SOp _ e1 e2) = freeSVars e1 `union` freeSVars e2
freeSVars (SApp e1 e2) = freeSVars e1 `union` freeSVars e2
freeSVars _ = Data.IntSet.empty

simplify :: SymValue -> String
simplify e = "(simplify " ++ prettyZ3SymValue e ++ ")"

fun e = exec . seval $ e
fun' e = mapM_ putStrLn $ prettyZ3 (exec . seval $ e) "(push)" Data.IntSet.empty 6
-- fun' e = mapM_ putStrLn $ prettyZ3 (exec . seval $ e) "(push)" Data.IntSet.empty 6

-- BUG
-- fun' minus_1
-- simplify (- 1 x0) no declaration

-- (check-sat p) when p is empty, there should be no simplify
