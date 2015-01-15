-- IDEA: AST ~> SBV AST ~> satisfiable
-- goal: 1. satisfiable
--       2. simplify the condition
--       3. find counter-example

module SymbolicEvaluator where

import           Core                    hiding (fix)
import qualified Language.Java.Syntax    as J (Op (..))
import           Panic
import           Prelude                 hiding (EQ, GT, LT)
import qualified Src                     as S
import Control.Monad.Fix (fix)
import Data.IntSet hiding (map, foldr)
import Data.Maybe
import Data.List (intercalate)
-- import           PrettyUtils
-- import           Text.PrettyPrint.ANSI.Leijen

data Value = VInt Integer
           | VBool Bool
           | VConstr S.Name [Value]
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
eval (BLam _ f) = eval (f ())
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
               S.Compare J.Equal -> VBool $ a == b
               S.Compare J.NotEq -> VBool $ a /= b
               -- _ -> simplified
         _ -> panic "e1 and e2 should be either Int or Boolean simutaneously"
eval g@(Fix _ _ f _ _) = VFun (\n -> eval $ f (eval g) n)
eval (LetRec _ _ binds body) = eval . body . fix $ map eval . binds
eval (Constr c es) = VConstr (constrName c) (map eval es)
eval (Case e alts) =
    case eval e of
      VConstr n vs -> eval $ fromJust (lookup n table) vs
    where table = map (\(ConstrAlt c _ f) -> (constrName c, f)) alts
eval _ = panic "Can not be evaled"

data ExecutionTree = Exp SymValue
                   -- | Fork ExecutionTree SymValue ExecutionTree
                   | Fork SymValue (Either (ExecutionTree, ExecutionTree) [(Constructor (), [ExecutionTree] -> ExecutionTree)])
                   | NewSymVar Int SymType ExecutionTree

data SymType = TInt
             | TBool
             | TFun [SymType] SymType

data SymValue = SVar Int SymType -- free variables
              | SInt Integer
              | SBool Bool
              | SApp SymValue SymValue
              | SOp Op SymValue SymValue
              | SFun (ExecutionTree -> ExecutionTree) SymType
              | SConstr (Constructor ()) [SymValue]

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
exec :: ExecutionTree -> (ExecutionTree, Int)
exec e = go e 0
    where go (Exp (SFun f t)) i =
              case (go (f . Exp $ SVar i t) (i+1)) of
                (e, i') -> (NewSymVar i t e, i')
          go e i = (e, i)

-- symbolic evaluation
seval :: Expr () ExecutionTree -> ExecutionTree
seval (Var _ x) = x
seval (Lit x) =
    case x of
      S.Int n -> Exp $ SInt n
      S.Bool b -> Exp $ SBool b

seval (If e1 e2 e3) =
    case e1' of
        Exp (SBool True) -> seval e2
        Exp (SBool False) -> seval e3
        -- _ -> propagate e1' (seval e2) (seval e3)
        _ -> propagate e1' (Left (seval e2, seval e3))
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

seval (Lam _ t f) = Exp $ SFun (seval . f) (etype2stype t)
seval (Let _ e f) = seval . f $ seval e
seval (App e1 e2) = treeApply (seval e1) (seval e2)
seval (BLam _ f) =  seval $ f ()
seval (TApp e _) = seval e
seval g@(Fix _ _ f t _) = Exp $ SFun (\n -> seval $ f (seval g) n) (etype2stype t)
seval (LetRec _ _ binds body) = seval . body . fix $ map seval . binds
seval (Constr c es) = mergeList (SConstr c) (map seval es)
seval (Case e alts) = propagate (seval e) (Right (map (\(ConstrAlt c _ f) -> (c, seval . f)) alts))
seval _ = error "seval: not supported"

-- propagate :: ExecutionTree -> ExecutionTree -> ExecutionTree -> ExecutionTree
-- propagate (Exp e) t1 t2 = Fork t1 e t2
-- propagate (Fork l e r) t1 t2 = Fork (propagate l t1 t2) e (propagate r t1 t2)
-- propagate (NewSymVar i typ tree) t1 t2 = NewSymVar i typ (propagate tree t1 t2)

propagate :: ExecutionTree ->
             Either (ExecutionTree, ExecutionTree) [(Constructor (), [ExecutionTree] -> ExecutionTree)] ->
             ExecutionTree
propagate (Exp e) ts = Fork e ts
propagate (Fork e (Left (l,r))) ts' = Fork e (Left (propagate l ts', propagate r ts'))
propagate (Fork e (Right ts)) ts' = Fork e (Right [(c, \es -> propagate (f es) ts')| (c,f) <- ts])
propagate (NewSymVar i typ t) ts = NewSymVar i typ (propagate t ts)

etype2stype :: Type t -> SymType
etype2stype (JClass t) = str2stype t
etype2stype (Fun t1 t2) = TFun [etype2stype t1] (etype2stype t2)
etype2stype _ = error "etype2stype: not supported"

str2stype :: String -> SymType
str2stype "java.lang.Integer" = TInt
str2stype "java.lang.Boolean" = TBool
str2stype _ = error "str2stype: unknown java type"

mergeList :: ([SymValue] -> SymValue) -> [ExecutionTree] -> ExecutionTree
mergeList f [] = Exp (f [])
mergeList f (Exp e : xs) = mergeList (\es -> f (e:es)) xs
mergeList f (Fork e (Left (l, r)) : xs) = Fork e $ Left (mergeList f (l:xs), mergeList f (r:xs))
mergeList f (Fork e (Right ts) : xs) = Fork e $ Right [(c, \es -> mergeList f (g es : xs)) | (c, g) <- ts]
mergeList f (NewSymVar i typ tree : xs) = NewSymVar i typ (mergeList f (tree:xs))

merge :: (SymValue -> SymValue -> SymValue, Op)
      -> ExecutionTree
      -> ExecutionTree
      -> ExecutionTree

merge (_, ADD) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SInt (a+b)
merge (_, ADD) (Exp (SInt 0)) e = e
merge (_, ADD) e (Exp (SInt 0)) = e

merge (_, MUL) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SInt (a*b)
merge (_, MUL) zero@(Exp (SInt 0)) _ = zero
merge (_, MUL) _ zero@(Exp (SInt 0)) = zero
merge (_, MUL) (Exp (SInt 1)) e = e
merge (_, MUL) e (Exp (SInt 1)) = e

merge (_, SUB) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SInt (a-b)
merge (_, SUB) e (Exp (SInt 0)) = e

merge (_, DIV) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SInt (a `div` b)
merge (_, DIV) zero@(Exp (SInt 0)) _ = zero
merge (_, DIV) e (Exp (SInt 1)) = e

merge (_, EQ) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBool (a==b)
merge (_, EQ) (Exp (SBool a)) (Exp (SBool b)) = Exp $ SBool (a==b)

merge (_, NEQ) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBool (a/=b)
merge (_, NEQ) (Exp (SBool a)) (Exp (SBool b)) = Exp $ SBool (a/=b)

merge (_, LT) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBool (a<b)

merge (_, LE) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBool (a<=b)

merge (_, GT) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBool (a>b)

merge (_, GE) (Exp (SInt a)) (Exp (SInt b)) = Exp $ SBool (a>=b)

merge (_, OR) (Exp (SBool a)) (Exp (SBool b)) = Exp $ SBool (a||b)
merge (_, OR) true@(Exp (SBool True)) _ = true
merge (_, OR) _ true@(Exp (SBool True)) = true
merge (_, OR) e (Exp (SBool False)) = e
merge (_, OR) (Exp (SBool False)) e = e

merge (_, AND) (Exp (SBool a)) (Exp (SBool b)) = Exp $ SBool (a&&b)
merge (_, AND) false@(Exp (SBool False)) _ = false
merge (_, AND) _ false@(Exp (SBool False)) = false
merge (_, AND) (Exp (SBool True)) e = e
merge (_, AND) e (Exp (SBool True)) = e

merge (f, _) (Exp e1) (Exp e2) = Exp (f e1 e2)
merge f (Fork e (Left (l,r))) t = Fork e $ Left (merge f l t, merge f r t)
merge f t (Fork e (Left (l,r))) = Fork e $ Left (merge f t l, merge f t r)
merge f (Fork e (Right ts)) t = Fork e $ Right [(c, \es -> merge f (g es) t) | (c,g) <- ts]
merge f t (Fork e (Right ts)) = Fork e $ Right [(c, \es -> merge f t (g es)) | (c,g) <- ts]
-- merge f (Fork l e r) t = Fork (merge f l t) e (merge f r t)
-- merge f t (Fork l e r) = Fork (merge f t l) e (merge f t r)
merge f (NewSymVar i t t1) t2 = NewSymVar i t (merge f t1 t2)
merge f t1 (NewSymVar i typ t2) = NewSymVar i typ (merge f t1 t2)


treeApply :: ExecutionTree -> ExecutionTree -> ExecutionTree
treeApply (Exp e) t =
    case e of
      SVar i typ -> apply (SApp (SVar i typ)) t
      SFun f _ -> f t
-- treeApply (Fork l e r) t = Fork (treeApply l t) e (treeApply r t)
treeApply (Fork e (Left (l,r))) t = Fork e $ Left (treeApply l t, treeApply r t)
treeApply (Fork e (Right ts)) t = Fork e $ Right [(c, \es -> treeApply (f es) t) | (c,f) <- ts]
treeApply (NewSymVar i typ t1) t2 = NewSymVar i typ (treeApply t1 t2)

apply :: (SymValue -> SymValue) -> ExecutionTree -> ExecutionTree
apply f (Exp e) = Exp (f e)
-- apply f (Fork l e r) = Fork (apply f l) e (apply f r)
apply f (Fork e (Left (l,r))) = Fork e $ Left (apply f l, apply f r)
apply f (Fork e (Right ts)) = Fork e $ Right [(c, apply f . g)| (c,g) <- ts]
apply f (NewSymVar i typ t) = NewSymVar i typ (apply f t)

instance Show Value where
    show (VFun _) = "<<func>>"
    show (VInt x) = show x
    show (VBool b) = show b
    show (VConstr n vs) = n ++ " " ++ intercalate " " (map show vs)

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
    show (SVar i _) = "x" ++ show i
    show (SInt i) = show i
    show (SBool b) = show b
    show (SApp e1 e2) = show e1 ++ " " ++ show e2
    show (SOp op e1 e2) = "(" ++ show e1 ++ show op ++ show e2 ++ ")"
    show (SFun _ _) = "<<fun>>"
    show (SConstr c es) = (constrName c) ++ " " ++ intercalate " " (map show es)

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
pp (Fork e (Left (l,r))) s stop =
    let s1 = show e
        (s2, stop2) = pp l (s ++ " && " ++ s1) stop
        (s3, stop3) = pp r (s ++ " && " ++ "not (" ++ s1 ++ ")") stop2
    in (s2 ++ s3, stop3)
pp (Fork e (Right ts)) s stop =
    foldr (\(c,f) (s', stop') ->
               let (s'', stop'') = pp (f []) (s ++ " && " ++ constrName c) stop'
               in (s'++s'', stop''))
       ("", stop)
       ts
-- pp (Fork l e r) s stop =
--     let s1 = show e
--         (s2, stop2) = pp l (s ++ " && " ++ s1) stop
--         (s3, stop3) = pp r (s ++ " && " ++ "not (" ++ s1 ++ ")") stop2
--     in (s2 ++ s3, stop3)
pp (NewSymVar _ _ t) s stop = pp t s stop

-- prettyZ3 :: ExecutionTree -> String -> IntSet -> Int -> [String]
-- prettyZ3 _ _ _ 0 = []
-- prettyZ3 (Exp e) s vars stop = [s ++ "\n(check-sat)\n" ++ simplify e ++ "\n(pop)"]
-- prettyZ3 (Fork l e r) s vars stop =
--     let v1 = freeSVars e
--         undeclared = v1 `difference` vars
--         vars' = vars `union` undeclared
--         declared = unlines $ map declare $ toList undeclared
--         s1 = s ++ "\n" ++ declared
--         s2 = prettyZ3 l (s1 ++ assert e) vars' (stop - 1)
--         s3 = prettyZ3 r (s1 ++ assertNeg e) vars' (stop - 1)
--     in s2 ++ s3

-- prettyZ3SymValue :: SymValue -> String
-- prettyZ3SymValue (SOp op e1 e2) =
--     case op of
--       ADD -> "(+ " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
--       MUL -> "(* " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
--       SUB -> "(- " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
--       DIV -> "(/ " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
--       LT -> "(< " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
--       LE -> "(<= " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
--       GT -> "(> " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
--       GE -> "(>= " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
--       EQ -> "(= " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
--       NEQ -> "(not (= " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ "))"
--       OR -> "(or " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
--       AND -> "(and " ++ prettyZ3SymValue e1 ++ " " ++ prettyZ3SymValue e2 ++ ")"
-- prettyZ3SymValue (SFun _ _) = error "prettyZ3SymValue SFun"
-- prettyZ3SymValue e = show e

-- declare :: Int -> String
-- declare n = "(declare-const " ++ "x" ++ show n ++ "Int)"

-- assert, assertNeg :: SymValue -> String
-- assert e = "(assert " ++ prettyZ3SymValue e ++ ")"
-- assertNeg e = "(assert (not" ++ prettyZ3SymValue e ++ "))"

-- freeSVars :: SymValue -> IntSet
-- freeSVars (SVar i _) = singleton i
-- freeSVars (SOp _ e1 e2) = freeSVars e1 `union` freeSVars e2
-- freeSVars (SApp e1 e2) = freeSVars e1 `union` freeSVars e2
-- freeSVars _ = Data.IntSet.empty

-- simplify :: SymValue -> String
-- simplify e = "(simplify " ++ prettyZ3SymValue e ++ ")"

fun e = fst . exec . seval $ e
-- fun' e = mapM_ putStrLn $ prettyZ3 (exec . seval $ e) "(push)" Data.IntSet.empty 6
