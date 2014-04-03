{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- Hindley–Milner type system
-- http://en.wikipedia.org/wiki/Hindley%E2%80%93Milner
-- http://web.cs.wpi.edu/~cs4536/c12/milner-damas_principal_types.pdf
module HM where

import HMSyntax
import HMParser         (readHM)

import Prelude hiding   (id)
import Control.Monad.State
import Data.List        (union, delete, intercalate)
import Data.Maybe       (fromMaybe)

evenOdd :: String
evenOdd = "let rec even = \\n -> n == 0 || odd (n-1) and odd = \\n -> if n == 0 then 0 else even (n-1) in odd 10"

true :: Poly
true = PForall "a" (PForall "b" (PMono (MVar "a")))

false :: Poly
false = PForall "a" (PForall "b" (PMono (MVar "b")))

-- We should have: true == true'
true' :: Poly
true' = PForall "b" (PForall "a" (PMono (MVar "b")))

type Env = [(Var, Poly)]

-- Free Type Variables
free :: Type -> [TVar]
free (TMono (MVar a)) = [a]
free (TMono (MPrim _)) = []
free (TMono (MApp t1 t2)) = free (TMono t1) `union` free (TMono t2)
free (TPoly (PForall a poly)) = delete a (free (TPoly poly))
free (TPoly (PMono t)) = free (TMono t)

freeEnv :: Env -> [TVar]
freeEnv env = concatMap (\(x,s) -> free (TPoly s)) env

type TSubst = [(TVar, Mono)]   

dom :: [(a, b)] -> [a]
dom = map fst

range :: [(a, b)] -> [b]
range = map snd

-- TaPL, p. 318; Damas & Milner '82, p. 3
tsubstMono :: TSubst -> Mono -> Mono
tsubstMono s (MVar x) = fromMaybe (MVar x) (lookup x s)
tsubstMono s (MPrim t) = MPrim t
tsubstMono s (MApp t1 t2) = MApp (tsubstMono s t1) (tsubstMono s t2)

-- TODO: implement
isLessSpecialThan :: Poly -> Poly -> Bool
isLessSpecialThan s1 s2 = error "isLessSpecialThan"

-- Declarative Rule System
typeExp :: Env -> Exp -> Maybe Type
typeExp _env (EVar x) = Just $ TMono (MVar "a") -- lookup x env >>= Just . TPoly
typeExp env (EApp e0 e1) = do
    t0 <- typeExp env e0
    t1 <- typeExp env e1
    case t0 of 
        (TMono (MApp t t')) -> if t1 == TMono t then Just (TMono t') else Nothing
        _ -> Nothing
typeExp env (ELam x e) = do
    t  <- typeExp env (EVar x)
    t' <- typeExp ((x, type2poly t):env) e
    return $ TMono (MApp (type2mono t) (type2mono t'))
typeExp env (ELet x e0 e1) = do
    t0 <- typeExp env e0
    typeExp ((x, type2poly t0):env) e1
-- TODO: missing two cases: [Inst] and [Gen]

type2poly :: Type -> Poly
type2poly (TMono t) = PMono t
type2poly (TPoly s) = s

type2mono :: Type -> Mono
type2mono (TMono t) = t
type2mono (TPoly s) = error "type2mono (TPoly s)"

-- Syntactical Rule System
-- g(tau) = forall a. tau, a = free(t) - free(env)
-- generalize :: Env -> Mono -> Poly
-- quantifies all monotype variables not bound in env

id :: Exp
id = ELet "id" (ELam "x" (EVar "x")) (EVar "id")

foo :: Exp
foo = ELam "y" (EVar "x")

bar :: Exp
bar = ELam "x" (ELet "foo" foo (EVar "foo"))

example1 :: Exp
example1 = ELet "bar" bar (EVar "bar")

prettyExp :: Exp -> String
prettyExp (EVar x) = x
prettyExp (ELit i) = show i
prettyExp (EApp e0 e1) = "(" ++ prettyExp e0 ++ " " ++ prettyExp e1 ++ ")"
prettyExp (ELam x e) = "(\\" ++ x ++ " -> " ++ prettyExp e ++ ")"
prettyExp (ELet x e0 e1) = "(let " ++ x ++ " = " ++ prettyExp e0 ++ " in " ++ prettyExp e1 ++ ")"
prettyExp (ELetRec bindings body) = "(let rec " ++ prettyBindings ++ " in " ++ prettyExp body ++ ")"
    where prettyBindings = intercalate " and " $ map (\(x, e) -> x ++ " = " ++ prettyExp e) bindings
prettyExp (EUn  op e) = "(" ++ prettyUnOp op  ++ prettyExp e ++ ")"
prettyExp (EBin op e1 e2) = "(" ++ prettyExp e1 ++ " " ++ prettyBinOp op ++ " " ++ prettyExp e2 ++ ")"
prettyExp (EIf e0 e1 e2) = "(if " ++ prettyExp e0 ++ " then " ++ prettyExp e1 ++ " else " ++ prettyExp e2 ++ ")"

prettyUnOp :: UnOp -> String
prettyUnOp UMinus = "-"
prettyUnOp Not    = "!"

prettyBinOp :: BinOp -> String
prettyBinOp Add = "+" 
prettyBinOp Sub = "-" 
prettyBinOp Mul = "*" 
prettyBinOp Div = "/" 
prettyBinOp Mod = "%"

prettyBinOp Eq = "=="
prettyBinOp Ne = "!="
prettyBinOp Lt = "<"
prettyBinOp Gt = ">"
prettyBinOp Le = "<="
prettyBinOp Ge = ">="

prettyBinOp And = "&&"
prettyBinOp Or = "||"

prettyType :: Type -> String
prettyType (TMono t) = prettyMono t
prettyType (TPoly s) = prettyPoly s

prettyMono :: Mono -> String
prettyMono (MVar a) = a
prettyMono (MPrim Int) = "int"
prettyMono (MPrim Bool) = "bool"
prettyMono (MApp t0 t1) = "(" ++ prettyMono t0 ++ " -> " ++ prettyMono t1 ++ ")"

prettyPoly :: Poly -> String
prettyPoly (PMono t) = prettyMono t
prettyPoly (PForall a s) = "(forall " ++ a ++ " . " ++ prettyPoly s ++ ")"

-- Constraint-based typing (TaPL, Figure 22-1)

type Constraint = (CType, CType) 

-- Types used in constraints, different from `Type`
data CType = CTLit
           | CTVar Var
           | CTArr CType CType -- t1 -> t2
           deriving (Eq, Show)

type Context = [(Var, CType)]

type TcMonad = State Int
 
-- Generate a fresh type variable.
uvargen :: TcMonad CType
uvargen = do n <- get
             put $ n + 1
             return $ CTVar ("a" ++ show n)

ctype :: Context -> Exp -> TcMonad ([Constraint], CType)
ctype ctx (EVar x) = 
    case lookup x ctx of 
        Nothing -> return $ error ("Unbound variable: " ++ x)
        Just t -> return ([], t)

ctype ctx (ELit i) = return ([], CTLit)

ctype ctx (EApp e1 e2) = 
    do (c1, t1) <- ctype ctx e1 
       (c2, t2) <- ctype ctx e2
       x <- uvargen
       return (c1 ++ c2 ++ [(t1, CTArr t2 x)], x)

ctype ctx (ELam x e2) = 
    do t1 <- uvargen -- annotate x with some fresh type variable t1
       (c2, t2) <- ctype (ctx ++ [(x, t1)]) e2
       return (c2, CTArr t1 t2)

ctype ctx (EUn op e1) = ctype ctx e1

ctype ctx (EBin op e1 e2) = 
    do (c1, t1) <- ctype ctx e1
       (c2, t2) <- ctype ctx e2
       return (c1 ++ c2 ++ [(t1, t2)], t1)

ctype ctx (EIf e1 e2 e3) = 
    do (c1, t1) <- ctype ctx e1
       (c2, t2) <- ctype ctx e2
       (c3, t3) <- ctype ctx e3
       return (c1 ++ c2 ++ c3 ++ [(t1, CTLit), (t2, t3)], t2)

-- HM expression to constraints listed one per line
testctype :: String -> IO ()
testctype s = 
    let e = readHM s in
    let (c, _t) = evalState (ctype [] e) 0 in
    putStrLn $ intercalate "\n" $ map show $ c
