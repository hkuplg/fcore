{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- Hindley–Milner type system
-- http://en.wikipedia.org/wiki/Hindley%E2%80%93Milner
-- http://web.cs.wpi.edu/~cs4536/c12/milner-damas_principal_types.pdf
module HM where

import HMSyntax
import HMParser         (readHM)

import Prelude hiding (id)
import Data.Maybe       (fromMaybe)
import Data.List        (union, delete, intercalate)

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

-- Type constraints
-- See http://cs.brown.edu/courses/cs173/2012/book/types.html 
-- "15.3.2.1 Constraint Generation"
type Constraint = (ConstraintTerm, ConstraintTerm) 

-- A term in a type constraint
data ConstraintTerm = CTExp Exp
                    | CTVar Var
                    | CTLit
                    | CTArr ConstraintTerm ConstraintTerm
                    deriving (Eq, Show)

generateConstraints :: [Constraint] -> Exp -> [Constraint]
generateConstraints c e = newConstraints e ++ c
    where 
        newConstraints e = 
            case e of
                EVar x -> [(CTExp e, CTVar x)]
                ELit i -> [(CTExp e, CTLit)]
                EApp f a -> [(CTExp f, CTArr (CTExp a) (CTExp e))]
                ELam x body -> [(CTExp e, CTArr (CTVar x) (CTExp body))]
                ELet x e0 body -> [(CTVar x, CTExp e0), (CTExp e, CTExp body)]
                ELetRec bindings body -> map (\(x, e0) -> (CTVar x, CTExp e0)) bindings ++ [(CTExp e, CTExp body)]
                EUn op e1     -> [(CTExp e1, CTLit), (CTExp e, CTLit)]
                EBin op e1 e2 -> [(CTExp e1, CTLit), (CTExp e2, CTLit), (CTExp e, CTLit)]
                EIf e0 e1 e2 -> [(CTExp e0, CTLit), (CTExp e1, CTExp e2), (CTExp e, CTExp e1)]