{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- Hindley–Milner type system
-- http://en.wikipedia.org/wiki/Hindley%E2%80%93Milner
-- http://web.cs.wpi.edu/~cs4536/c12/milner-damas_principal_types.pdf
module HM where

import HMSyntax
import HMParser         (readHM)

import Prelude hiding   (id)
import Control.Monad.State
import Data.List        (union, delete, intercalate, nub)
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

rng :: [(a, b)] -> [b]
rng = map snd

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

-- Constraint-based typing (TaPL, Figure 22-1)

type Constraint = (CType, CType) 

-- Types used in constraints, different from `Type`
data CType = CTLit
           | CTVar Var
           | CTArr CType CType -- t1 -> t2
           deriving (Eq, Show)

type Context = [(Var, CType)]

-- The type reconstruction monad
type TR = State Int
 
-- Generate a fresh type variable.
uvargen :: TR CType
uvargen = do n <- get
             put $ n + 1
             return $ CTVar ("a" ++ show n)

ctype :: Context -> Exp -> TR ([Constraint], CType)
ctype ctx (EVar x) = 
    case lookup x ctx of 
        Nothing -> error ("Unbound variable: " ++ x)
        Just t -> return ([], t)

ctype ctx (ELit i) = return ([], CTLit)

ctype ctx (EApp e1 e2) = 
    do (c1, t1) <- ctype ctx e1 
       (c2, t2) <- ctype ctx e2
       x <- uvargen
       return (c1 `union` c2 `union` [(t1, CTArr t2 x)], x)

ctype ctx (ELam x e2) = 
    do t1 <- uvargen -- annotate x with some fresh type variable t1
       (c2, t2) <- ctype (ctx `union` [(x, t1)]) e2
       return (c2, CTArr t1 t2)

ctype ctx (EUn op e1) = ctype ctx e1

ctype ctx (EBin op e1 e2) = 
    do (c1, t1) <- ctype ctx e1
       (c2, t2) <- ctype ctx e2
       let c' = if isLitLitOp op then [(t1, CTLit), (t2, CTLit)] else [(t1, t2)]
       return (c1 `union` c2 `union` c', t1)

ctype ctx (EIf e1 e2 e3) = 
    do (c1, t1) <- ctype ctx e1
       (c2, t2) <- ctype ctx e2
       (c3, t3) <- ctype ctx e3
       return (c1 `union` c2 `union` c3 `union` [(t1, CTLit), (t2, t3)], t2)

infer :: Exp -> CType
infer e = 
    let (c, t) = evalState (ctype [] e) 0 in 
    let s = unify c in
    subst s t

type Substitution = (Var, CType)

fv :: CType -> [Var]
fv CTLit = []
fv (CTVar x) = [x]
fv (CTArr t1 t2) = nub $ (fv t1) ++ (fv t2)

class Subst a where
    subst :: [Substitution] -> a -> a

instance Subst CType where
    subst _s CTLit = CTLit
    subst s (CTVar x) = fromMaybe (CTVar x) (lookup x s)
    subst s (CTArr t1 t2) = CTArr (subst s t1) (subst s t2)

-- Composition of substitution s1 and s2
composeS :: [Substitution] -> [Substitution] -> [Substitution]
composeS s1 s2 = mapping1 ++ mapping2
    where mapping1 = map (\(x, t) -> (x, subst s1 t)) s2
          mapping2 = map (\x -> x) (filter (\(x, t) -> not (x `elem` dom s2)) s1)

substC :: [Substitution] -> [Constraint] -> [Constraint]
substC s c = map (substC0 s) c
    where substC0 s (t1, t2) = (subst s t1, subst s t2)

double :: String
double = "\\f -> (\\x -> f(f(x)))"

unify :: [Constraint] -> [Substitution]
unify [] = []
unify c@((s, t):c')
    | s == t = unify c'
    | otherwise = case (s, t) of
        (CTVar x, _) -> if x `elem` fv t then raise else unify (substC [(x, t)] c') `composeS` [(x, t)] 
        (_, CTVar x) -> if x `elem` fv s then raise else unify (substC [(x, s)] c') `composeS` [(x, s)] 
        (CTArr s1 s2, CTArr t1 t2) -> unify $ c' ++ [(s1, t1), (s2, t2)]
        _ -> raise
    where 
        raise = error $ "Cannot unify " ++ show s ++ " and " ++ show t ++ " given constraints:\n" ++ show c

-- Similar to the ":t" in GHCi
t :: String -> IO ()
t = putStrLn . pretty . infer . readHM 

class Pretty a where 
    pretty :: a -> String

instance Pretty CType where
    pretty CTLit = "lit"
    pretty (CTVar x) = x
    pretty (CTArr t1 t2) = 
        case t1 of 
            CTArr _ _ -> paren (pretty t1) ++ arrow ++ pretty t2
            _         -> pretty t1 ++ arrow ++ pretty t2
            where arrow = " -> "
                  paren s = "(" ++ s ++ ")"

instance Pretty Exp where
    pretty (EVar x) = x
    pretty (ELit i) = show i
    pretty (EApp e0 e1) = "(" ++ pretty e0 ++ " " ++ pretty e1 ++ ")"
    pretty (ELam x e) = "(\\" ++ x ++ " -> " ++ pretty e ++ ")"
    pretty (ELet x e0 e1) = "(let " ++ x ++ " = " ++ pretty e0 ++ " in " ++ pretty e1 ++ ")"
    pretty (ELetRec bindings body) = "(let rec " ++ prettyBindings ++ " in " ++ pretty body ++ ")"
        where prettyBindings = intercalate " and " $ map (\(x, e) -> x ++ " = " ++ pretty e) bindings
    pretty (EUn  op e) = "(" ++ pretty op  ++ pretty e ++ ")"
    pretty (EBin op e1 e2) = "(" ++ pretty e1 ++ " " ++ pretty op ++ " " ++ pretty e2 ++ ")"
    pretty (EIf e0 e1 e2) = "(if " ++ pretty e0 ++ " then " ++ pretty e1 ++ " else " ++ pretty e2 ++ ")"

instance Pretty UnOp where
    pretty UMinus = "-"
    pretty Not    = "!"

instance Pretty BinOp where
    pretty Add = "+" 
    pretty Sub = "-" 
    pretty Mul = "*" 
    pretty Div = "/" 
    pretty Mod = "%"

    pretty Eq = "=="
    pretty Ne = "!="
    pretty Lt = "<"
    pretty Gt = ">"
    pretty Le = "<="
    pretty Ge = ">="

    pretty And = "&&"
    pretty Or = "||"

instance Pretty Type where
    pretty (TMono t) = pretty t
    pretty (TPoly s) = pretty s

instance Pretty Mono where
    pretty (MVar a) = a
    pretty (MPrim Int) = "int"
    pretty (MPrim Bool) = "bool"
    pretty (MApp t0 t1) = "(" ++ pretty t0 ++ " -> " ++ pretty t1 ++ ")"

instance Pretty Poly where
    pretty (PMono t) = pretty t
    pretty (PForall a s) = "(forall " ++ a ++ " . " ++ pretty s ++ ")"
