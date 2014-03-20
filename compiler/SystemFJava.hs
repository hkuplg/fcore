{-# OPTIONS -XRankNTypes -XFlexibleInstances #-}

module SystemFJava where

import Prelude hiding (init)
import Debug.Trace
import Data.List hiding (init)

import Control.Monad.State

import qualified Language.Java.Syntax as J
import Language.Java.Pretty
import Mixins

-- System F syntax

data PFTyp t = FTVar t | FForall (t -> PFTyp t) | FFun (PFTyp t) (PFTyp t) | PFInt

type PrimLit = Integer -- later maybe Bool | Char

data PFExp t e = 
     FVar e 
   | FBLam (t -> PFExp t e) 
   | FLam (PFTyp t) (e -> PFExp t e) 
   | FApp (PFExp t e) (PFExp t e)
   | FTApp (PFExp t e) (PFTyp t)
   | FPrimOp (PFExp t e) (J.Op) (PFExp t e) -- SystemF extension from: https://www.cs.princeton.edu/~dpw/papers/tal-toplas.pdf (no int restriction)
   | FLit PrimLit
   | Fif0 (PFExp t e) (PFExp t e) (PFExp t e)
   | FTuple [PFExp t e]
   | FProj Int (PFExp t e)
   
-- Closure F syntax

data Scope b t e = Body b | Kind (t -> Scope b t e) | Typ (PCTyp t) (e -> Scope b t e) 

type TScope t = Scope (PCTyp t) t ()

type EScope t e = Scope (PCExp t e) t e

data PCTyp t = CTVar t | CForall (TScope t) | CInt | CTupleType [PCTyp t]

data PCExp t e = 
     CVar e 
   | CFVar Int
   | CLam (EScope t e)  
   | CApp (PCExp t e) (PCExp t e)
   | CTApp (PCExp t e) (PCTyp t)
   | CFPrimOp (PCExp t e) (J.Op) (PCExp t e)
   | CFLit PrimLit
   | CFif0 (PCExp t e) (PCExp t e) (PCExp t e)
   | CFTuple [PCExp t e]
   | CFProj Int (PCExp t e)

-- System F to Closure F

ftyp2scope :: PFTyp t -> TScope t
ftyp2scope (FForall f)   = Kind (\a -> ftyp2scope (f a))
ftyp2scope (FFun t1 t2)  = Typ (ftyp2ctyp t1) (\x -> ftyp2scope t2)
ftyp2scope t             = Body (ftyp2ctyp t)
-- ftyp2scope PFInt         = Body CInt
-- ftyp2scope (FTVar x)     = Body (CTVar x)

ftyp2ctyp :: PFTyp t -> PCTyp t
ftyp2ctyp (FTVar x) = CTVar x
ftyp2ctyp (PFInt)     = CInt
ftyp2ctyp t         = CForall (ftyp2scope t)

fexp2cexp :: PFExp t e -> PCExp t e
fexp2cexp (FVar x)      = CVar x
fexp2cexp (FApp e1 e2)  = CApp (fexp2cexp e1) (fexp2cexp e2)
fexp2cexp (FTApp e t)   = CTApp (fexp2cexp e) (ftyp2ctyp t)
fexp2cexp (FPrimOp e1 op e2) = CFPrimOp (fexp2cexp e1) op (fexp2cexp e2)
fexp2cexp (FLit e) = CFLit e
fexp2cexp (Fif0 e1 e2 e3) = CFif0 (fexp2cexp e1) (fexp2cexp e2) (fexp2cexp e3)
fexp2cexp (FTuple tuple) = CFTuple (map fexp2cexp tuple)
fexp2cexp (FProj i e) = CFProj i (fexp2cexp e)
fexp2cexp e             = CLam (groupLambda e)

groupLambda :: PFExp t e -> EScope t e
groupLambda (FBLam f)  = Kind (\a -> groupLambda (f a))
groupLambda (FLam t f) = Typ (ftyp2ctyp t) (\x -> groupLambda (f x))
groupLambda e          = Body (fexp2cexp e)

-- type inference for Closure F

infer = joinPCTyp . inferExp

inferExp :: PCExp (PCTyp t) (PCTyp t) -> PCTyp (PCTyp t)
inferExp (CVar t) = CTVar t
inferExp (CTApp e t) =
  case inferExp e of 
     (CForall (Kind f)) -> scope2ctyp (f (joinPCTyp t))
inferExp (CApp e1 e2) = 
  case (inferExp e1, inferExp e2) of 
     (CForall (Typ t1 f),t3) -> scope2ctyp (f ())
inferExp (CLam s) = CForall (inferScope s)

-- inferScope :: EScope (PCTyp t e) (PCTyp t e) -> TScope (PCTyp t e) (PCTyp t e)
inferScope (Body e)   = Body (inferExp e)
inferScope (Kind f)   = Kind (\a -> inferScope (f a))
inferScope (Typ t f)  = Typ t (\_ -> inferScope (f (joinPCTyp t)))

scope2ctyp :: TScope t -> PCTyp t
scope2ctyp (Body t)  = t
scope2ctyp s         = CForall s

joinPCTyp :: PCTyp (PCTyp t) -> PCTyp t
joinPCTyp (CTVar t)   = t
joinPCTyp (CForall s) = CForall (joinTScope s)
joinPCTyp CInt = CInt
joinPCTyp (CTupleType ts) = CTupleType (map joinPCTyp ts)

joinTScope :: TScope (PCTyp t) -> TScope t
joinTScope (Body b)   = Body (joinPCTyp b)
joinTScope (Kind f)   = Kind (joinTScope . f . CTVar)
joinTScope (Typ t f)  = let t' = (joinPCTyp t) in Typ t' (\x -> joinTScope (f x))

-- Closure F to Java

var x = J.ExpName (J.Name [J.Ident x])

jbody = Just (J.Block [])

init = [J.InitDecl False (J.Block [])]

jexp init body = J.InstanceCreation [] (J.ClassType [(J.Ident "Closure",[])]) [] 
       (Just (J.ClassBody (init ++  [
          J.MemberDecl (J.MethodDecl [] [] Nothing (J.Ident "apply") [] [] (J.MethodBody body))
       ])))

closureType = J.RefType (J.ClassRefType (J.ClassType [(J.Ident "Closure",[])]))

comparezero :: J.Exp -> J.Exp
comparezero jexp = J.BinOp jexp J.Equal (J.Lit $ J.Int 0)

ifBody :: ([J.BlockStmt], [J.BlockStmt]) -> (J.Exp, J.Exp, J.Exp) -> Int -> (J.BlockStmt, J.Exp)
ifBody (s2, s3) (j1, j2, j3) n = (J.BlockStmt $ J.IfThenElse (comparezero j1) (J.StmtBlock $ J.Block (s2 ++ j2Stmt)) (J.StmtBlock $ J.Block (s3 ++ j3Stmt)), newvar)
    where
        j2Stmt = [(J.LocalVars [] (J.RefType (refType "Object")) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Just (J.InitExp (J.Cast (J.RefType (refType "Object")) j2)))]))]
        j3Stmt = [(J.LocalVars [] (J.RefType (refType "Object")) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Just (J.InitExp (J.Cast (J.RefType (refType "Object")) j3)))]))]
        ifvarname = ("ifres" ++ show n)
        refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
        newvar = var ifvarname

createCU :: (J.Block, J.Exp, t) -> (J.CompilationUnit, t)
createCU (J.Block bs,e,t) = (cu,t) where
   cu = J.CompilationUnit Nothing [] [closureClass, classDecl]
   field name = J.MemberDecl (J.FieldDecl [] (J.RefType (refType "Object")) [
              J.VarDecl (J.VarId (J.Ident name)) Nothing])
   app mod b = J.MemberDecl (J.MethodDecl mod [] Nothing (J.Ident "apply") [] [] (J.MethodBody b))
   closureClass = J.ClassTypeDecl (J.ClassDecl [J.Abstract] (J.Ident "Closure") [] Nothing [] (
                  J.ClassBody [field "x",field "out",app [J.Abstract] Nothing]))
   body = Just (J.Block (bs ++ [ass]))
   ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA e))
   refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
   classDecl = J.ClassTypeDecl (J.ClassDecl [] (J.Ident "MyClosure") [] (Just (refType "Closure")) [] (J.ClassBody [app [] body]))

reduceTTuples :: [([a], J.Exp, PCTyp t)] -> ([a], J.Exp, PCTyp t)
reduceTTuples all = (merged, arrayAssignment, tupleType)
    where
        merged = concat $ map (\x -> case x of (a,b,c) -> a) all
        arrayAssignment = J.ArrayCreateInit (J.RefType (refType "Object")) 1 (J.ArrayInit (map (\x -> case x of (a,b,c) -> J.InitExp b) all))
        tupleType = CTupleType (map (\x -> case x of (a,b,c) -> c) all)
        refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])

-- Free variable substitution

substScope n t (Body t1) = Body (substType n t t1)
substScope n t (Kind f)  = Kind (\a -> substScope n t (f a))
substScope n t (Typ t1 f) = Typ (substType n t t1) (\x -> substScope n t (f x))

substType n t (CTVar x) = subst n t x
substType n t (CForall s) = CForall (substScope n t s)
substType n t x = x

class Subst t where
   subst :: Int -> PCTyp Int -> t -> PCTyp t

instance Subst Int where
   subst n t x 
      | n == x = t
      | otherwise = CTVar x

instance Subst t => Subst (PCTyp t) where
   subst n t x = CTVar (substType n t x)

-- main translation function
data Translate = T {
  translateM :: 
     PCExp Int (Int, PCTyp Int) -> 
     State Int ([J.BlockStmt], J.Exp, PCTyp Int),
  translateScopeM :: 
    Scope (PCExp Int (Int, PCTyp Int)) Int (Int, PCTyp Int) -> 
    State Int ([J.BlockStmt], J.Exp, TScope Int)
  }

trans :: Open Translate
trans this = T {
  translateM = \e -> case e of 
     CVar (i,t) -> 
       return ([],var ("x" ++ show i ++ ".x"), t)
     
     CFLit e    -> 
       return ([],J.Lit $ J.Int e, CInt)
     
     CFPrimOp e1 op e2 ->
       do  (s1,j1,t1) <- translateM this e1
           (s2,j2,t2) <- translateM this e2
           return (s1 ++ s2, J.BinOp j1 op j2, t1)
           
     CFif0 e1 e2 e3 ->
       do  n <- get
           put (n+1)
           (s1,j1,t1) <- translateM this e1
           (s2,j2,t2) <- translateM this e2
           (s3,j3,t3) <- translateM this e3
           let  (ifstmt, ifexp) = ifBody (s2, s3) (j1, j2, j3) n  -- uses a fresh variable
           return (s1 ++ [ifstmt], ifexp, t2)                     -- need to check t2 == t3
           
     CFTuple tuple ->
       liftM reduceTTuples $ mapM (translateM this) tuple
       
     CFProj i (CFTuple tuple) ->
       translateM this (tuple!!i)
       
     CTApp e t -> 
       do  n <- get
           (s,je, CForall (Kind f)) <- translateM this e
           return (s,je, scope2ctyp (substScope n t (f n)))
           
     CLam s ->
       do  (s,je, t) <- translateScopeM this s
           return (s,je, CForall t)
           
     CApp e1 e2 ->
       do  n <- get
           put (n+1)
           (s1,j1, CForall (Typ t1 g)) <- translateM this e1
           (s2,j2,t2) <- translateM this e2
           let t    = g ()
           let f    = J.Ident ("x" ++ show n) -- use a fresh variable
           let cvar = J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp (J.Cast closureType j1)))])
           let ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "x"))) J.EqualA j2) ) 
           let apply = J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall (J.ExpName (J.Name [f])) [] (J.Ident "apply") [])))
           let s3 = case t of -- checking the type whether to generate the apply() call
                       Body _ -> [cvar,ass,apply]
                       _ -> [cvar,ass]
           let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
           return (s1 ++ s2 ++ s3, j3, scope2ctyp t), -- need to check t1 == t2
  translateScopeM = \e -> case e of 
      Body t ->
        do  (s,je, t1) <- translateM this t
            return (s,je, Body t1)
          
      Kind f -> 
        do  n <- get
            put (n+1) -- needed? 
            (s,je,t1) <- translateScopeM this (f n)
            return (s,je, Kind (\a -> substScope n (CTVar a) t1)) 
            
      Typ t f ->
        do  n <- get
            put (n+2)
            (s,je,t1) <- translateScopeM this (f (n+1,t))
            let f    = J.Ident ("x" ++ show n) -- use a fresh variable
            let self = J.Ident ("x" ++ show (n+1)) -- use another fresh variable
            let cvar = refactoredScopeTranslationBit je self s f
            return ([cvar],J.ExpName (J.Name [f]), Typ t (\_ -> t1) )
  }

-- Naive translation

transNaive = new trans

translate = translateM transNaive

translateScope = translateM transNaive

-- seperating (hopefully) the important bit

refactoredScopeTranslationBit :: J.Exp -> J.Ident -> [J.BlockStmt] -> J.Ident -> J.BlockStmt
refactoredScopeTranslationBit javaExpression idCurrentName statementsBeforeOA idNextName = completeClosure
    where
        outputAssignment = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA  javaExpression))
        fullAssignment = J.InitDecl False (J.Block [(J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA
                                                    (pullupClosure statementsBeforeOA))))])
        currentInitialDeclaration = J.MemberDecl $ J.FieldDecl [] closureType [J.VarDecl (J.VarId idCurrentName) (Just (J.InitExp J.This))]
        completeClosure | checkExp javaExpression  = J.LocalVars [] closureType [J.VarDecl (J.VarId idNextName) 
                                                (Just (J.InitExp 
                                                    (jexp [currentInitialDeclaration] (Just (J.Block (statementsBeforeOA ++ [outputAssignment]))))
                                                    )
                                                )]
                        | otherwise = J.LocalVars [] closureType [J.VarDecl (J.VarId idNextName) 
                                                (Just (J.InitExp 
                                                    (jexpOutside [currentInitialDeclaration,fullAssignment]
                                                    )
                                                ))]

checkExp :: J.Exp -> Bool
checkExp (J.ExpName (J.Name [J.Ident f])) = '.' `elem` f
checkExp x = True

jexpOutside init = J.InstanceCreation [] (J.ClassType [(J.Ident "Closure",[])]) [] 
       (Just (J.ClassBody (init ++  [
          J.MemberDecl (J.MethodDecl [] [] Nothing (J.Ident "apply") [] [] (J.MethodBody (Just(J.Block $ []))))
       ])))

pullupClosure [J.LocalVars [] rf vd] = case vd of
                                [J.VarDecl variableId (Just(J.InitExp exp))] -> exp


-- Pretty Printing

showPCTyp :: PCTyp Int -> Int -> String
showPCTyp (CTVar i) n = "a" ++ show i
showPCTyp (CForall s) n = "(forall " ++ showTScope s n ++ ")"
showPCTyp CInt n = "Int"

showTScope (Body t) n = ". " ++ showPCTyp t n
showTScope (Kind f) n = "a" ++ show n ++ " " ++ showTScope (f n) (n+1)
showTScope (Typ t f) n = "(_ : " ++ showPCTyp t n ++ ") " ++ showTScope (f ()) (n)

showEScope (Body t) n = ". " ++ showPCExp t n
showEScope (Kind f) n = "a" ++ show n ++ " " ++ showEScope (f n) (n+1)
showEScope (Typ t f) n = "(x" ++ show n ++ " : " ++ showPCTyp t n ++ ") " ++ showEScope (f n) (n+1)

showPCExp (CVar x) n      = "x" ++ show x
showPCExp (CLam s) n      = "(\\" ++ showEScope s n ++ ")"
showPCExp (CApp e1 e2) n  = showPCExp e1 n ++ " " ++ showPCExp e2 n
showPCExp (CTApp e t) n   = showPCExp e n ++ " " ++ showPCTyp t n

instance Show (PCTyp Int) where
   show e = showPCTyp e 0

instance Show (TScope Int) where
   show e = showTScope e 0

instance Show (EScope Int Int) where
   show e = showEScope e 0

instance Show (PCExp Int Int) where
   show e = showPCExp e 0

prettyJ :: Pretty a => a -> IO ()
prettyJ = putStrLn . prettyPrint

compile e = 
  case evalState (translate (fexp2cexp e)) 0 of
      (ss,exp,t) -> (J.Block ss,exp, t)

compilePretty e = let (b,exp,t) = compile e in (prettyJ b >> prettyJ exp >> putStrLn (show t))

compileCU e = let (cu,t) = createCU $ compile e in (prettyJ cu >> putStrLn (show t))
