{-# OPTIONS -XRankNTypes -XFlexibleInstances #-}

module TransCFJava where

import Prelude hiding (init, last)
import Debug.Trace
import Data.List hiding (init, last)

import Control.Monad.State

import qualified Language.Java.Syntax as J
import Language.Java.Pretty
import ClosureF
import Mixins
import Data.Char
import qualified Data.Map as Map

-- Closure F to Java

localVarPrefs = ["ifres", "inttemp"]

var x = J.ExpName (J.Name [J.Ident x])

jbody = Just (J.Block [])

init = [J.InitDecl False (J.Block [])]

jexp init body = J.InstanceCreation [] (J.ClassType [(J.Ident "Closure",[])]) [] 
       (Just (J.ClassBody (init ++  [
          J.MemberDecl (J.MethodDecl [] [] Nothing (J.Ident "apply") [] [] (J.MethodBody body))
       ])))

closureType = J.RefType (J.ClassRefType (J.ClassType [(J.Ident "Closure",[])]))


ifBody :: ([J.BlockStmt], [J.BlockStmt]) -> (J.Exp, J.Exp, J.Exp) -> Int -> (J.BlockStmt, J.Exp)
ifBody (s2, s3) (j1, j2, j3) n = (J.BlockStmt $ J.IfThenElse (j1) (J.StmtBlock $ J.Block (s2 ++ j2Stmt)) (J.StmtBlock $ J.Block (s3 ++ j3Stmt)), newvar)
    where
        j2Stmt = [(J.LocalVars [] (J.RefType (refType "")) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Just (J.InitExp (J.Cast (J.RefType (refType "Object")) j2)))]))]
        j3Stmt = [(J.LocalVars [] (J.RefType (refType "")) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Just (J.InitExp (J.Cast (J.RefType (refType "Object")) j3)))]))]
        ifvarname = (localVarPrefs!!0 ++ show n)
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

boxedIntType = J.RefType (J.ClassRefType (J.ClassType [(J.Ident "Integer",[])]))

genOp :: J.Exp -> J.Op -> J.Exp -> Map.Map J.Exp J.Exp -> [Char] -> [Char] -> ([J.BlockStmt], J.Exp, Map.Map J.Exp J.Exp)
genOp j1 op j2 m temp1 temp2 = (casts1 ++ casts2, J.BinOp maybeCasted1 op maybeCasted2, Map.union mf1 mf2)
    where
        (c1, exp1, m1) = case (Map.lookup j1 m) of Just e -> ([], e, m)
                                                   Nothing -> ([defV1], (var temp1), Map.insert j1 (var temp1) m)
        defV1 = J.LocalVars [] (boxedIntType) ([J.VarDecl (J.VarId $ J.Ident temp1) (Just (J.InitExp $ J.Cast boxedIntType j1))])
        (c2, exp2, m2) = case (Map.lookup j2 m1) of Just e -> ([], e, m)
                                                    Nothing -> ([defV2], (var temp2), Map.insert j2 (var temp2) m1)
        defV2 = J.LocalVars [] (boxedIntType) ([J.VarDecl (J.VarId $ J.Ident temp2) (Just (J.InitExp $ J.Cast boxedIntType j2))])        
        (casts1, maybeCasted1, mf1) = case j1 of J.Lit e -> ([], j1, m)
                                                 _ -> (c1, exp1, m1)
        (casts2, maybeCasted2, mf2) = case j2 of J.Lit e -> ([], j2, m)
                                                 _ -> (c2, exp2, m2)

type Var = Either Int Int -- left -> standard variable; right -> recursive variable

-- main translation function
data Translate = T {
  translateM :: 
     (PCExp Int (Var, PCTyp Int), Map.Map J.Exp J.Exp) ->
     State Int ([J.BlockStmt], J.Exp, PCTyp Int, Map.Map J.Exp J.Exp),
  translateScopeM :: 
    Scope (PCExp Int (Var, PCTyp Int)) Int (Var, PCTyp Int) -> 
    Maybe (Int,PCTyp Int) ->
    State Int ([J.BlockStmt], J.Exp, TScope Int)
  }

trans :: Open Translate
trans this = T {
           
  translateM = \e -> case e of 
     (CVar (Left i,t), m) -> -- non-recursive variable
       return ([],var ("x" ++ show i ++ ".x"), t, m)
     
     (CVar (Right i, t), m) -> -- recursive variable
       return ([],var ("x" ++ show i), t, m)
     
     (CFLit e, m)    -> 
       return ([],J.Lit $ J.Int e, CInt, m)
     
     (CFPrimOp e1 op e2, m) ->
       do  n <- get
           put (n+2)
           (s1,j1,t1, m1) <- translateM this (e1, m)
           (s2,j2,t2, m2) <- translateM this (e2, m1)
           let temp1 = localVarPrefs!!1 ++ show (n+1)
           let temp2 = localVarPrefs!!1 ++ show (n+2)    
           let (s, j, mf) = genOp j1 op j2 m2 temp1 temp2
           return (s1 ++ s2 ++ s, j, t1, mf)
           
     (CFif0 e1 e2 e3, m) ->
       do  n <- get
           put (n+1)
           (s1,j1,t1, m1) <- translateM this ((CFPrimOp e1 J.Equal (CFLit 0)), m)
           (s2,j2,t2, m2) <- translateM this (e2, m1)
           (s3,j3,t3, m3) <- translateM this (e3, m2)
           let ifvarname = (localVarPrefs!!0 ++ show n)
           let refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
           let ifresdecl = J.LocalVars [] (J.RefType (refType "Object")) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Nothing)])
           let  (ifstmt, ifexp) = ifBody (s2, s3) (j1, j2, j3) n  -- uses a fresh variable        
           return (s1 ++ [ifresdecl,ifstmt], ifexp, t2, Map.empty)   
           
     (CFTuple tuple, m) ->
       error("not working")--(liftM reduceTTuples $ mapM (translateM this) tuple, m)
       
     (CFProj i (CFTuple tuple), m) ->
       translateM this (tuple!!i, m)
     
     (CTApp e t, m) -> 
       do  n <- get
           (s,je, CForall (Kind f), m1) <- translateM this (e, m)
           return (s,je, scope2ctyp (substScope n t (f n)), m1)
     
     (CLam s, m) ->
       do  (s,je, t) <- translateScopeM this s Nothing
           return (s,je, CForall t, m)
     
     (CFix t s, m)   -> 
       do  n <- get
           put (n+1)
           (s, je, t') <- translateScopeM this (s (Right n,t)) (Just (n,t)) -- weird!
           return (s,je, CForall t', m)
           
     (CApp e1 e2, m) ->
       do  n <- get
           put (n+1)
           (s1,j1, CForall (Typ t1 g), m1) <- translateM this (e1, m)
           -- DEBUG
           -- (s1,j1, debug) <- translateM this e1
           -- (CForall (Typ t1 g)) <- trace ("C:" ++ show debug) (return debug)
           -- END DEBUG
           (s2,j2,t2, m2) <- translateM this (e2, m1)
           let t    = g ()
           let f    = J.Ident ("x" ++ show n) -- use a fresh variable
           let cvar = J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp (J.Cast closureType j1)))])
           let ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "x"))) J.EqualA j2) ) 
           let apply = J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall (J.ExpName (J.Name [f])) [] (J.Ident "apply") [])))
           let s3 = case t of -- checking the type whether to generate the apply() call
                       Body _ -> [cvar,ass,apply]
                       _ -> [cvar,ass]
           let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
           return (s1 ++ s2 ++ s3, j3, scope2ctyp t, m2), -- need to check t1 == t2
  
  translateScopeM = \e m -> case e of 
      Body t ->
        do  (s,je, t1, m1) <- translateM this (t, Map.empty)
            return (s,je, Body t1)
          
      Kind f -> 
        do  n <- get
            put (n+1) -- needed? 
            (s,je,t1) <- translateScopeM this (f n) m
            return (s,je, Kind (\a -> substScope n (CTVar a) t1)) 
          
      Typ t g -> 
        do  n <- get
            let f    = J.Ident ("x" ++ show n) -- use a fresh variable
            case m of -- Consider refactoring later?
              Just (i,t') | last (g (Right i,t')) ->
                do  put (n+1)
                    let self = J.Ident ("x" ++ show i)
                    (s,je,t1) <- translateScopeM this (g (Left i,t)) m
                    let cvar = refactoredScopeTranslationBit je self s f
                    return ([cvar],J.ExpName (J.Name [f]), Typ t (\_ -> t1) )
              otherwise -> 
                do  put (n+2)
                    let self = J.Ident ("x" ++ show (n+1)) -- use another fresh variable              
                    (s,je,t1) <- translateScopeM this (g (Left (n+1),t)) m
                    let cvar = refactoredScopeTranslationBit je self s f
                    return ([cvar],J.ExpName (J.Name [f]), Typ t (\_ -> t1) )
  
  }

last (Typ _ _) = False
last (Kind f)  = last (f 0)
last (Body _)  = True

-- seperating (hopefully) the important bit

currentInitialDeclaration idCurrentName = J.MemberDecl $ J.FieldDecl [] closureType [J.VarDecl (J.VarId idCurrentName) (Just (J.InitExp J.This))]
outputAssignment javaExpression = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA  javaExpression))
standardTranslation javaExpression statementsBeforeOA idCurrentName idNextName = J.LocalVars [] closureType [J.VarDecl (J.VarId idNextName) 
                                                (Just (J.InitExp 
                                                    (jexp [currentInitialDeclaration idCurrentName] (Just (J.Block (statementsBeforeOA ++ [outputAssignment javaExpression]))))
                                                    )
                                                )]

refactoredScopeTranslationBit :: J.Exp -> J.Ident -> [J.BlockStmt] -> J.Ident -> J.BlockStmt
refactoredScopeTranslationBit javaExpression idCurrentName statementsBeforeOA idNextName = completeClosure
    where
        outputAssignment = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA  javaExpression))
        fullAssignment = J.InitDecl False (J.Block [(J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA
                                                    (pullupClosure statementsBeforeOA))))])
        currentInitialDeclaration = J.MemberDecl $ J.FieldDecl [] closureType [J.VarDecl (J.VarId idCurrentName) (Just (J.InitExp J.This))]
        completeClosure | checkExp javaExpression  = standardTranslation javaExpression statementsBeforeOA idCurrentName idNextName
                        | otherwise = J.LocalVars [] closureType [J.VarDecl (J.VarId idNextName) 
                                                (Just (J.InitExp 
                                                    (jexpOutside [currentInitialDeclaration,fullAssignment]
                                                    )
                                                ))]

checkExp :: J.Exp -> Bool
checkExp (J.ExpName (J.Name [J.Ident f])) = ('.' `elem` f || ((filter (not . isDigit) f) `elem` localVarPrefs))
checkExp x = True

jexpOutside init = J.InstanceCreation [] (J.ClassType [(J.Ident "Closure",[])]) [] 
       (Just (J.ClassBody (init ++  [
          J.MemberDecl (J.MethodDecl [] [] Nothing (J.Ident "apply") [] [] (J.MethodBody (Just(J.Block $ []))))
       ])))

pullupClosure [J.LocalVars [] rf vd] = case vd of
                                [J.VarDecl variableId (Just(J.InitExp exp))] -> exp
                                _ -> error ("B:" ++ show vd)
pullupClosure m = error ("A: " ++ concatMap prettyPrint m)   
