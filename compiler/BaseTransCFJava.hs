{-# OPTIONS -XFlexibleContexts #-}

module BaseTransCFJava where
-- translation that does not pre-initialize Closures that are ininitalised in apply() methods of other Closures 
import Prelude hiding (init, last)
import Debug.Trace
import Data.List hiding (init, last)

import Control.Monad.State
import Control.Monad.Writer

import qualified Language.Java.Syntax as J
import Language.Java.Pretty
import ClosureF
import Mixins
import StringPrefixes

-- Closure F to Java

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
        ifvarname = (ifresultstr ++ show n)
        refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
        newvar = var ifvarname

createCU :: (J.Block, J.Exp, t) -> (J.CompilationUnit, t)
createCU (J.Block bs,e,t) = (cu,t) where
   cu = J.CompilationUnit Nothing [] [closureClass, classDecl]
   field name = J.MemberDecl (J.FieldDecl [] (J.RefType (refType "Object")) [
              J.VarDecl (J.VarId (J.Ident name)) Nothing])
   app mod b = J.MemberDecl (J.MethodDecl mod [] Nothing (J.Ident "apply") [] [] (J.MethodBody b))
   closureClass = J.ClassTypeDecl (J.ClassDecl [J.Abstract] (J.Ident "Closure") [] Nothing [] (
                  J.ClassBody [field localvarstr,field "out",app [J.Abstract] Nothing]))
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

genOp :: J.Exp -> J.Op -> J.Exp -> J.Exp
genOp j1 op j2 = J.BinOp maybeCasted1 op maybeCasted2
    where
        maybeCasted1 = case j1 of J.Lit e -> j1
                                  _ -> J.Cast boxedIntType j1
        maybeCasted2 = case j2 of J.Lit e -> j2
                                  _ -> J.Cast boxedIntType j2

type Var = Either Int Int -- left -> standard variable; right -> recursive variable

last (Typ _ _) = False
last (Kind f)  = last (f 0)
last (Body _)  = True

currentInitialDeclaration idCurrentName = J.MemberDecl $ J.FieldDecl [] closureType [J.VarDecl (J.VarId idCurrentName) (Just (J.InitExp J.This))]
outputAssignment javaExpression = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA  javaExpression))
standardTranslation javaExpression statementsBeforeOA idCurrentName idNextName = J.LocalVars [] closureType [J.VarDecl (J.VarId idNextName) 
                                                (Just (J.InitExp 
                                                    (jexp [currentInitialDeclaration idCurrentName] (Just (J.Block (statementsBeforeOA ++ [outputAssignment javaExpression]))))
                                                    )
                                                )]
  
data Translate m = T {
  translateM :: 
     PCExp Int (Var, PCTyp Int) -> 
     m ([J.BlockStmt], J.Exp, PCTyp Int),
  translateScopeM :: 
    Scope (PCExp Int (Var, PCTyp Int)) Int (Var, PCTyp Int) -> 
    Maybe (Int,PCTyp Int) ->
    m ([J.BlockStmt], J.Exp, TScope Int)
  }

instance Monoid Bool where  
    mempty = False  
    mappend a b = a  

trans :: (MonadState Int m) => Open (Translate m)
trans this = T {
  translateM = \e -> case e of 
     CVar (Left i,t) -> 
        do return ([],var (localvarstr ++ show i ++ "." ++ localvarstr), t)
     
     CVar (Right i, t) ->
       do return ([],var (localvarstr ++ show i), t)
     
     CFLit e    ->
       return ([],J.Lit $ J.Int e, CInt)
     
     CFPrimOp e1 op e2 ->
       do  (s1,j1,t1) <- translateM this e1
           (s2,j2,t2) <- translateM this e2
           return (s1 ++ s2, genOp j1 op j2, t1)
           
     CFif0 e1 e2 e3 ->
       do  n <- get
           put (n+1)
           (s1,j1,t1) <- translateM this (CFPrimOp e1 J.Equal (CFLit 0))
           (s2,j2,t2) <- translateM this e2
           (s3,j3,t3) <- translateM this e3
           let ifvarname = (ifresultstr ++ show n)
           let refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
           let ifresdecl = J.LocalVars [] (J.RefType (refType "Object")) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Nothing)])
           let  (ifstmt, ifexp) = ifBody (s2, s3) (j1, j2, j3) n  -- uses a fresh variable        
           return (s1 ++ [ifresdecl,ifstmt], ifexp, t2)                     -- need to check t2 == t3
           
     CFTuple tuple ->
       liftM reduceTTuples $ mapM (translateM this) tuple
       
     CFProj i (CFTuple tuple) ->
       translateM this (tuple!!i)
     
     CTApp e t -> 
       do  n <- get
           (s,je, CForall (Kind f)) <- translateM this e
           return (s,je, scope2ctyp (substScope n t (f n)))
     
     CLam s ->
       do  (s,je, t) <- translateScopeM this s Nothing
           return (s,je, CForall t)
     
     CFix t s   -> 
       do  n <- get
           put (n+1)
           (s, je, t') <- translateScopeM this (s (Right n,t)) (Just (n,t)) -- weird!
           return (s,je, CForall t')
           
     CApp e1 e2 ->
       do  n <- get
           put (n+1)
           (s1,j1, CForall (Typ t1 g)) <- translateM this e1
           -- DEBUG
           -- (s1,j1, debug) <- translateM this e1
           -- (CForall (Typ t1 g)) <- trace ("C:" ++ show debug) (return debug)
           -- END DEBUG
           (s2,j2,t2) <- translateM this e2
           let t    = g ()
           let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
           let cvar = J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp (J.Cast closureType j1)))])
           let ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident localvarstr))) J.EqualA j2) ) 
           let apply = J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall (J.ExpName (J.Name [f])) [] (J.Ident "apply") [])))
           let s3 = case t of -- checking the type whether to generate the apply() call
                       Body _ -> [cvar,ass,apply]
                       _ -> [cvar,ass]
           let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
           return (s1 ++ s2 ++ s3, j3, scope2ctyp t), -- need to check t1 == t2
           
  translateScopeM = \e m -> case e of 

      Body t ->
        do  (s,je, t1) <- translateM this t
            return (s,je, Body t1)
          
      Kind f -> 
        do  n <- get
            put (n+1) -- needed?
            (s,je,t1) <- translateScopeM this (f n) m
            return (s,je, Kind (\a -> substScope n (CTVar a) t1)) 
                
      Typ t g -> 
        do  n <- get
            let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable              
            case m of -- Consider refactoring later?
              Just (i,t') | last (g (Right i,t')) ->
                do  put (n+1)
                    let self = J.Ident (localvarstr ++ show i)
                    (s,je,t1) <- translateScopeM (this) (g (Left i,t)) m
                    let cvar = standardTranslation je s self f
                    return ([cvar],J.ExpName (J.Name [f]), Typ t (\_ -> t1) )
              otherwise -> 
                do  put (n+2)
                    let self = J.Ident (localvarstr ++ show (n+1)) -- use another fresh variable              
                    (s,je,t1) <- translateScopeM (this) (g (Left (n+1),t)) m
                    let cvar = standardTranslation je s self f
                    return ([cvar],J.ExpName (J.Name [f]), Typ t (\_ -> t1) )

    }