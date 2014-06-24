{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XConstraintKinds -XScopedTypeVariables #-}

module ApplyTransCFJava where

import Prelude hiding (init, last)
import Debug.Trace
import Data.List hiding (init, last)

import qualified Language.Java.Syntax as J
import Language.Java.Pretty
import ClosureF
-- import Mixins
import Inheritance
import Data.Char
import Data.Map as Map
import qualified Data.Set as Set
import BaseTransCFJava
import StringPrefixes
import MonadLib

data ApplyOptTranslate m = NT {toT :: Translate m}

instance (:<) (ApplyOptTranslate m) (Translate m) where
   up              = up . toT 

instance (:<) (ApplyOptTranslate m) (ApplyOptTranslate m) where --reflexivity
   up              = id
   
--   override (NT fm) f  = NT (override fm f) -- needed to do proper overriding of methods, when we only know we inherit from a subtype. If 

-- main translation function
transApply :: (MonadState Int m, MonadState (Map.Map J.Exp Int) m, MonadWriter Bool m, selfType :< ApplyOptTranslate m, selfType :< Translate m) => Mixin selfType (Translate m) (ApplyOptTranslate m) -- generalize to super :< Translate m?
transApply this super = NT {toT = T { --override this (\trans -> trans {
  translateM = \e -> case e of 
       CLam s ->
           do  tell False
               translateM super e
--TODO: merge common parts with BaseTransCF
       CApp e1 e2 ->
           do  tell True
               (n :: Int) <- get
               put (n+1)
               (s1,j1, CForall (Typ t1 g)) <- translateM (up this) e1
               (s2,j2,t2) <- translateM (up this) e2
               (env :: Map.Map J.Exp Int) <- get
               let t    = g ()
               let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
               let nje1 = case (Map.lookup j1 env) of Nothing -> J.Cast closureType j1
                                                      Just no -> var (tempvarstr ++ show no) 
               let maybeCloned = case t of
                                       Body _ ->
                                           nje1
                                       _ ->
                                           J.MethodInv (J.PrimaryMethodCall (nje1) [] (J.Ident "clone") [])
                                           
               let cvar = J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp (maybeCloned)))])                                                      
               let nje2 = case (Map.lookup j2 env) of Nothing -> j2
                                                      Just no -> var (tempvarstr ++ show no)                       
               let ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident localvarstr))) J.EqualA nje2) ) 
               let apply = J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall (J.ExpName (J.Name [f])) [] (J.Ident "apply") [])))
               let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
               s3 <- case t of -- checking the type whether to generate the apply() call
                               Body _ ->
                                case (scope2ctyp t) of CInt ->
                                                        do (result, _) <- genSubst j3 initIntCast
                                                           let r = [cvar,ass,apply] ++ result
                                                           return r
                                                       CForall (_) ->
                                                        do (result, _) <- genSubst j3 initClosure
                                                           let r = [cvar,ass,apply] ++ result
                                                           return r
                                                       _ ->  
                                                        do (result, _) <- genSubst j3 initObj
                                                           let r = [cvar,ass,apply] ++ result
                                                           return r
                               _ -> do return [cvar,ass]

               return (s1 ++ s2 ++ s3, j3, scope2ctyp t) -- need to check t1 == t2
               
       otherwise -> 
            do  tell True
                translateM super e,
  
  translateScopeM = \e m -> case e of 
      Typ t g -> 
        do  n <- get
            let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
            case m of -- Consider refactoring later?
              Just (i,t') | last (g (Right i,t')) ->
                do  put (n+1)
                    let self = J.Ident (localvarstr ++ show i)
                    tell False
                    ((s,je,t1), closureCheck) <- listen $ translateScopeM (up this) (g (Left i,t)) m
                    (env :: Map.Map J.Exp Int) <- get
                    let nje = case (Map.lookup je env) of Nothing -> je
                                                          Just no -> var (tempvarstr ++ show no)
                    let cvar = refactoredScopeTranslationBit nje s i n closureCheck -- standardTranslation nje s i n (Set.member n envs)
                    return (cvar,J.ExpName (J.Name [f]), Typ t (\_ -> t1) )
              otherwise -> 
                do  put (n+2)
                    let self = J.Ident (localvarstr ++ show (n+1)) -- use another fresh variable
                    tell False
                    ((s,je,t1), closureCheck) <- listen $ translateScopeM (up this) (g (Left (n+1),t)) m
                    (env :: Map.Map J.Exp Int) <- get
                    let nje = case (Map.lookup je env) of Nothing -> je
                                                          Just no -> var (tempvarstr ++ show no)
                    let cvar = refactoredScopeTranslationBit nje s (n+1) n closureCheck
                    return (cvar,J.ExpName (J.Name [f]), Typ t (\_ -> t1) )

      otherwise ->
          do tell True
             translateScopeM super e m
  }}
 
-- seperating (hopefully) the important bit
refactoredScopeTranslationBit javaExpression statementsBeforeOA currentId nextId closureCheck = completeClosure
    where
        -- outputAssignment = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA  javaExpression))
        fullAssignment = {-J.InitDecl False (J.Block [-}(J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA
                                                    (pullupClosure statementsBeforeOA)))){-])-}
        currentInitialDeclaration = J.MemberDecl $ J.FieldDecl [] closureType [J.VarDecl (J.VarId $ J.Ident $ localvarstr ++ show currentId) (Just (J.InitExp J.This))]
        completeClosure | closureCheck  = standardTranslation javaExpression statementsBeforeOA currentId nextId
                        | otherwise = [(J.LocalClass (J.ClassDecl [] (J.Ident ("Fun" ++ show nextId)) [] 
                                        (Just $ J.ClassRefType (J.ClassType [(J.Ident "Closure",[])])) [] (jexp [currentInitialDeclaration, applyCall] (Just(J.Block $ [fullAssignment]))  nextId))),
                                        J.LocalVars [] (closureType) ([J.VarDecl (J.VarId $ J.Ident (localvarstr ++ show nextId)) (Just (J.InitExp (instCreat nextId)))])] 

applyCall = J.InitDecl False $ J.Block [J.BlockStmt $ J.ExpStmt (J.MethodInv (J.MethodCall (J.Name $ [J.Ident "apply"]) []))]                                    
       
pullupClosure [J.LocalVars [] rf vd] = case vd of
                                [J.VarDecl variableId (Just(J.InitExp exp))] -> transf exp
                                _ -> error ("B:" ++ show vd)
                                where
                                    transf (J.InstanceCreation [] (J.ClassType [(J.Ident "Closure",[])]) [] (Just (J.ClassBody b))) = case (head $ tail b) of J.MemberDecl (J.MethodDecl [] [] Nothing (J.Ident "apply") [] [] (J.MethodBody (Just (J.Block (ts))))) -> 
                                                                                                                                                                case (head ts) of J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident idO)])) J.EqualA (rhs))) ->
                                                                                                                                                                                    case idO of "out" ->
                                                                                                                                                                                                    applyDef b
                                                                                                                                                                                                _ -> case rhs of (J.InstanceCreation _ _ _ _) -> applyDef b
                                                                                                                                                                                                                 _ -> defaultDef b
                                                                                                                                                                                  _ -> defaultDef b
                                                                                                                                                              _ -> defaultDef b
                                    transf (exp) = exp
                                    defaultDef b = (J.InstanceCreation [] (J.ClassType [(J.Ident "Closure",[])]) [] (Just (J.ClassBody b)))
                                    applyDef b = J.InstanceCreation [] (J.ClassType [(J.Ident "Closure",[])]) [] (Just (J.ClassBody ([head b, applyCall] ++ (tail b))))
                                    
                                    
                             
pullupClosure m = error ("A: " ++ concatMap prettyPrint m)   
