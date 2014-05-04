{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XConstraintKinds #-}

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
transApply :: (MonadState Int m, MonadWriter Bool m, selfType :< ApplyOptTranslate m) => Mixin selfType (Translate m) (ApplyOptTranslate m) -- generalize to super :< Translate m?
transApply this super = NT {toT = T { --override this (\trans -> trans {
  translateM = \e -> case e of 
       CLam s ->
           do  tell False
               translateM super e

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
                    ((s,je,t1), closureCheck) <- listen $ translateScopeM super (g (Left i,t)) m
                    let cvar = refactoredScopeTranslationBit je self s f closureCheck
                    return ([cvar],J.ExpName (J.Name [f]), Typ t (\_ -> t1) )
              otherwise -> 
                do  put (n+2)
                    let self = J.Ident (localvarstr ++ show (n+1)) -- use another fresh variable
                    tell False
                    ((s,je,t1), closureCheck) <- listen $ translateScopeM super (g (Left (n+1),t)) m
                    let cvar = refactoredScopeTranslationBit je self s f closureCheck
                    return ([cvar],J.ExpName (J.Name [f]), Typ t (\_ -> t1) )

      otherwise ->
          do tell False
             translateScopeM super e m
  }}
 
-- seperating (hopefully) the important bit

refactoredScopeTranslationBit :: J.Exp -> J.Ident -> [J.BlockStmt] -> J.Ident -> Bool -> J.BlockStmt
refactoredScopeTranslationBit javaExpression idCurrentName statementsBeforeOA idNextName closureCheck = completeClosure
    where
        -- outputAssignment = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA  javaExpression))
        fullAssignment = J.InitDecl False (J.Block [(J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA
                                                    (pullupClosure statementsBeforeOA))))])
        currentInitialDeclaration = J.MemberDecl $ J.FieldDecl [] closureType [J.VarDecl (J.VarId idCurrentName) (Just (J.InitExp J.This))]
        completeClosure | closureCheck  = standardTranslation javaExpression statementsBeforeOA idCurrentName idNextName
                        | otherwise = J.LocalVars [] closureType [J.VarDecl (J.VarId idNextName) 
                                                (Just (J.InitExp 
                                                    (jexpOutside [currentInitialDeclaration,fullAssignment]
                                                    )
                                                ))]

jexpOutside init = J.InstanceCreation [] (J.ClassType [(J.Ident "Closure",[])]) [] 
       (Just (J.ClassBody (init ++  [
          J.MemberDecl (J.MethodDecl [] [] Nothing (J.Ident "apply") [] [] (J.MethodBody (Just(J.Block $ []))))
       ])))

pullupClosure [J.LocalVars [] rf vd] = case vd of
                                [J.VarDecl variableId (Just(J.InitExp exp))] -> exp
                                _ -> error ("B:" ++ show vd)
pullupClosure m = error ("A: " ++ concatMap prettyPrint m)   
