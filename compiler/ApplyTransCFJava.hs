{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts #-}

module ApplyTransCFJava where

import Prelude hiding (init, last)
import Debug.Trace
import Data.List hiding (init, last)

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity

import qualified Language.Java.Syntax as J
import Language.Java.Pretty
import ClosureF
import Mixins
import Data.Char
import BaseTransCFJava

data ApplyOptTranslate m = NT {
  toT :: Translate m
  }

-- main translation function
transApply :: (MonadState Int m, MonadWriter Bool m) => Open (ApplyOptTranslate m)
transApply this = NT { toT = T {
  translateM = \e -> case e of 
       CLam s ->
           do  tell False
               translateM (toT this) e

       otherwise -> 
            do  tell True
                translateM (toT this) e,
  
  translateScopeM = \e m -> case e of 
      Typ t g -> 
        do  n <- get
            let f    = J.Ident ("x" ++ show n) -- use a fresh variable
            case m of -- Consider refactoring later?
              Just (i,t') | last (g (Right i,t')) ->
                do  put (n+1)
                    let self = J.Ident ("x" ++ show i)
                    tell False
                    ((s,je,t1), closureCheck) <- listen $ translateScopeM (toT this) (g (Left i,t)) m
                    let cvar = refactoredScopeTranslationBit je self s f closureCheck
                    return ([cvar],J.ExpName (J.Name [f]), Typ t (\_ -> t1) )
              otherwise -> 
                do  put (n+2)
                    let self = J.Ident ("x" ++ show (n+1)) -- use another fresh variable
                    tell False
                    ((s,je,t1), closureCheck) <- listen $ translateScopeM (toT this) (g (Left (n+1),t)) m
                    let cvar = refactoredScopeTranslationBit je self s f closureCheck
                    return ([cvar],J.ExpName (J.Name [f]), Typ t (\_ -> t1) )

      otherwise ->
          do tell False
             translateScopeM (toT this) e m
  }
  }

-- seperating (hopefully) the important bit

refactoredScopeTranslationBit :: J.Exp -> J.Ident -> [J.BlockStmt] -> J.Ident -> Bool -> J.BlockStmt
refactoredScopeTranslationBit javaExpression idCurrentName statementsBeforeOA idNextName closureCheck = completeClosure
    where
        outputAssignment = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA  javaExpression))
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
