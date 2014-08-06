{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XConstraintKinds -XScopedTypeVariables #-}

module ApplyTransCFJava where

import Prelude hiding (init, last)

import qualified Data.Map as Map

import qualified Language.Java.Syntax as J
import ClosureF
-- import Mixins
import Inheritance
import BaseTransCFJava
import StringPrefixes
import MonadLib

data ApplyOptTranslate m = NT {toT :: Translate m}

instance (:<) (ApplyOptTranslate m) (Translate m) where
   up              = up . toT

instance (:<) (ApplyOptTranslate m) (ApplyOptTranslate m) where --reflexivity
   up              = id

-- main translation function
transApply :: (MonadState Int m, MonadWriter Bool m, selfType :< ApplyOptTranslate m, selfType :< Translate m) => Mixin selfType (Translate m) (ApplyOptTranslate m) -- generalize to super :< Translate m?
transApply this super = NT {toT = T { --override this (\trans -> trans {
  translateM = \e -> case e of
       CLam s ->
           do  tell True
               translateM super e

       CApp e1 e2 ->
           do  tell True -- not doing the apply elimination, only pulling up closures to initialization for the moment
               translateM super e

       otherwise ->
            do  tell False
                translateM super e,

  translateScopeM = \e m -> case e of
      Typ t g ->
        do  n <- get
            let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
            let (v,n')  = maybe (n+1,n+2) (\(i,_) -> (i,n+1)) m -- decide whether we have found the fixpoint closure or not
            put (n' + 1)
            let self = J.Ident (localvarstr ++ show v)
            ((s,je,t1), closureCheck) <- listen $ translateScopeM (up this) (g (Left n',t)) Nothing
            let cvar = refactoredScopeTranslationBit je s (v,t) n' n closureCheck
            return (cvar,J.ExpName (J.Name [f]), Typ t (\_ -> t1) )

      otherwise ->
          do tell True
             translateScopeM super e m,
             
     createWrap = \n e -> createWrap super n e
  }}

-- seperating (hopefully) the important bit
refactoredScopeTranslationBit javaExpression statementsBeforeOA (currentId,currentTyp) freshVar nextId closureCheck = completeClosure
    where
        currentInitialDeclaration = J.MemberDecl $ J.FieldDecl [] closureType [J.VarDecl (J.VarId $ J.Ident $ localvarstr ++ show currentId) (Just (J.InitExp J.This))]
        completeClosure | closureCheck  = standardTranslation javaExpression statementsBeforeOA (currentId, currentTyp) freshVar nextId
                        | otherwise =
                           let (f,_) = chooseCastBox currentTyp
                               je = J.FieldAccess $ J.PrimaryFieldAccess (J.ExpName (J.Name [J.Ident $ localvarstr ++ show currentId])) (J.Ident localvarstr)
                           in [(J.LocalClass (J.ClassDecl [] (J.Ident ("Fun" ++ show nextId)) []
                                 (Just $ J.ClassRefType (J.ClassType [(J.Ident "Closure",[])])) [] (jexp [currentInitialDeclaration, J.InitDecl False (J.Block $ ([f localvarstr freshVar je] ++ statementsBeforeOA ++ [outputAssignment javaExpression]))] (Just $ J.Block [])  nextId))),
                                        J.LocalVars [] (closureType) ([J.VarDecl (J.VarId $ J.Ident (localvarstr ++ show nextId)) (Just (J.InitExp (instCreat nextId)))])]

applyCallI = J.InitDecl False $ J.Block [applyCall]
