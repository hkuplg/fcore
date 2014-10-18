{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XConstraintKinds -XScopedTypeVariables #-}

module ApplyTransCFJava where

import Prelude hiding (init, last)

import qualified Data.Set as Set

import qualified Language.Java.Syntax as J
import ClosureF
import Inheritance
import BaseTransCFJava
import StringPrefixes
import MonadLib

import JavaEDSL

data ApplyOptTranslate m = NT {toT :: Translate m}

instance (:<) (ApplyOptTranslate m) (Translate m) where
   up              = up . toT

instance (:<) (ApplyOptTranslate m) (ApplyOptTranslate m) where
   up              = id

last (Type _ _) = False
last (Kind f)   = last (f 0)
last (Body _)   = True

-- main translation function
transApply :: (MonadState Int m, MonadState (Set.Set J.Exp) m, MonadReader InitVars m, selfType :< ApplyOptTranslate m, selfType :< Translate m) => Mixin selfType (Translate m) (ApplyOptTranslate m)
transApply this super = NT {toT = super {
  translateScopeTyp = \currentId nextId initVars nextInClosure m ->
    case last nextInClosure of
         True -> do   (initVars' :: InitVars) <- ask
                      translateScopeTyp super currentId nextId (initVars ++ initVars') nextInClosure (local (\(_ :: InitVars) -> []) m)
         False -> do  (s,je,t1) <- local (initVars ++) m
                      refactored <- refactoredScopeTranslationBit (up this) je s currentId nextId
                      return (refactored,t1),

  genApply = \f t x y z -> if (last t) then genApply super f t x y z else return [],

  setClosureVars = \t f j1 j2 -> do
              (usedCl :: Set.Set J.Exp) <- get
              maybeCloned <-  case t of
                                Body _ ->
                                   return j1
                                _ ->
                                   if (Set.member j1 usedCl) then
                                      return $ J.MethodInv (J.PrimaryMethodCall (j1) [] (J.Ident "clone") [])
                                   else do
                                           put (Set.insert j1 usedCl)
                                           return j1
              setClosureVars super t f maybeCloned j2,

  genClone = return True
}}

refactoredScopeTranslationBit :: (Show a, Monad m) => Translate m -> J.Exp -> [J.BlockStmt] -> a -> Int -> m [J.BlockStmt]
refactoredScopeTranslationBit this javaExpression statementsBeforeOA currentId nextId =
  do closureClass <- liftM2 (++) (getPrefix this) (return "Closure")
     let closureType' = classTy closureClass
     let currentInitialDeclaration =
          J.MemberDecl $
          J.FieldDecl
            []
            closureType'
            [J.VarDecl (J.VarId $ J.Ident $ localvarstr ++ show currentId)
                       (Just (J.InitExp J.This))]
     return [(J.LocalClass
              (J.ClassDecl
                 []
                 (J.Ident ("Fun" ++ show nextId))
                 []
                 (Just $
                  J.ClassRefType (J.ClassType [(J.Ident closureClass,[])]))
                 []
                 (closureBodyGen
                    [currentInitialDeclaration
                    ,J.InitDecl
                       False
                       (J.Block $
                        (statementsBeforeOA ++
                         [assign (name ["out"]) javaExpression]))]
                    []
                    nextId
                    True
                    closureType')))
          ,J.LocalVars
             []
             (closureType')
             ([J.VarDecl (J.VarId $
                          J.Ident (localvarstr ++ show nextId))
                         (Just (J.InitExp (funInstCreate nextId)))])]

