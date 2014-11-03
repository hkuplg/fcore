{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XConstraintKinds -XScopedTypeVariables #-}

module ApplyTransCFJava where

import qualified Data.Set as Set
import qualified Language.Java.Syntax as J
import           Prelude hiding (init, last)

import           BaseTransCFJava
import           ClosureF
import           Inheritance
import           JavaEDSL
import           MonadLib
import           StringPrefixes

data ApplyOptTranslate m = NT {toT :: Translate m}

instance (:<) (ApplyOptTranslate m) (Translate m) where
   up              = up . toT

instance (:<) (ApplyOptTranslate m) (ApplyOptTranslate m) where
   up              = id

last :: Num t1 => Scope t t1 t2 -> Bool
last (Type _ _) = False
last (Kind f)   = last (f 0)
last (Body _)   = True

-- main translation function
transApply :: (MonadState Int m,
               MonadState (Set.Set J.Exp) m,
               MonadReader InitVars m,
               selfType :< ApplyOptTranslate m,
               selfType :< Translate m)
              => Mixin selfType (Translate m) (ApplyOptTranslate m)
transApply _ super = NT {toT = super {
  translateScopeTyp = \currentId nextId initVars nextInClosure m closureClass ->
    case last nextInClosure of
         True -> do   (initVars' :: InitVars) <- ask
                      translateScopeTyp super currentId nextId (initVars ++ initVars') nextInClosure (local (\(_ :: InitVars) -> []) m) closureClass
         False -> do  (s,je,t1) <- local (initVars ++) m
                      let refactored = modifiedScopeTyp je s currentId nextId closureClass
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

modifiedScopeTyp :: J.Exp -> [J.BlockStmt] -> Int -> Int -> String -> [J.BlockStmt]
modifiedScopeTyp oexpr ostmts currentId nextId closureClass = completeClosure
  where closureType' = classTy closureClass
        currentInitialDeclaration = memberDecl $ fieldDecl closureType' (varDecl (localvarstr ++ show currentId) J.This)
        completeClosure = [(localClassDecl ("Fun" ++ show nextId) closureClass
                            (closureBodyGen
                             [currentInitialDeclaration, J.InitDecl False (block $ (ostmts ++ [assign (name ["out"]) oexpr]))]
                             []
                             nextId
                             True
                             closureType'))
                          ,localVar closureType' (varDecl (localvarstr ++ show nextId) (funInstCreate nextId))]

