{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, TypeOperators, MultiParamTypeClasses, ConstraintKinds, ScopedTypeVariables #-}
{- |
Module      :  ApplyTransCFJava
Description :  Translation of FCore to Java with Apply-opt
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Jeremy <bixuanxbi@gmail.com>, Tomas <tomtau@connect.hku.hk>
Stability   :  experimental
Portability :  non-portable (MPTC)

This module augments the basic translation of FCore to Java with
multi-argument function optimization.
-}

module ApplyTransCFJava where
-- TODO: isolate all hardcoded strings to StringPrefixes (e.g. Fun)

import qualified Language.Java.Syntax as J

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

isMultiBinder :: EScope Int (Var, Type Int) -> Bool
isMultiBinder (Type _ _) = False
isMultiBinder (Kind _)   = True
isMultiBinder (Body _)   = True

-- main translation function
transApply :: (MonadState Int m,
               MonadReader Int m,
               MonadReader InitVars m,
               selfType :< ApplyOptTranslate m,
               selfType :< Translate m)
              => Mixin selfType (Translate m) (ApplyOptTranslate m)
transApply this super = NT {toT = super {

  translateM =
     \e -> case e of
              App e1 e2 -> do (n :: Int) <- ask
                              let flag = case e1 of Var _ _ -> True
                                                    _ -> False
                              translateApply (up this)
                                             flag
                                             (local (\(_ :: Int) -> n + 1) $ translateM (up this) e1)
                                             (local (\(_ :: Int) -> 0) $ translateM (up this) e2)
              _ -> translateM super e,

  genClosureVar =
    \flag arity j1 ->
     do (n :: Int) <- ask
        if flag && arity > n
          then return $ J.MethodInv (J.PrimaryMethodCall (unwrap j1) [] (J.Ident "clone") [])
          else return (unwrap j1),

  translateScopeTyp = \x1 f initVars nextInClosure m closureClass ->
    if isMultiBinder nextInClosure
    then do (initVars' :: InitVars) <- ask
            translateScopeTyp super x1 f (initVars ++ initVars') nextInClosure (local (\(_ :: InitVars) -> []) m) closureClass
    else do (s,je,t1) <- local (initVars ++) m
            let refactored = modifiedScopeTyp (unwrap je) s x1 f closureClass
            return (refactored,t1),

  genApply = \f t x y z ->
              do applyGen <- genApply super f t x y z
                 return [bStmt $ J.IfThen (fieldAccess f "hasApply") (J.StmtBlock (block applyGen))],

  genClone = return True
}}

modifiedScopeTyp :: J.Exp -> [J.BlockStmt] -> Int -> Int -> String -> [J.BlockStmt]
modifiedScopeTyp oexpr ostmts x1 f closureClass = completeClosure
  where closureType' = classTy closureClass
        currentInitialDeclaration = memberDecl $ fieldDecl closureType' (varDecl (localvarstr ++ show x1) J.This)
        setApplyFlag = assignField (fieldAccExp (left $ var (localvarstr ++ show x1)) "hasApply") (J.Lit (J.Boolean False))
        fc = f
        completeClosure = [localClassDecl ("Fun" ++ show fc) closureClass
                            (closureBodyGen
                             [currentInitialDeclaration, J.InitDecl False (block (setApplyFlag : ostmts ++ [assign (name [closureOutput]) oexpr]))]
                             []
                             fc
                             True
                             closureType')]


-- Alternate version of transApply that works with Stack translation
transAS :: (MonadState Int m,
            MonadReader Int m,
            MonadReader Bool m,
            MonadReader InitVars m,
            selfType :< ApplyOptTranslate m,
            selfType :< Translate m)
           => Mixin selfType (Translate m) (ApplyOptTranslate m)
transAS this super = NT {toT = (up (transApply this super)) {

  translateM =
   \e -> case e of
            App e1 e2 -> do (n :: Int) <- ask
                            let flag = case e1 of Var _ _ -> True
                                                  _ -> False
                            translateApply (up this)
                                           flag
                                           (local (False &&) $ (local (\(_ :: Int) -> n + 1) $ translateM (up this) e1))
                                           (local (False &&) $ (local (\(_ :: Int) -> 0) $ translateM (up this) e2))
            _ -> translateM super e,

  genApply = \f t tempOut outType z ->
    do applyGen <- genApply super f t tempOut outType z
       let tempDecl = localVar outType
                      (varDecl tempOut (case outType of
                                         J.PrimType J.IntT -> J.Lit (J.Int 0)
                                         _ -> J.Lit J.Null))
       let elseDecl = assign (name [tempOut]) (cast outType (J.FieldAccess (fieldAccExp (cast z f) closureOutput)))

       if length applyGen == 2
         then return applyGen
         else return [tempDecl, bStmt $ J.IfThenElse (fieldAccess f "hasApply")
                                (J.StmtBlock (block applyGen))
                                (J.StmtBlock (block [elseDecl]))]
  }}
