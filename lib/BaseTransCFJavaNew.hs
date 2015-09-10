{-# LANGUAGE FlexibleContexts, TypeOperators, MultiParamTypeClasses, ScopedTypeVariables #-}
{- |
Module      :  BaseTransCFJavaNew
Description :  Basic translation of dependently typed FCore to Java
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Jeremy <bixuanxbi@gmail.com>, Tomas <tomtau@connect.hku.hk>
Stability   :  stable
Portability :  non-portable (MPTC)

This module implements the basic translation of FCore to Java. For
more information, please refer to the paper on wiki.
-}


module BaseTransCFJavaNew where

import           Data.Char (toLower)
import           Data.List (zip4, elemIndex)
import qualified Language.Java.Syntax as J
import           Lens.Micro


import           ClosureFNew
import           Inheritance
import           JavaEDSL
import           MonadLib hiding (Alt)
import           Panic
import qualified Src as S
import           StringPrefixes

instance (:<) (Translate m) (Translate m) where
   up = id

type InitVars = [J.BlockStmt]

-- Closure F to Java

createCUB :: Maybe J.PackageDecl -> [J.TypeDecl] -> J.CompilationUnit
createCUB package compDef = cu
  where
    cu = J.CompilationUnit package [] compDef


initClass :: String -> String -> J.Exp -> J.BlockStmt
initClass className tempName expr =
    let ty = classTy className
    in localFinalVar
           ty
           (varDecl
                tempName
                (if ty == objClassTy
                     then expr
                     else cast ty expr))

type Var = Int -- Either normal variable or class name

type TransJavaExp = Either J.Name J.Literal -- either variable or special case: Lit

type TransType = ([J.BlockStmt], TransJavaExp, Type Int)

data Translate m = T
    { translateM :: Expr (Var, Type Int) -> m TransType
    , translateScopeM :: EScope (Var, Type Int) -> Maybe (Int, Type Int) -> m ([J.BlockStmt], TransJavaExp, EScope Int)
    , translateApply :: Bool -> m TransType -> m TransType -> m TransType
    , translateIf :: m TransType -> m TransType -> m TransType -> m TransType
    , translateLet :: TransType -> ((Var, Type Int) -> Expr (Var, Type Int)) -> m TransType
    , translateScopeTyp :: Int -> Int -> [J.BlockStmt] -> EScope (Var, Type Int) -> m ([J.BlockStmt], TransJavaExp, EScope Int) -> String -> m ([J.BlockStmt], EScope Int)
    , genApply :: J.Exp -> EScope Int -> String -> J.Type -> J.Type -> m [J.BlockStmt]
    , genRes :: EScope Int -> [J.BlockStmt] -> m [J.BlockStmt]
    , applyRetType :: Type Int -> m (Maybe J.Type)
    , genClone :: m Bool
    , withApply :: m Bool
    , getPrefix :: m String
    , javaType :: Type Int -> m J.Type
    , chooseCastBox :: Type Int -> m (String -> J.Exp -> J.BlockStmt, J.Type)
    , stackMainBody :: Type Int -> m [J.BlockStmt]
    , genClosureVar :: Bool -> Int -> TransJavaExp -> m J.Exp
    , createWrap :: String -> Expr (Var, Type Int) -> m (J.CompilationUnit, Type Int)
    }
