{-# LANGUAGE FlexibleContexts, TypeOperators, MultiParamTypeClasses, ScopedTypeVariables #-}
{- |
Module      :  BaseTransCFJava
Description :  Basic translation of FCore to Java
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Jeremy <bixuanxbi@gmail.com>, Tomas <tomtau@connect.hku.hk>
Stability   :  stable
Portability :  non-portable (MPTC)

This module implements the basic translation of FCore to Java. For
more information, please refer to the paper on wiki.
-}


module BaseTransCFJava(Translate,InitVars,createWrap,trans,Var) where

import qualified Language.Java.Syntax as J
import           ClosureF
import           Inheritance
import           MonadLib
import           JavaEDSL(wrapperClass,bStmt,mainBody,var,classTy,
                          objClassTy,localFinalVar,varDecl,
                          -- needed by lam/fixpoint?
                          name, fieldAccess, left, cast, localVar, funInstCreate,
                          localClassDecl, classBody, methodDecl, memberDecl, fieldDecl,
                          assign,
                          -- needed by app
                          assignField, applyMethodCall,fieldAccExp)
import qualified Src as S
import           StringPrefixes(localvarstr, closureOutputO, closureOutputL)

-- messy lam/fixpoint
import           Debug.Trace
import           Data.Char
import           BaseTransCFJava.Helper

trans :: (MonadState Int m, selfType :< Translate m) => Base selfType (Translate m)
trans self = let this = up self
  in T {
  translateM = \e -> case e of
      Var lname (i,t) -> return ([],DualVar i,t)

      Lit lit ->
          case lit of
            (S.Int i)    -> return ([], PrimLit $ J.Int i, CFLong)
            (S.UnitLit)  -> return ([], PrimLit J.Null, Unit)
            (S.String s) -> return ([], PrimLit $ J.String s,  JClass "java.lang.String")
            (S.Bool b)   -> return ([], PrimLit $ J.Boolean b, CFBool)
            (S.Char c)   -> return ([], PrimLit $ J.Char c, CFChar)
            _ -> error "Ooops, this literal is not implemented."

      PrimOp e1 op e2 ->
            do (s1,j1,_) <- translateM this e1
               (s2,j2,_) <- translateM this e2
               let j1' = unwrap (Left j1)
               let j2' = unwrap (Left j2)
               let (jexpr,typ) = case op of
                                   (S.Arith realOp) -> (J.BinOp j1' realOp j2', CFLong)
                                   (S.Compare realOp) -> (J.BinOp j1' realOp j2', CFBool)
                                   (S.Logic realOp) -> (J.BinOp j1' realOp j2', CFBool)
                                   _ -> error "Ooops, this operator is not implemented."
               newVarName <- getNewVarName this
               let assignExpr = assignVar typ newVarName jexpr
               return (s1 ++ s2 ++ [assignExpr], SingleVar $ name [newVarName] ,typ)

      TApp expr t ->
       do (n :: Int) <- get
          put (n + 1) -- needed to distinguish different type variables
          (s,je,Forall (Kind lname f)) <- translateM this expr
          let fn = lname ++ show n
          -- ??? hack
          return (s,je,scope2ctyp (substScope' (name [fn], name [fn]) t (f (name [fn], name [fn]))))

      App e1 e2 ->
        do (s1,SingleVar j1,Forall (Type fname _ g)) <- translateM this e1
           (s2,j2,t2) <- translateM this e2
           let retTyp = g ()
           (n :: Int) <- get
           put (n+2)
           let f = fname ++ show n
           let (primitiveAccessField, objectAccessField) = dualArgFields f

           -- TODO: the final parameter (name [f], name [f]) to Nothing?
           let (fargAssign, _) = multiAssignment ToField t2 (Left j2)
                                (assignField primitiveAccessField,
                                assignField objectAccessField) (name [f], name [f])

           let fd = localVar (classTy "f2j.unbox.Closure") (varDecl f (J.ExpName j1))
           let primitiveResultField = fieldAccess (J.ExpName (name [f])) closureOutputL
           let objectResultField = fieldAccess (J.ExpName (name [f])) closureOutputO
           let apply = [bStmt $ applyMethodCall (J.ExpName $ name [f])]

           let xfl = "l" ++ localvarstr ++ show (n+1)
           let xfo = "o" ++ localvarstr ++ show (n+1)
           let rt = scope2ctyp retTyp
           let typRT = javaType rt
           let flag = typRT == objClassTy
           let primRT = case rt of (TVar _) -> J.PrimType J.LongT
                                   _ -> typRT
           let objFun field = localFinalVar typRT (varDecl xfo (if flag then field else cast typRT field))
           let (tempVars, finalExpr) = multiAssignment FromField rt (Right (primitiveResultField, objectResultField))
                                (localFinalVar primRT . varDecl xfl,
                                objFun) (name [xfl], name [xfo])

           let s3 = fd : fargAssign ++ apply ++ tempVars
           return (s1 ++ s2 ++ s3, finalExpr, rt)

      Lam se ->
            do (s,je,t) <- translateScopeM this se
               return (s,je,Forall t)

      Let lname expr b ->
        do (s1, j1, t1) <- translateM this expr
           (n :: Int) <- get
           put (n + 1)
           let x = lname ++ show n
           -- hack?
           (s2, j2, t2) <- translateM this (b ((name [x], name [x]), t1))
           let javaT1 = javaType t1
           let xDecl = localVar javaT1 (varDecl x $ unwrap (Left j1))
           return (s1 ++ [xDecl] ++ s2, j2, t2)
                -- TODO: rest; just default to be able to run tests
      _ -> return ([], PrimLit J.Null, Unit)
               ,

  translateScopeM =
    \e ->
      case e of

        Body t ->
          do (s,je,t1) <- translateM this t
             return (s,je,Body t1)

        Kind lname f ->
          do (n :: Int) <- get
             put (n + 1) -- needed to distingush type variable
             let xl = name ["l" ++ lname ++ show n]
             let xo = name ["o" ++ lname ++ show n]
             (s,je,t1) <- translateScopeM this (f (xl, xo))
             return (s,je,Kind lname (\a -> substScope' (xl, xo) (TVar a) t1))

        Type lname t g ->
          do (n :: Int) <- get
             let x1 = lname ++ show n
             let x2l = "l" ++ lname ++ show (n + 1)
             let x2o = "o" ++ lname ++ show (n + 1)
             put (n + 2)
             let (primitiveField, objectField) = dualArgFields x1
             let (primitiveAccessField, objectAccessField)  = (J.FieldAccess primitiveField, J.FieldAccess objectField)
             let typT1 = javaType t
             let flag = typT1 == objClassTy
             let primRT = case t of (TVar _) -> J.PrimType J.LongT
                                    _ -> typT1
             let objFun field = localFinalVar typT1 (varDecl x2o (if flag then field else cast typT1 field))

             let (tempVars, _) = multiAssignment FromField t (Right (primitiveAccessField, objectAccessField))
                                  (localFinalVar primRT . varDecl x2l,
                                  objFun) (name [x2l], name [x2o])

             let nextInClosure = g ((name [x2l], name [x2o]),t)


             let closureClass = "f2j.unbox.Closure"
             (ostmts,oexpr,t1) <- translateScopeM this nextInClosure
             let t1' = case t1 of
                           Kind _ _ -> error "Found Kind, where Type or Body expected."
                           Type {} -> Forall t1
                           Body b -> b

             let (outAssignment, _) = multiAssignment ToField t1' (Left oexpr)
                                  (assign (name [closureOutputL]),
                                  assign (name [closureOutputO])) (name [x1], name [x1])

             let cvar = [localClassDecl ("Fun" ++ show n)
                                     closureClass
                                     (closureBodyGen [memberDecl $ fieldDecl (classTy closureClass)
                                                                             (varDecl x1 J.This)]
                                                     (tempVars ++ ostmts ++ outAssignment)
                                                     (classTy closureClass))]

             let fstmt = [localVar (classTy "f2j.unbox.Closure") (varDecl x1 (funInstCreate n))]

             return (cvar ++ fstmt, SingleVar $ name [x1] ,Type lname t (const t1))
    ,

  createWrap = \className fExp ->
    do
      (bs,e,t) <- translateM this fExp
      let returnStmt = case t of
             CFLong -> [bStmt $ J.Return $ Just (unwrap (Left e))]
             CFBool -> error "Boolean conversion not implemented."
             CFChar -> error "Character conversion not implemented." -- Character.toChars(int value)
             CFDouble -> error "Floating point conversion not implemented."
             _ -> [bStmt $ J.Return $ Just (case e of SingleVar x -> J.ExpName x
                                                      DualVar (_,x2) -> J.ExpName x2 -- or x1?
                                                      PrimLit z -> J.Lit z)]


      let mainDecl = wrapperClass className (bs ++ returnStmt) (Just (javaType t)) mainBody
      return (createCUB self [mainDecl],Unit)
}
