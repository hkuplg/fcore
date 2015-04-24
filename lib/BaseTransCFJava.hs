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
                          assignField, applyMethodCall,fieldAccExp,
                          -- letrec
                          varDeclNoInit, localClass, instCreat, classTyp)
import qualified Src as S
import           StringPrefixes(localvarstr, tempvarstr, closureOutputO, closureOutputL)

-- messy lam/fixpoint
import           Debug.Trace
import           Data.Char
import           BaseTransCFJava.Helper
import           PrettyUtils

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

{-
translateIf this
            (translateM this e1)
            (translateM this e2)
            (translateM this e3)
genIfBody :: MonadState Int m
          => Translate m
          -> m TransType
          -> m TransType
          -> ([J.BlockStmt],TransJavaExp)
          -> Int
          -> m TransType
genIfBody this m2 m3 (s1,j1) n =
  do (s2,j2,t2) <- m2 {-translateM this e2-}
     (s3,j3,_) <- m3 {-translateM this e3-}
     let ifvarname = ifresultstr ++ show n
     aType <- javaType this t2
     let ifresdecl = localVar aType (varDeclNoInit ifvarname)
     let thenPart  = J.StmtBlock $ block (s2 ++ [assign (name [ifvarname]) (unwrap j2)])
     let elsePart  = J.StmtBlock $ block (s3 ++ [assign (name [ifvarname]) (unwrap j3)])
     let ifstmt    = bStmt $ J.IfThenElse (unwrap j1) thenPart elsePart

     return (s1 ++ [ifresdecl,ifstmt],var ifvarname ,t2) -- need to check t2 == t3

\m1 m2 m3 ->
  do n <- get
     put (n + 1)
     (s1,j1,_) <- m1 {- translateM this e1 -}
     genIfBody this m2 m3 (s1, j1) n
-}

      If e1 e2 e3 -> translateM this e2

      TApp expr t ->
       do (n :: Int) <- get
          put (n + 1) -- needed to distinguish different type variables
          (s,je,Forall (Kind lname f)) <- translateM this expr
          let fn = lname ++ show n
          -- ??? hack
          return (s,je,scope2ctyp (substScope' (name [fn], name [fn]) t (f (name [fn], name [fn]))))

      App e1 e2 ->
        do (s1,j1',Forall (Type fname _ g)) <- translateM this e1
           let j1 = case j1' of SingleVar j1 -> j1
                                DualVar (_,obj) -> obj
           (s2,j2,t2) <- translateM this e2
           let retTyp = g ()
           (n :: Int) <- get
           put (n+2)
           let fname = fname ++ show n
           let f = name [fname]
           let (primitiveAccessField, objectAccessField) = dualArgFields (J.ExpName f)

           -- TODO: the final parameter (name [f], name [f]) to Nothing?
           let (fargAssign, _) = multiAssignment ToField t2 (Left j2)
                                (assignField primitiveAccessField,
                                assignField objectAccessField) (f, f)

           let fd = localVar (classTy "f2j.unbox.Closure") (varDecl fname (J.ExpName j1))
           let primitiveResultField = fieldAccess (J.ExpName f) closureOutputL
           let objectResultField = fieldAccess (J.ExpName f) closureOutputO
           let apply = [bStmt $ applyMethodCall (J.ExpName f)]

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
            do (s,je,t) <- translateScopeM this se Nothing
               return (s,je,Forall t)

{-      Fix lname t s ->
         do (n :: Int) <- get
            put (n + 1)
            let x = lname ++ show n
            (expr,je,t') <- translateScopeM this (s ((name [x], name [x]),t)) (Just ((name [x], name [x]),t)) -- weird!
            return (expr,je,Forall t')

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

      LetRec names t xs body ->
        do (n :: Int) <- get
           let needed = length t
           put (n + 2 + needed)
           mfuns <- return (\defs -> forM (xs defs) (translateM this))
           -- TODO: change Var type?
           let nnames = zipWith (\x y -> (name [x ++ y], name [x ++ y])) names (map show [n ..])
           let vars = liftM (map (\(_,b,c) -> (b,c))) (mfuns (zip nnames t))

           let (bindings :: [Var]) = zipWith (\x y -> (name [x ++ y], name [x ++ y])) names (map show [n + 2 .. n + 1 + needed])
           let (bindings' :: [String]) = zipWith (\x y -> x ++ y) names (map show [n + 2 .. n + 1 + needed])
           newvars <- liftM (pairUp bindings) vars

           let finalFuns = mfuns newvars
           let appliedBody = body newvars
           let varnums = map fst newvars
           (bindStmts,bindExprs,tbind) <- liftM unzip3 finalFuns
           (bodyStmts,bodyExpr,t') <- translateM this appliedBody
           let bindtyps = map javaType tbind
           let typ = javaType t'

           -- initialize declarations
           let mDecls = map (\(x, btyp) -> memberDecl (fieldDecl btyp (varDeclNoInit x))) (zip bindings' bindtyps)

           -- assign new created closures bindings to variables
           let assm = map (\(i,jz) -> assign (name [localvarstr ++ show i]) jz)
                          (varnums `zip` map unwrap (map Left bindExprs))
           let stasm = concatMap (\(a,b) -> a ++ [b]) (bindStmts `zip` assm) ++ bodyStmts ++ [assign (name [tempvarstr]) (unwrap (Left bodyExpr))]
           let letClass =
                 [localClass ("Let" ++ show n)
                              (classBody (memberDecl (fieldDecl objClassTy (varDeclNoInit tempvarstr)) :
                                          mDecls ++ [J.InitDecl False (J.Block stasm)]))

                 ,localVar (classTy ("Let" ++ show n))
                           (varDecl (localvarstr ++ show n)
                                    (instCreat (classTyp ("Let" ++ show n)) []))
                 ,localFinalVar typ (varDecl (localvarstr ++ show (n + 1))
                                             (cast typ (J.ExpName (name [localvarstr ++ show n, tempvarstr]))))]
           return (letClass,SingleVar (name [localvarstr ++ show (n + 1)]),t')-}
  {-
               do (n :: Int) <- get
                  let needed = length t
                  put (n + 2 + needed)
                  mfuns <- return (\defs -> forM (xs defs) (translateM this))
                  let vars = liftM (map (\(_,b,c) -> (b,c))) (mfuns (zip [n ..] t))
                  let (bindings :: [Var]) = [n + 2 .. n + 1 + needed]
                  newvars <- liftM (pairUp bindings) vars

                  let finalFuns = mfuns newvars
                  let appliedBody = body newvars
                  let varnums = map fst newvars
                  (bindStmts,bindExprs,tbind) <- liftM unzip3 finalFuns
                  (bodyStmts,bodyExpr,t') <- translateM this appliedBody
                  bindtyps <- mapM (javaType this) tbind
                  typ <- javaType this t'

                  -- initialize declarations
                  let mDecls = map (\(x, btyp) -> memberDecl (fieldDecl btyp (varDeclNoInit (localvarstr ++ show x)))) (zip bindings bindtyps)

                  -- assign new created closures bindings to variables
                  let assm = map (\(i,jz) -> assign (name [localvarstr ++ show i]) jz)
                                 (varnums `zip` map unwrap bindExprs)
                  let stasm = concatMap (\(a,b) -> a ++ [b]) (bindStmts `zip` assm) ++ bodyStmts ++ [assign (name [tempvarstr]) (unwrap bodyExpr)]
                  let letClass =
                        [localClass ("Let" ++ show n)
                                     (classBody (memberDecl (fieldDecl objClassTy (varDeclNoInit tempvarstr)) :
                                                 mDecls ++ [J.InitDecl False (J.Block stasm)]))

                        ,localVar (classTy ("Let" ++ show n))
                                  (varDecl (localvarstr ++ show n)
                                           (instCreat (classTyp ("Let" ++ show n)) []))
                        ,localFinalVar typ (varDecl (localvarstr ++ show (n + 1))
                                                    (cast typ (J.ExpName (name [localvarstr ++ show n, tempvarstr]))))]
                  return (letClass,var (localvarstr ++ show (n + 1)),t')
-}
                -- TODO: rest; just default to be able to run tests
      _ -> (case e of       Tuple _ -> error "zmdrdi tuple"
                            Proj _ _ -> error "proj"
                            LetRec _ _ _ _ -> error "letrec"
                            JNew _ _ -> error "new"
                            JMethod {} -> return ([], PrimLit J.Null, Unit)
                            JField  _ _ _ -> error "field"
                            _ -> error "jinej zmdr")
               ,

  translateScopeM =
    \e m ->
      case e of

        Body t ->
          do (s,je,t1) <- translateM this t
             return (s,je,Body t1)

        Kind lname f ->
          do (n :: Int) <- get
             put (n + 1) -- needed to distingush type variable
             let xl = name ["l" ++ lname ++ show n]
             let xo = name ["o" ++ lname ++ show n]
             (s,je,t1) <- translateScopeM this (f (xl, xo)) m
             return (s,je,Kind lname (\a -> substScope' (xl, xo) (TVar a) t1))

        Type lname t g ->
          do (n :: Int) <- get
             let x1 = case m of
                        Just ((_,s),_) -> s
                        Nothing -> name [lname ++ show n]
             {-let (x1',x2') = maybe (n + 1,n + 2) (\(i,_) -> (i,n+1)) m -- decide whether we have found the fixpoint closure or not
             --put (x2 + 1)
             --let nextInClosure = g (x2,t)-}
             let x2l = "l" ++ lname ++ show (n + 1)
             let x2o = "o" ++ lname ++ show (n + 1)
             put (n + 2)
             let (primitiveField, objectField) = dualArgFields (J.ExpName x1)
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
             (ostmts,oexpr,t1) <- translateScopeM this nextInClosure Nothing
             let t1' = case t1 of
                           Kind _ _ -> error "Found Kind, where Type or Body expected."
                           Type {} -> Forall t1
                           Body b -> b

             let (outAssignment, _) = multiAssignment ToField t1' (Left oexpr)
                                  (assign (name [closureOutputL]),
                                  assign (name [closureOutputO])) (x1, x1)

             let cvar = [localClassDecl ("Fun" ++ show n)
                                     closureClass
                                     (closureBodyGen [memberDecl $ fieldDecl (classTy closureClass)
                                                                             (varDecl (case x1 of J.Name [J.Ident s] -> s) J.This)]
                                                     (tempVars ++ ostmts ++ outAssignment)
                                                     (classTy closureClass))]

             let fstmt = [localVar (classTy "f2j.unbox.Closure") (varDecl (case x1 of J.Name [J.Ident s] -> s) (funInstCreate n))]

             return (cvar ++ fstmt, SingleVar x1 ,Type lname t (const t1))
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
