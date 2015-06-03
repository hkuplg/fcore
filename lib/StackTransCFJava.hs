{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , OverlappingInstances
           , RankNTypes
           , ScopedTypeVariables
           , TypeOperators
           , UndecidableInstances #-}
{- |
Module      :  StackTransCFJava
Description :  Translation of FCore to Java with Stack-opt
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Jeremy <bixuanxbi@gmail.com>, Tomas <tomtau@connect.hku.hk>
Stability   :  stable
Portability :  non-portable (MPTC)

This module augments the basic translation of FCore to Java to support
tail-call elimination.
-}

module StackTransCFJava where
-- TODO: isolate all hardcoded strings to StringPrefixes (e.g. Fun)

import qualified Language.Java.Syntax as J
import           Data.Maybe (fromJust)

import           BaseTransCFJava
import           ClosureF
import           Inheritance
import           JavaEDSL
import           MonadLib
import           StringPrefixes


data TranslateStack m = TS {
  toTS :: Translate m -- supertype is a subtype of Translate (later on at least)
  }

instance {-(r :< Translate m) =>-} (:<) (TranslateStack m) (Translate m) where
   up              = up . toTS

instance (:<) (TranslateStack m) (TranslateStack m) where -- reflexivity
  up = id

nextClass ::(Monad m) => Translate m -> m String
nextClass this = liftM2 (++) (getPrefix this) (return "Next")

whileApplyLoop :: (Monad m) => Translate m -> Bool -> String -> String -> J.Type -> J.Type -> m [J.BlockStmt]
whileApplyLoop this flag ctemp tempOut outType ctempCastTyp = do
  closureClass <- liftM2 (++) (getPrefix this) (return "Closure")
  let closureType' = classTy closureClass
  nextName <- nextClass (up this)
  let doWhileStmts = [localVar closureType' (varDeclNoInit ctemp),
                      localVar outType (varDecl tempOut (case outType of
                                                          J.PrimType J.IntT -> J.Lit (J.Int 0) -- TODO: could be bug
                                                          _ -> J.Lit J.Null)),
                      bStmt (J.Do (J.StmtBlock (block [assign (name [ctemp]) (J.ExpName $ name [nextName, "next"])
                                                      ,assign (name [nextName, "next"]) (J.Lit J.Null)
                                                      ,bStmt (methodCall [ctemp, "apply"] [])]))
                             (J.BinOp (J.ExpName $ name [nextName, "next"])
                              J.NotEq
                              (J.Lit J.Null))),
                      assign (name [tempOut]) (cast outType (J.FieldAccess (fieldAccExp (cast ctempCastTyp (left $ var ctemp)) closureOutput)))]
  if flag -- False means stack with apply
    then return doWhileStmts
    else return (let (l1,l2) = splitAt 2 doWhileStmts
                 in head l1:l2)

whileApplyLoopMain :: (Monad m) => Translate m -> String -> String -> J.Type -> J.Type -> m [J.BlockStmt]
whileApplyLoopMain this ctemp tempOut outType ctempCastTyp =
  do closureClass <- liftM2 (++) (getPrefix this) (return "Closure")
     let closureType' = classTy closureClass
     nextName <- nextClass (up this)
     let nextNEqNull = J.BinOp (J.ExpName $ name [nextName,"next"]) J.NotEq (J.Lit J.Null)
     let loop =
           [bStmt (J.Do (J.StmtBlock
                           (block [assign (name [ctemp]) (J.ExpName $ name [nextName,"next"])
                                  ,assign (name [nextName,"next"])
                                          (J.Lit J.Null)
                                  ,bStmt (methodCall [ctemp,"apply"]
                                                     [])]))
                        nextNEqNull)
           ,assign (name [tempOut]) (cast outType (J.FieldAccess (fieldAccExp (cast ctempCastTyp (left $ var ctemp)) closureOutput)))]
     return [localVar closureType' (varDeclNoInit ctemp)
            ,localVar outType (varDecl tempOut (J.MethodInv (J.MethodCall (name ["apply"]) [])))
            ,bStmt (J.IfThen nextNEqNull (J.StmtBlock (block loop)))]

containsNext :: [J.BlockStmt] -> Bool
containsNext =
  foldr ((||) .
         (\x ->
            case x of
              (J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident _nextClass,J.Ident "next"])) J.EqualA _))) -> True
              _ -> False))
        False

-- ad-hoc fix for final-returned expressions in Stack translation
empyClosure :: Monad m => Translate m -> J.Exp -> String -> m J.BlockStmt
empyClosure this outExp box =
  do closureClass <- liftM (++ box) $ liftM2 (++) (getPrefix this) (return "Closure")
     nextName <- nextClass (up this)
     return (assign (name [nextName,"next"])
                    (J.InstanceCreation
                       []
                       (classTyp closureClass)
                       []
                       (Just (classBody [memberDecl
                                           (methodDecl
                                              [annotation "Override",J.Public]
                                              (Just (classTy closureClass))
                                              "clone"
                                              []
                                              returnNull)
                                        ,memberDecl
                                           (methodDecl
                                              [annotation "Override",J.Public]
                                              Nothing
                                              "apply"
                                              []
                                              (Just (block [assign (name [closureOutput]) outExp])))]))))

whileApply :: (Monad m) => Translate m -> Bool -> J.Exp -> String -> String -> J.Type -> J.Type -> m [J.BlockStmt]
whileApply this flag cl ctemp tempOut outType ctempCastTyp =
  do loop <- whileApplyLoop this flag ctemp tempOut outType ctempCastTyp
     nextName <- nextClass (up this)
     return (assign (name [nextName,"next"]) cl : loop)

--e.g. Next.next = x8;
nextApply :: (Monad m) => Translate m -> J.Exp -> String -> J.Type -> m [J.BlockStmt]
nextApply this cl tempOut outType =
  do nextName <- nextClass this
     return [assign (name [nextName,"next"]) cl
             ,localVar outType
                       (varDecl tempOut
                                (case outType of
                                   J.PrimType J.IntT -> J.Lit (J.Int 0)
                                   J.PrimType J.CharT -> J.Lit (J.Char 'a') --TODO: better default value?
                                   _ -> J.Lit J.Null))]

transS :: forall m selfType.
          (MonadState Int m,
           MonadReader Bool m,
           selfType :< TranslateStack m,
           selfType :< Translate m)
          => Mixin selfType (Translate m) (TranslateStack m)
transS this super =
  TS {toTS = super {

         translateM =
           \e ->
             case e of
               -- count abstraction as in tail position
               Lam _ _ -> local (True ||) $ translateM super e
               Fix _ _ _ _ -> local (True ||) $ translateM super e
               -- type application just inherits existing flag
               TApp _ _ -> translateM super e
               -- if e1 e2 e3: e1 can't be in tail position, e2 and e3 inherits flag
               If e1 e2 e3 ->
                 translateIf (up this) (local (False &&) $ translateM (up this) e1) (translateM (up this) e2) (translateM (up this) e3)
               App e1 e2 -> do
                 translateApply (up this) False (local (False &&) $ translateM (up this) e1) (local (False &&) $ translateM (up this) e2)
               -- let e1 e2: e1 can't be in tail position, e2 inherits flag
               Let _ expr body ->
                 do (s1,j1,t1) <- local (False &&) $ translateM (up this) expr
                    translateLet (up this) (s1,j1,t1) body
               -- case scrutinee can't be in tail position, alts inherit flag
               Case scrut alts -> do
                      (scrutStmts, scrutExpr, _) <- local (False &&) $ translateM (up this) scrut
                      (altsStmts, varName, typ) <- transAlts (up this) scrutExpr alts
                      return (scrutStmts ++ altsStmts, varName, typ)
               -- the last expression in the sequence inherit flag
               SeqExprs es -> do
                 let allButLast = init es
                 let lastExpr = last es
                 emost <- mapM (local (False &&) . translateM (up this)) allButLast
                 (eLastStmt, eLastExpr, eLastType) <- translateM (up this) lastExpr
                 let statements = concatMap (\(x,_,_) -> x) emost
                 return (statements ++ eLastStmt, eLastExpr, eLastType)
               -- count other expressions as not in tail position
               _ -> local (False &&) $ translateM super e

         ,genApply =
            \f _ x jType ctempCastTyp ->
             do (tailPosition :: Bool) <- ask
                (n :: Int) <- get
                put (n + 1)
                flag <- withApply (up this) -- False means Stack with Apply
                if tailPosition
                  then nextApply (up this) f x jType
                  else whileApply (up this) flag f ("c" ++ show n) x jType ctempCastTyp
         ,genRes = \_ _ -> return []
         ,stackMainBody =
              \t ->
               do closureClass <- liftM2 (++) (getPrefix (up this)) (return "Closure")
                  retType <- applyRetType (up this) t
                  loop <- whileApplyLoopMain (up this) "c" "result" (fromJust retType) (classTy closureClass)
                  return (loop ++ [bStmt (classMethodCall (left $ var "System.out") "println" [left $ var "result"])])
         ,applyRetType =
                \t ->
                 (case t of
                   JClass "java.lang.Integer" -> return $ Just $ classTy "java.lang.Integer"
                   JClass "java.lang.Boolean" -> return $ Just $ classTy "java.lang.Boolean"
                   CFInt -> return $ Just $ classTy "java.lang.Integer"
                   _ -> return $ Just objClassTy)
         ,createWrap =
                  \nam expr ->
                   do (bs,e,t) <- translateM (up this) expr
                      returnType <- applyRetType (up this) t
                      let returnStmt = [bStmt $ J.Return $ Just (unwrap e)]
                      -- box <- getBox (up this) t
                      -- empyClosure' <- empyClosure (up this) (unwrap e) box
                      mainbody <- stackMainBody (up this) t
                      let stackDecl =
                            wrapperClass
                              nam
                              (bs ++
                               -- (if containsNext bs then [] else [empyClosure']) ++
                               returnStmt)
                              returnType (Just $ J.Block mainbody)
                      return (createCUB (up this :: Translate m) [stackDecl]
                             ,t)}}

-- Alternative version of transS that interacts with the Apply translation

transSA :: (MonadState Int m, MonadReader Bool m, selfType :< TranslateStack m, selfType :< Translate m) => Mixin selfType (Translate m) (TranslateStack m)
transSA this super = TS {toTS = (up (transS this super)) {
   withApply = return False
  }}

-- Alternative version of transS that interacts with the Unbox translation
-- transSU :: (MonadState Int m, MonadReader (Int, Bool) m, selfType :< TranslateStack m, selfType :< Translate m) => Mixin selfType (Translate m) (TranslateStack m)
-- transSU this super =
--   TS {toTS = (up (transS this super)) {
--          getBox = \t -> case t of
--                          CFInt -> return "BoxInt"
--                          _ -> return "BoxBox",
--          applyRetType = \t -> (case t of
--                                 JClass "java.lang.Integer" -> return $ Just $ J.PrimType J.IntT
--                                 JClass "java.lang.Boolean" -> return $ Just $ J.PrimType J.BooleanT
--                                 CFInt -> return $ Just $ J.PrimType J.IntT
--                                 _ -> return $ Just objClassTy),
--          stackMainBody = \t -> do
--            closureClass <- liftM2 (++) (getPrefix (up this)) (return "Closure")
--            let closureType' = classTy closureClass
--            nextName <- nextClass (up this)
--            let finalType = case t of
--                             CFInt -> "Int"
--                             _ -> "Box"
--            let resultType = case t of
--                              CFInt -> J.PrimType J.IntT
--                              CFChar -> J.PrimType J.CharT
--                              JClass "java.lang.Integer" -> classTy "java.lang.Integer"
--                              _ -> objClassTy

--            let nextNEqNull = J.BinOp (J.ExpName $ name [nextName, "next"])
--                              J.NotEq
--                              (J.Lit J.Null)

--            let loop = [bStmt (J.Do (J.StmtBlock (block [assign (name ["c"]) (J.ExpName $ name [nextName, "next"])
--                                                        ,assign (name [nextName, "next"]) (J.Lit J.Null)
--                                                        ,bStmt (methodCall ["c", "apply"] [])]))
--                               (J.BinOp (J.ExpName $ name [nextName, "next"])
--                                J.NotEq
--                                (J.Lit J.Null))),
--                        bStmt (J.IfThenElse
--                               (J.InstanceOf (left $ var "c") (J.ClassRefType $ classTyp (closureClass ++ "Int" ++ finalType)))
--                               (assignE
--                                 (name ["result"])
--                                 (cast resultType
--                                  (J.FieldAccess (fieldAccExp
--                                                  (cast (classTy (closureClass ++ "Int" ++ finalType)) (left $ var "c"))
--                                                  closureOutput))))
--                               (assignE
--                                (name ["result"])
--                                (cast resultType
--                                 (J.FieldAccess (fieldAccExp
--                                                 (cast (classTy (closureClass ++ "Box" ++ finalType)) (left $ var "c"))
--                                                 closureOutput)))))]

--            return (localVar closureType' (varDeclNoInit "c") :
--                    localVar resultType (varDecl "result" (J.MethodInv (J.MethodCall (name ["apply"]) []))) :
--                    bStmt (J.IfThen nextNEqNull (J.StmtBlock (block loop))) :
--                    [bStmt (classMethodCall (left $ var "System.out") "println" [left $ var "result"])])
--          }}


-- Alternative version of transS that interacts with the Unbox and Apply translation
-- transSAU :: (MonadState Int m, MonadReader (Int, Bool) m, selfType :< TranslateStack m, selfType :< Translate m) => Mixin selfType (Translate m) (TranslateStack m)
-- transSAU this super = TS {toTS = (up (transSU this super)) {
--    -- genRes = \t s -> if (last t) then return [] else genRes super t s
--   genApply = \f _ x jType ctempCastTyp ->
--       do (_ :: Int, tailPosition :: Bool) <- ask
--          (n :: Int) <- get
--          put (n+1)
--          if tailPosition
--            then nextApply (up this) f x jType
--            else whileApply (up this) False f ("c" ++ show n) x jType ctempCastTyp
--   }}
