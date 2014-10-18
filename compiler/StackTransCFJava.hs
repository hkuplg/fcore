{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           -- , KindSignatures
           , MultiParamTypeClasses
           , OverlappingInstances
           , RankNTypes
           , ScopedTypeVariables
           , TypeOperators
           , UndecidableInstances #-}

module StackTransCFJava where

import Prelude hiding (init, last)

import qualified Language.Java.Syntax as J
import ClosureF
import Inheritance
import BaseTransCFJava
import ApplyTransCFJava (last)
import MonadLib
-- import StringPrefixes
import Panic
import JavaEDSL

data TranslateStack m = TS {
  toTS :: Translate m -- supertype is a subtype of Translate (later on at least)
  }

instance {-(r :< Translate m) =>-} (:<) (TranslateStack m) (Translate m) where
   up              = up . toTS

instance (:<) (TranslateStack m) (TranslateStack m) where -- reflexivity
  up = id

nextClass ::(Monad m) => (Translate m) -> m String
nextClass this = liftM2 (++) (getPrefix this) (return "Next")

whileApplyLoop :: (Monad m) => Translate m -> String -> String -> J.Type -> J.Type -> m [J.BlockStmt]
whileApplyLoop this ctemp tempOut outType ctempCastTyp = do
  closureClass <- liftM2 (++) (getPrefix this) (return "Closure")
  let closureType' = classTy closureClass
  nextName <- nextClass (up this)
  return [localVar closureType' (varDeclNoInit ctemp),
          localVar outType (varDecl tempOut (case outType of
                                              J.PrimType J.IntT -> J.Lit (J.Int 0)
                                              _ -> (J.Lit J.Null))),
          bStmt (J.Do (J.StmtBlock (block [assign (name [ctemp]) (J.ExpName $ name [nextName, "next"])
                                          ,assign (name [nextName, "next"]) (J.Lit J.Null)
                                          ,bStmt (methodCall [ctemp, "apply"] [])]))
                 (J.BinOp (J.ExpName $ name [nextName, "next"])
                  J.NotEq
                  (J.Lit J.Null))),
          assign (name [tempOut]) (cast outType (J.FieldAccess (fieldAccExp (cast ctempCastTyp (var ctemp)) "out")))]

containsNext :: [J.BlockStmt] -> Bool
containsNext l = foldr (||) False $ map (\x -> case x of (J.BlockStmt (J.ExpStmt (J.Assign (
                                                                J.NameLhs (J.Name [J.Ident _nextClass,J.Ident "next"])) J.EqualA _))) -> True
                                                         _ -> False) l

-- ad-hoc fix for final-returned expressions in Stack translation
empyClosure :: Monad m => Translate m -> J.Exp -> String -> m J.BlockStmt
empyClosure this outExp box = do
  closureClass <- liftM (++ box) $ liftM2 (++) (getPrefix this) (return "Closure")
  nextName <- nextClass (up this)
  return (assign (name [nextName, "next"])
          (J.InstanceCreation [] (classTyp closureClass) []
           (Just (classBody [memberDecl
                             (methodDecl
                              [annotation "Override",J.Public]
                              (Just (classTy closureClass))
                              "clone"
                              []
                              returnNull),
                             (memberDecl
                              (methodDecl
                               [annotation "Override", J.Public]
                               Nothing
                               "apply"
                               []
                               (Just (block [assign (name ["out"]) outExp]))))]))))

whileApply :: (Monad m) => Translate m -> J.Exp -> String -> String -> J.Type -> J.Type -> m [J.BlockStmt]
whileApply this cl ctemp tempOut outType ctempCastTyp = do
  loop <- whileApplyLoop this ctemp tempOut outType ctempCastTyp
  nextName <- nextClass (up this)
  return ((assign (name [nextName, "next"]) cl) : loop)

--e.g. Next.next = x8;
nextApply :: (Monad m) => Translate m -> J.Exp -> String -> J.Type -> m [J.BlockStmt]
nextApply this cl tempOut outType = do
  nextName <- nextClass this
  return ([assign (name [nextName,"next"]) cl,
           localVar outType (varDecl tempOut (if outType == J.PrimType J.IntT
                                              then J.Lit (J.Int 0) -- TODO: potential bug
                                              else J.Lit J.Null))])

applyCall :: J.BlockStmt
applyCall = bStmt $ methodCall ["apply"] []

stackMainBody :: Monad m => Translate m -> Type t -> m [J.BlockStmt]
stackMainBody this t = do
  loop <- whileApplyLoop this "c" "result" (case t of
                                                  JClass "java.lang.Integer" -> classTy "java.lang.Integer"
                                                  _ -> objClassTy) closureType
  return (applyCall : loop ++ [bStmt (classMethodCall (var "System.out") "println" [var "result"])])

transS :: forall m selfType . (MonadState Int m, MonadReader Bool m, selfType :< TranslateStack m, selfType :< Translate m) => Mixin selfType (Translate m) (TranslateStack m)
transS this super = TS {toTS = super {
  translateM = \e -> case e of
       Lam s       -> local (&& False) $ translateM super e
       Fix t s     -> local (&& False) $ translateM super e
       TApp _ _    -> local (|| False) $ translateM super e
       If e1 e2 e3 -> translateIf (up this) (local (|| True) $ translateM (up this) e1) (translateM (up this) e2) (translateM (up this) e3)
       App e1 e2   -> translateApply (up this) (local (|| True) $ translateM (up this) e1) (local (|| True) $ translateM (up this) e2)
       otherwise   -> local (|| True) $ translateM super e,

  genApply = \f t x jType ctempCastTyp ->
      do (genApplys :: Bool) <- ask
         (n :: Int) <- get
         put (n+1)
         case x of
            J.ExpName (J.Name [J.Ident h]) -> if genApplys then -- relies on translated code!
                                        (whileApply (up this) (J.ExpName (J.Name [f])) ("c" ++ show n) h jType ctempCastTyp)
                                      else nextApply (up this) (J.ExpName (J.Name [f])) h jType
            _ -> panic "expected temporary variable name" ,

  genRes = \t s -> return [],

  createWrap = \name exp ->
        do (bs,e,t) <- translateM (up this) exp
           box <- getBox (up this)
           empyClosure' <- empyClosure (up this) e box
           mainbody <- stackMainBody (up this) t
           let stackDecl = wrapperClass name (bs ++ (if (containsNext bs) then [] else [empyClosure'])) Nothing (Just $ J.Block mainbody)
           return (createCUB  (up this :: Translate m) [stackDecl], t)

  }}

-- Alternative version of transS that interacts with the Apply translation

transSA :: (MonadState Int m, MonadReader Bool m, selfType :< TranslateStack m, selfType :< Translate m) => Mixin selfType (Translate m) (TranslateStack m)
transSA this super = TS {toTS = (up (transS this super)) {
   genRes = \t s -> if (last t) then return [] else genRes super t s
  }}

-- Alternative version of transS that interacts with the Unbox translation
transSU :: (MonadState Int m, MonadReader Bool m, selfType :< TranslateStack m, selfType :< Translate m) => Mixin selfType (Translate m) (TranslateStack m)
transSU this super =
  TS {toTS = (up (transS this super)) {
         getBox = return "BoxBox"
         }}
