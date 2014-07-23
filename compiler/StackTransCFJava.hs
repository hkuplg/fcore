{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses  -XScopedTypeVariables -XKindSignatures -XUndecidableInstances -XOverlappingInstances #-}

module StackTransCFJava where

import Prelude hiding (init, last)

import qualified Language.Java.Syntax as J
import ClosureF
-- import Mixins
import Inheritance
import Data.Map as Map
import Data.Set as Set
import BaseTransCFJava
import ApplyTransCFJava
import StringPrefixes
import MonadLib

data TranslateStack m = TS {
  toTS :: Translate m -- supertype is a subtype of Translate (later on at least)
  }

instance {-(r :< Translate m) =>-} (:<) (TranslateStack m) (Translate m) where
   up              = up . toTS
--   override (TS fm ts) f  = TS (override fm f) ts

instance (:<) (TranslateStack m) (TranslateStack m) where -- reflexivity
  up = id

{-
instance (r :< TranslateStack m) => (:<) r (Translate m) where -- transitivity
  up = up . up
-}

whileApply :: J.Exp -> String -> J.Ident -> J.Type -> [J.BlockStmt]
whileApply cl ctemp tempOut outType = (J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "Next",J.Ident "next"])) J.EqualA (cl))))
         : (whileApplyLoop ctemp tempOut outType)
--Next.next = x8;
nextApply cl tempOut outType = [J.BlockStmt $ J.ExpStmt $ J.Assign (J.NameLhs (J.Name [J.Ident "Next",J.Ident "next"])) J.EqualA (cl),
                J.LocalVars [] outType [J.VarDecl (J.VarId tempOut) (Just (J.InitExp (J.Lit J.Null)))]]

-- copy-paste from ApplyOpt, TODO: modularize
transS :: (MonadState Int m, MonadState Bool m, MonadState (Map.Map J.Exp Int) m, MonadState (Set.Set J.Exp) m, MonadWriter Bool m, selfType :< TranslateStack m, selfType :< Translate m) => Mixin selfType (Translate m) (TranslateStack m)
transS this super = TS {
  toTS = T {  translateM = \e -> case e of
       CLam s ->
           do  tell False
               put False
               translateM super e

       CFix t s ->
           do  put False
               translateM super e

       --CTApp e t -> translateM super e

       CFIf0 e1 e2 e3 ->
                do  n <- get
                    put (n+1)
                    (genApplys :: Bool) <- get --state before
                    put True
                    (s1,j1,t1) <- translateM (up this) (CFPrimOp e1 J.Equal (CFLit 0))
                    put genApplys
                    (s2,j2,t2) <- translateM (up this) e2
                    put genApplys
                    (s3,j3,t3) <- translateM (up this) e3
                    let ifvarname = (ifresultstr ++ show n)
                    let refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
                    let ifresdecl = J.LocalVars [] (objType) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Nothing)])
                    let  (ifstmt, ifexp) = ifBody (s2, s3) (j1, j2, j3) n  -- uses a fresh variable
                    return (s1 ++ [ifresdecl,ifstmt], ifexp, t2)                    -- need to check t2 == t3

--TODO: merge common parts with BaseTransCF
       CApp e1 e2 ->
           do  tell True
               (n :: Int) <- get
               put (n+1)
               -- e1 e2, if c then e1 else e2, e1 T
               (genApplys :: Bool) <- get --state before
               put True --not last
               (s1,j1, CForall (Typ t1 g)) <- translateM (up this) e1
               put True --not last either
               (s2,j2,t2) <- translateM (up this) e2

               (env :: Map.Map J.Exp Int) <- get
               let t    = g ()
               let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
               let nje1 = case (Map.lookup j1 env) of Nothing -> J.Cast closureType j1
                                                      Just no -> var (tempvarstr ++ show no)
               (usedCl :: Set.Set J.Exp) <- get                                       
               maybeCloned <- case t of
                                       Body _ ->
                                           return nje1
                                       _ ->
                                           if (Set.member nje1 usedCl) then 
                                                return $ J.MethodInv (J.PrimaryMethodCall (nje1) [] (J.Ident "clone") [])
                                           else do
                                                put (Set.insert nje1 usedCl)
                                                return nje1

               let cvar = J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp (maybeCloned)))])
               let nje2 = case (Map.lookup j2 env) of Nothing -> j2
                                                      Just no -> var (tempvarstr ++ show no)
               let ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident localvarstr))) J.EqualA nje2) )
               let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
               s3 <- case (scope2ctyp t) of CInt ->
                                                        do (_, loc) <- genSubst j3 initIntCast
                                                           let h = case loc of J.ExpName (J.Name [z]) -> z
                                                           let applyCall = if genApplys then (whileApply (J.ExpName (J.Name [f])) ("c" ++ show n) h boxedIntType) else (nextApply (J.ExpName (J.Name [f])) h boxedIntType)
                                                           return ([cvar, ass] ++ applyCall)
                                            CForall (_) ->
                                                        do (_, loc) <- genSubst j3 initClosure
                                                           let h = case loc of J.ExpName (J.Name [z]) -> z
                                                           let applyCall = if genApplys then (whileApply (J.ExpName (J.Name [f])) ("c" ++ show n) h closureType) else (nextApply (J.ExpName (J.Name [f])) h closureType)
                                                           return ([cvar, ass] ++ applyCall)
                                            CTupleType (_) ->
                                                        do (_, loc) <- genSubst j3 initObjArray
                                                           let h = case loc of J.ExpName (J.Name [z]) -> z
                                                           let applyCall = if genApplys then (whileApply (J.ExpName (J.Name [f])) ("c" ++ show n) h objArrayType) else (nextApply (J.ExpName (J.Name [f])) h objArrayType)
                                                           return ([cvar, ass] ++ applyCall)
                                            _ ->
                                                        do (_, loc) <- genSubst j3 initObj
                                                           let h = case loc of J.ExpName (J.Name [z]) -> z
                                                           let applyCall = if genApplys then (whileApply (J.ExpName (J.Name [f])) ("c" ++ show n) h objType) else (nextApply (J.ExpName (J.Name [f])) h objType)
                                                           return ([cvar, ass] ++ applyCall)

               return (s1 ++ s2 ++ s3, j3, scope2ctyp t) -- need to check t1 == t2

       otherwise ->
            do  tell True
                put True
                translateM super e,

  translateScopeM = \e m -> 
             translateScopeM super e m
  }}

