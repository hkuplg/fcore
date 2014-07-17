{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses  -XScopedTypeVariables -XKindSignatures -XUndecidableInstances -XOverlappingInstances #-}

module StackTransCFJava where

import Prelude hiding (init, last)

import qualified Language.Java.Syntax as J
import ClosureF
-- import Mixins
import Inheritance
import Data.Map as Map
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
transS :: (MonadState Int m, MonadState Bool m, MonadState (Map.Map J.Exp Int) m, MonadWriter Bool m, selfType :< TranslateStack m, selfType :< Translate m) => Mixin selfType (Translate m) (TranslateStack m)
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
               let maybeCloned = case t of
                                       Body _ ->
                                           nje1
                                       _ ->
                                           J.MethodInv (J.PrimaryMethodCall (nje1) [] (J.Ident "clone") [])

               let cvar = J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp (maybeCloned)))])
               let nje2 = case (Map.lookup j2 env) of Nothing -> j2
                                                      Just no -> var (tempvarstr ++ show no)
               let ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident localvarstr))) J.EqualA nje2) )
               let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
               s3 <- case t of -- checking the type whether to generate the apply() call
                               Body _ -> do
                                case (scope2ctyp t) of CInt ->
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
                               _ -> do return [cvar,ass]

               return (s1 ++ s2 ++ s3, j3, scope2ctyp t) -- need to check t1 == t2

       otherwise ->
            do  tell True
                put True
                translateM super e,

  translateScopeM = \e m -> case e of
      Typ t g ->
        do  n <- get
            let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
            let (v,n')  = maybe (n+1,n+2) (\(i,_) -> (i,n+1)) m -- decide whether we have found the fixpoint closure or not
            put n'
            let self = J.Ident (localvarstr ++ show v)
            ((s,je,t1), closureCheck::Bool) <- listen $ translateScopeM (up this) (g (Left v,t)) Nothing
            (env :: Map.Map J.Exp Int) <- get
            let nje = case (Map.lookup je env) of Nothing -> je
                                                  Just no -> var (tempvarstr ++ show no)
            let cvar = refactoredScopeTranslationBit nje s v n closureCheck -- standardTranslation nje s i n (Set.member n envs)
            return (cvar,J.ExpName (J.Name [f]), Typ t (\_ -> t1) )

      otherwise ->
          do tell True
             translateScopeM super e m
  }}

