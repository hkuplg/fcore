{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses  -XScopedTypeVariables -XKindSignatures #-}

module StackTransCFJava where

import Prelude
import Debug.Trace
import Data.List

import qualified Language.Java.Syntax as J
import Language.Java.Pretty
import ClosureF
import Mixins

-- import ApplyTransCFJava 
import BaseTransCFJava
import StringPrefixes
import MonadLib

type Schedule = [([J.BlockStmt],[J.BlockStmt])]

data TranslateStack (f :: (* -> *) -> *) m = TS {
  toTS :: f m, -- supertype is a subtype of Translate
  translateScheduleM :: PCExp Int (Var, PCTyp Int) -> m ([J.BlockStmt], J.Exp, Schedule, PCTyp Int)
  }
                      
instance (f :< Translate) => (:<) (TranslateStack f) Translate where
   to              = to . toTS 
   override (TS fm ts) f  = TS (override fm f) ts 


sstack :: Schedule -> [J.BlockStmt]
sstack []              = [] -- Sigma-Empty
sstack ((s1,s2) : [])  = s1 ++ s2 -- Sigma-One
sstack ((s1,[]) : ss)  = s1 ++ sstack ss -- Sigma-Many1
sstack ((s1,s2) : ss)  = s1 ++ [push (closure ss)] ++ s2 -- Sigma-Many2
   where closure ss = jexp [] (Just $ J.Block (sstack ss))
         
-- push method call.

push :: J.Exp -> J.BlockStmt
push e = J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall (J.ExpName (J.Name [stack])) [] (J.Ident "push") [e])))
  where stack = (J.Ident "Stack")

transS :: (MonadState Int m, MonadWriter Bool m, f :< Translate) => Open (TranslateStack f m)
transS this = TS {
  toTS = override (toTS this) (\trans -> trans {
    translateM = \e -> case e of 
       CApp _ _ ->
         do  (s1,je,sig,t) <- translateScheduleM this e 
             return (s1 ++ (sstack sig), je, t)
       
       otherwise -> translateM (to this) e, 
    translateScopeM = translateScopeM (to this)
    }),
  
  translateScheduleM = \e -> case e of
    CApp e1 e2  -> -- CJ-App-Sigma
      do  (n :: Int) <- get
          put (n+1)
          (s1,j1,sig1,CForall (Typ t1 g)) <- translateScheduleM this e1
          (s2,j2,sig2,t2) <- translateScheduleM this e2
          let t    = g ()
          let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
          let cvar = J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp (J.Cast closureType j1)))])
          let ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "x"))) J.EqualA j2) ) 
          let p = push (J.ExpName (J.Name [f]))
          let sig = case t of -- checking the type whether to generate the apply() call
                      Body _ -> ([cvar,ass],[p])
                      _ -> ([cvar,ass],[])
          let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
          return (s1 ++ s2, j3, sig1 ++ (sig2 ++ [sig]), scope2ctyp t) -- need to check t1 == t2
    otherwise ->
         do  (s,j,t) <- translateM (to this) e
             return (s,j,[],t)
  } 
