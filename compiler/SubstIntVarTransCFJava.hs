{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts #-}

module SubstIntVarTransCFJava where
    
import Prelude hiding (init, last)
import Debug.Trace
import Data.List hiding (init, last)

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Identity

import qualified Language.Java.Syntax as J
import Language.Java.Pretty
import ClosureF
import Mixins
import Data.Char
import qualified Data.Map as Map
import BaseTransCFJava

data SubstIntVarTranslate m = VNT {
  toTST :: Translate m,
  translateSubst :: PCExp Int (Var, PCTyp Int) -> 
     m (([J.BlockStmt], J.Exp, PCTyp Int), Map.Map J.Exp J.Exp),
  translateScopeSubst :: Scope (PCExp Int (Var, PCTyp Int)) Int (Var, PCTyp Int) -> 
    Maybe (Int,PCTyp Int) ->
    m (([J.BlockStmt], J.Exp, TScope Int), Map.Map J.Exp J.Exp)
  }

  
-- translation that is substituting casts
transNewVar :: (MonadState Int m, MonadReader (Map.Map J.Exp J.Exp) m) => Open (SubstIntVarTranslate m)
transNewVar this = VNT { toTST = T {
  translateM = \e -> do result <- translateSubst this e 
                        return $ fst $ result,
  translateScopeM = \e m -> do result <- translateScopeSubst this e m
                               return $ fst $ result
  },
    translateSubst = \e -> case e of 
     CFPrimOp e1 op e2 ->
       do  ((s1,j1,t1), enva) <- translateSubst this e1
           env1 <- local (Map.union enva) ask
           (s3, jf1, env1') <- case j1 of J.Lit e -> return ([], j1,env1)
                                          _ -> case (Map.lookup j1 env1) of Just e -> return ([], e,env1)
                                                                            Nothing -> do n <- get
                                                                                          put (n+1)
                                                                                          let temp1 = var ("inttemp" ++ show n)
                                                                                          let x = Map.insert j1 temp1 env1
                                                                                          let defV1 = J.LocalVars [] (boxedIntType) ([J.VarDecl (J.VarId $ J.Ident ("tempint" ++ show n)) (Just (J.InitExp $ J.Cast boxedIntType j1))])
                                                                                          return ([defV1], temp1,x)
           ((s2,j2,t2), envb) <- local (Map.union env1') (translateSubst this e2)
           env2 <- local (Map.union envb) ask
           (s4, jf2, env2') <- case j2 of J.Lit e -> return ([], j2,env2)
                                          _ -> case (Map.lookup j2 env2) of Just e -> return ([], e,env2)
                                                                            Nothing -> do n <- get
                                                                                          put (n+1)
                                                                                          let temp2 = var ("inttemp" ++ show n)
                                                                                          let x = Map.insert j2 temp2 env2
                                                                                          let defV2 = J.LocalVars [] (boxedIntType) ([J.VarDecl (J.VarId $ J.Ident ("tempint" ++ show n)) (Just (J.InitExp $ J.Cast boxedIntType j2))])
                                                                                          return ([defV2], temp2,x)                                                                                          
        
           return ((s1 ++ s2 ++ s3 ++ s4, J.BinOp jf1 op jf2, t1), env2')
           
     CFif0 e1 e2 e3 ->
       do  n <- get
           put (n+1)
           ((s1,j1,t1), enva) <- translateSubst this (CFPrimOp e1 J.Equal (CFLit 0))
           ((s2,j2,t2), envb) <- local (Map.union enva) (translateSubst this e2)
           ((s3,j3,t3), envc) <- local (Map.union envb) (translateSubst this e3)
           let ifvarname = (localVarPrefs!!0 ++ show n)
           let refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
           let ifresdecl = J.LocalVars [] (J.RefType (refType "Object")) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Nothing)])
           let  (ifstmt, ifexp) = ifBody (s2, s3) (j1, j2, j3) n  -- uses a fresh variable        
           return ((s1 ++ [ifresdecl,ifstmt], ifexp, t2), envc)                     -- need to check t2 == t3

     otherwise -> do result <- translateM (toTST this) e
                     env <- ask
                     return (result, env),
    translateScopeSubst = \e m -> do result <- translateScopeM (toTST this) e m
                                     env <- ask
                                     return (result, env)
  }

