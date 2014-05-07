{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XScopedTypeVariables #-}

module SubstIntVarTransCFJava where
    
import Prelude hiding (init, last)
import Debug.Trace
import Data.List hiding (init, last)

import Control.Monad.Identity

import qualified Language.Java.Syntax as J
import Language.Java.Pretty
import ClosureF
import Mixins
import Data.Char
import qualified Data.Map as Map
import BaseTransCFJava
import StringPrefixes
import MonadLib

data SubstIntVarTranslate (f :: (* -> *) -> *) m = VNT {
  toTST :: f m
  }

instance (f :< Translate) => (SubstIntVarTranslate f) :< Translate where
   to                           = to . toTST 
   override (VNT fm) f  = VNT (override fm f)

-- translation that is substituting casts; TODO: test

{-
transNewVar :: (MonadState Int m, MonadState (Map.Map String Int) n, f :< Translate) => n :-> m -> Open (SubstIntVarTranslate f m)
transNewVar view this = override this (\trans -> trans {
  translateM = \e -> case e of 
     CFPrimOp e1 op e2 ->
       do  (s1,j1,t1) <- translateM (to this) e1
           (env1 :: Map.Map String Int) <- getV view
           (s3, jf1) <- case j1 of J.Lit e -> return ([], j1)
                                   J.ExpName (J.Name [J.Ident x]) -> case (Map.lookup x env1) of Just e -> return ([], var (castedintstr ++ show e))
                                                                                                 Nothing -> do (n :: Int) <- get
                                                                                                               put (n+1)
                                                                                                               let temp1 = var (castedintstr ++ show n)
                                                                                                               putV view (Map.insert x n env1)
                                                                                                               let defV1 = J.LocalVars [] (boxedIntType) ([J.VarDecl (J.VarId $ J.Ident (castedintstr ++ show n)) (Just (J.InitExp $ J.Cast boxedIntType j1))])
                                                                                                               return ([defV1], temp1)
           (s2,j2,t2) <- translateM (to this) e2
           (env2 :: Map.Map String Int) <- getV view
           (s4, jf2) <- case j2 of J.Lit e -> return ([], j2)
                                   J.ExpName (J.Name [J.Ident x]) -> case (Map.lookup x env2) of Just e -> return ([], var (castedintstr ++ show e))
                                                                                                 Nothing -> do (n :: Int) <- get
                                                                                                               put (n+1)
                                                                                                               let temp2 = var (castedintstr ++ show n)
                                                                                                               putV view (Map.insert x n env2)
                                                                                                               let defV2 = J.LocalVars [] (boxedIntType) ([J.VarDecl (J.VarId $ J.Ident (castedintstr ++ show n)) (Just (J.InitExp $ J.Cast boxedIntType j2))])
                                                                                                               return ([defV2], temp2)                                                                                          
        
           return (s1 ++ s2 ++ s3 ++ s4, J.BinOp jf1 op jf2, t1)
           
     CFif0 e1 e2 e3 ->
       do  n <- get
           put (n+1)
           (s1,j1,t1) <- translateM (to this) (CFPrimOp e1 J.Equal (CFLit 0))
           (s2,j2,t2) <- translateM (to this) e2
           (s3,j3,t3) <- translateM (to this) e3
           let ifvarname = (ifresultstr ++ show n)
           let refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
           let ifresdecl = J.LocalVars [] (J.RefType (refType "Object")) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Nothing)])
           let  (ifstmt, ifexp) = ifBody (s2, s3) (j1, j2, j3) n  -- uses a fresh variable        
           return (s1 ++ [ifresdecl,ifstmt], ifexp, t2)                    -- need to check t2 == t3

     otherwise -> translateM (to this) e,           
  translateScopeM = \e m -> translateScopeM (to this) e m
  })
-}

transNewVar :: (MonadState Int m, MonadState (Map.Map String Int) m, f :< Translate) => Open (SubstIntVarTranslate f m)
transNewVar this = override this (\trans -> trans {
  translateM = \e -> case e of 
     CFPrimOp e1 op e2 ->
       do  (s1,j1,t1) <- translateM (to this) e1
           (env1 :: Map.Map String Int) <- get
           (s3, jf1) <- case j1 of J.Lit e -> return ([], j1)
                                   J.ExpName (J.Name [J.Ident x]) -> case (Map.lookup x env1) of Just e -> return ([], var (castedintstr ++ show e))
                                                                                                 Nothing -> do (n :: Int) <- get
                                                                                                               put (n+1)
                                                                                                               let temp1 = var (castedintstr ++ show n)
                                                                                                               put (Map.insert x n env1)
                                                                                                               let defV1 = J.LocalVars [] (boxedIntType) ([J.VarDecl (J.VarId $ J.Ident (castedintstr ++ show n)) (Just (J.InitExp $ J.Cast boxedIntType j1))])
                                                                                                               return ([defV1], temp1)
           (s2,j2,t2) <- translateM (to this) e2
           (env2 :: Map.Map String Int) <- get
           (s4, jf2) <- case j2 of J.Lit e -> return ([], j2)
                                   J.ExpName (J.Name [J.Ident x]) -> case (Map.lookup x env2) of Just e -> return ([], var (castedintstr ++ show e))
                                                                                                 Nothing -> do (n :: Int) <- get
                                                                                                               put (n+1)
                                                                                                               let temp2 = var (castedintstr ++ show n)
                                                                                                               put (Map.insert x n env2)
                                                                                                               let defV2 = J.LocalVars [] (boxedIntType) ([J.VarDecl (J.VarId $ J.Ident (castedintstr ++ show n)) (Just (J.InitExp $ J.Cast boxedIntType j2))])
                                                                                                               return ([defV2], temp2)                                                                                          
        
           return (s1 ++ s2 ++ s3 ++ s4, J.BinOp jf1 op jf2, t1)
           
     CFif0 e1 e2 e3 ->
       do  n <- get
           put (n+1)
           (s1,j1,t1) <- translateM (to this) (CFPrimOp e1 J.Equal (CFLit 0))
           (s2,j2,t2) <- translateM (to this) e2
           (s3,j3,t3) <- translateM (to this) e3
           let ifvarname = (ifresultstr ++ show n)
           let refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
           let ifresdecl = J.LocalVars [] (J.RefType (refType "Object")) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Nothing)])
           let  (ifstmt, ifexp) = ifBody (s2, s3) (j1, j2, j3) n  -- uses a fresh variable        
           return (s1 ++ [ifresdecl,ifstmt], ifexp, t2)                    -- need to check t2 == t3

     otherwise -> translateM (to this) e,           
  translateScopeM = \e m -> translateScopeM (to this) e m
  })
  