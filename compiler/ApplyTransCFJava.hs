{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XConstraintKinds -XScopedTypeVariables #-}

module ApplyTransCFJava where

import Prelude hiding (init, last)

import qualified Data.Set as Set

import qualified Language.Java.Syntax as J
import ClosureF
-- import Mixins
import Inheritance
import BaseTransCFJava 
import StringPrefixes
import MonadLib

data ApplyOptTranslate m = NT {toT :: Translate m}

instance (:<) (ApplyOptTranslate m) (Translate m) where
   up              = up . toT

instance (:<) (ApplyOptTranslate m) (ApplyOptTranslate m) where --reflexivity
   up              = id

-- To be used if we decide to implement full applyOpt
getCvarAss t f n j1 j2 = do
                   (usedCl :: Set.Set J.Exp) <- get                                       
                   maybeCloned <-  case t of
                                               Body _ ->
                                                   return j1
                                               _ ->
                                                   if (Set.member j1 usedCl) then 
                                                        return $ J.MethodInv (J.PrimaryMethodCall (j1) [] (J.Ident "clone") [])
                                                   else do
                                                        put (Set.insert j1 usedCl)
                                                        return j1
                                      
                   let cvar = J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp (maybeCloned)))])
                   let ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident localvarstr))) J.EqualA j2) )
                   return [cvar, ass]                                    

countAbs (Body _)  = 0 :: Int
countAbs (Typ t g) = 1 + countAbs (g ()) 
countAbs (Kind g)  = countAbs (g 0)

-- main translation function
transApply :: (MonadState Int m, MonadReader InitVars m, selfType :< ApplyOptTranslate m, selfType :< Translate m) => Mixin selfType (Translate m) (ApplyOptTranslate m) -- generalize to super :< Translate m?
transApply this super = NT {toT = T { 
  translateM = \e -> case e of 
     CApp e1 e2 -> 
       do  (n :: Int) <- get -- overriding App code
           put (n+1)
           (s1,j1, CForall (Typ t1 g)) <- translateM (up this) e1
           (s2,j2,t2) <- translateM (up this) e2
           let t    = g ()
           let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
           --cvarass <- getCvarAss t f n j1 j2
           let cvarass = [ J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp j1))]),
                           J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident localvarstr))) J.EqualA j2) )]            
                                            
           let genApply x y =  if (countAbs t == 0) then [J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall (J.ExpName (J.Name [f])) [] (J.Ident "apply") [])))] else []
           let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
           (s3, nje3) <- getS3 t j3 genApply id cvarass

           return (s1 ++ s2 ++ s3, nje3, scope2ctyp t) -- need to check t1 == t2           

     otherwise -> translateM super e,

  translateScopeM = \e m -> case e of
      Typ t g ->
        do  n <- get
            let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
            let (v,n')  = maybe (n+1,n+2) (\(i,_) -> (i,n+1)) m -- decide whether we have found the fixpoint closure or not
            put (n' + 1)
            let self = J.Ident (localvarstr ++ show v)
            let nextInClosure = g (n',t)
            (initVars :: InitVars) <- ask
            let js = (initStuff localvarstr n' (inputFieldAccess (localvarstr ++ show v)) (javaType t)) 
            (cvar,t1) <- case last nextInClosure of -- last?
                         True -> do   (s,je,t1) <- local (\(_ :: InitVars) -> []) $ translateScopeM (up this) nextInClosure Nothing
                                      return (standardTranslation je s (v, t) n' n (js:initVars) True,t1)
                         False -> do  (s,je,t1) <- local (\_ -> js:initVars) $ translateScopeM (up this) nextInClosure Nothing
                                      return (refactoredScopeTranslationBit je s (v, t) n' n,t1)
            return (cvar, J.ExpName (J.Name [f]), Typ t (\_ -> t1) )

      otherwise -> translateScopeM super e m,
             
     createWrap = createWrap super,
    
     closureClass = J.ClassTypeDecl (J.ClassDecl [J.Abstract] (J.Ident "Closure") [] Nothing [] (
                    J.ClassBody [field localvarstr,field "out",app [J.Abstract] Nothing Nothing "apply" [] ,app [J.Public,J.Abstract] Nothing (Just closureType) "clone" []]))
  }}

refactoredScopeTranslationBit javaExpression statementsBeforeOA (currentId,currentTyp) freshVar nextId = completeClosure
    where
        currentInitialDeclaration = J.MemberDecl $ J.FieldDecl [] closureType [J.VarDecl (J.VarId $ J.Ident $ localvarstr ++ show currentId) (Just (J.InitExp J.This))]
        completeClosure = [(J.LocalClass (J.ClassDecl [] (J.Ident ("Fun" ++ show nextId)) []
                                 (Just $ J.ClassRefType (J.ClassType [(J.Ident "Closure",[])])) [] (jexp [currentInitialDeclaration, J.InitDecl False (J.Block $ (statementsBeforeOA ++ [outputAssignment javaExpression]))] (Just $ J.Block [])  nextId True))),
                                        J.LocalVars [] (closureType) ([J.VarDecl (J.VarId $ J.Ident (localvarstr ++ show nextId)) (Just (J.InitExp (instCreat nextId)))])] 
        

-- applyCallI = J.InitDecl False $ J.Block [applyCall]
