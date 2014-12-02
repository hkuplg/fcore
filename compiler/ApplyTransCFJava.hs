{-# OPTIONS -XRankNTypes -XFlexibleInstances -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XConstraintKinds -XScopedTypeVariables #-}

module ApplyTransCFJava where

import qualified Data.Set as Set
import qualified Language.Java.Syntax as J

import           BaseTransCFJava
import           ClosureF
import           Inheritance
import           JavaEDSL
import           MonadLib
import           StringPrefixes

data ApplyOptTranslate m = NT {toT :: Translate m}

instance (:<) (ApplyOptTranslate m) (Translate m) where
   up              = up . toT

instance (:<) (ApplyOptTranslate m) (ApplyOptTranslate m) where
   up              = id

isMultiBinder :: EScope Int (Var, Type Int) -> Bool
isMultiBinder (Type _ _) = True
isMultiBinder (Kind f)   = isMultiBinder (f 0)
isMultiBinder (Body _)   = False

-- main translation function
transApply :: (MonadState Int m,
               MonadState (Set.Set J.Name) m,
               MonadReader InitVars m,
               selfType :< ApplyOptTranslate m,
               selfType :< Translate m)
              => Mixin selfType (Translate m) (ApplyOptTranslate m)
transApply this super = NT {toT = super {
  translateScopeTyp = \x1 f initVars nextInClosure m closureClass ->
    case isMultiBinder nextInClosure of
         False -> do (initVars' :: InitVars) <- ask
                     translateScopeTyp super x1 f (initVars ++ initVars') nextInClosure (local (\(_ :: InitVars) -> []) m) closureClass
         True -> do (s,je,t1) <- local (initVars ++) m
                    let refactored = modifiedScopeTyp (unwrap je) s x1 f closureClass
                    return (refactored,t1),

  genApply = \f t x y z -> do applyGen <- genApply super f t x y z
                              return [bStmt $ J.IfThen (fieldAccess f "hasApply")
                                      (J.StmtBlock (block applyGen)) ],

  genClosureVar = \t j1 typ -> do
    (usedCl :: Set.Set J.Name) <- get
    maybeCloned <- case t of
                    Body _ ->
                      return (J.ExpName j1)
                    _ ->
                      if (Set.member j1 usedCl) then
                        return $ J.MethodInv (J.PrimaryMethodCall (J.ExpName j1) [] (J.Ident "clone") [])
                      else do
                        put (Set.insert j1 usedCl)
                        return (J.ExpName j1)
    f <- getNewVarName (up this)
    case maybeCloned of
     J.MethodInv _ -> return ([localVar typ (varDecl f maybeCloned)], (name [f]))
     _ -> return ([], j1),


  genClone = return True
}}

modifiedScopeTyp :: J.Exp -> [J.BlockStmt] -> Int -> Int -> String -> [J.BlockStmt]
modifiedScopeTyp oexpr ostmts x1 f closureClass = completeClosure
  where closureType' = classTy closureClass
        currentInitialDeclaration = memberDecl $ fieldDecl closureType' (varDecl (localvarstr ++ show x1) J.This)
        setApplyFlag = assignField (fieldAccExp (left $ var (localvarstr ++ show x1)) "hasApply") (J.Lit (J.Boolean False))
        fc = f
        completeClosure = [(localClassDecl ("Fun" ++ show fc) closureClass
                            (closureBodyGen
                             [currentInitialDeclaration, J.InitDecl False (block $ (setApplyFlag : ostmts ++ [assign (name [closureOutput]) oexpr]))]
                             []
                             fc
                             True
                             closureType'))
                          ,localVar closureType' (varDecl (localvarstr ++ show f) (funInstCreate fc))]


-- Alternate version of transApply that works with Stack translation
transAS :: (MonadState Int m, MonadState (Set.Set J.Name) m, MonadReader InitVars m, selfType :< ApplyOptTranslate m, selfType :< Translate m) => Mixin selfType (Translate m) (ApplyOptTranslate m)
transAS this super = NT {toT = (up (transApply this super)) {

  genApply = \f t tempOut outType z ->
    do applyGen <- genApply super f t tempOut outType z
       let tempDecl = localVar outType
                      (varDecl tempOut (case outType of
                                         J.PrimType J.IntT -> J.Lit (J.Int 0)
                                         _ -> (J.Lit J.Null)))
       let elseDecl = assign (name [tempOut]) (cast outType (J.FieldAccess (fieldAccExp (cast z f) closureOutput)))

       if length applyGen == 2
         then return applyGen
         else return [tempDecl, bStmt $ J.IfThenElse (fieldAccess f "hasApply")
                                (J.StmtBlock (block applyGen))
                                (J.StmtBlock (block [elseDecl]))]
  }}
