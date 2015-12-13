{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{- |
Module      :  TypeCheck
Description :  Type checker for the source language.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>, Weixin Zhang <zhangweixinxd@gmail.com>
Stability   :  experimental
Portability :  portable
-}

{- We make typechecking (this file) happen before desugaring (Desugar.hs) so that
the error messages presented to the programmer can be clearer. However, an
undesired consequence of such approach for the compiler implementer is that the
implementation of the typing rules does not follow strictly the formalization.
For instance, in the formalization there is no rule for handling multi-field
records as they are desugared into intersections of single-field records first.
But here we have to handle such cases.-}

module TypeCheck
  ( checkExpr

  -- For REPL
  , checkExprWithEnv
  , mkInitCheckerStateWithEnv
  , TypeError
  ) where

import Checker
import IOEnv
import JavaUtils
import qualified JvmTypeQuery
import Panic
import PrettyUtils
import Src hiding (subtype)
import qualified Src (subtype)
import SrcLoc
import StringUtils
import TypeErrors

import Control.Monad.Except
import Data.Data (toConstr)
import Data.List (findIndex, intercalate)
import Data.List.Split
import qualified Data.Map  as Map
import Data.Maybe (fromMaybe, isJust, fromJust)
import qualified Data.Set  as Set
import Text.PrettyPrint.ANSI.Leijen

import Prelude hiding (pred, (<$>))

checkExpr :: ParsedExpr -> IO (Either (Located TypeError) (Type, CheckedExpr))
checkExpr e = JvmTypeQuery.withConnection
                  (\conn -> (evalIOEnv (mkInitCheckerState conn) . runExceptT . inferExpr) e)

-- Temporary hack for REPL
checkExprWithEnv :: ValueContext -> ParsedExpr -> IO (Either (Located TypeError) (Type, CheckedExpr))
checkExprWithEnv value_ctxt e = JvmTypeQuery.withConnection (\conn ->
  (evalIOEnv (mkInitCheckerStateWithEnv value_ctxt conn) . runExceptT . inferExpr) e)


disjoint :: TypeContext -> Type -> Type -> Bool
disjoint ctxt (TVar a) t
  = case lookupTVarConstraint ctxt a of
       Nothing -> False
       Just c  -> Src.subtype ctxt c t && Src.subtype ctxt t c
  -- = case lookupTVar ctxt a of
  --     Nothing -> invariantFailed "Disjoint.disjoint:TVar" $ text "Not in scope:" <+> code (text a)
  --     Just result ->
  --       case result of
  --         IsTypeVar UnConstrained    -> False
  --         IsTypeVar (DisjointWith s) -> subtype s t
  --         -- Disjointness check shouldn't be aware of the type synonym feature.
  --         IsTypeAlias _  ->
  --           invariantFailed "Disjoint.disjoint:TVar" $
  --             text "Type alias" <+> code (text a) <+> text "should already have been eliminated"
disjoint ctxt t (TVar a) = disjoint ctxt (TVar a) t
disjoint ctxt (Fun _ s) (Fun _ t) = disjoint ctxt s t
disjoint ctxt (Forall v1 b1) (Forall v2 b2) = True -- TODO
disjoint ctxt (And s t) u         = disjoint ctxt s u && disjoint ctxt t u
disjoint ctxt s         (And t u) = disjoint ctxt s t && disjoint ctxt s u
disjoint ctxt Unit Unit = True -- Special case, since `Unit` contains only one value.
disjoint ctxt (TupleType types1) (TupleType types2)
  = length types1 /= length types2
  || any (uncurry (disjoint ctxt)) (zip types1 types2)
disjoint ctxt (RecordType fields1) (RecordType fields2)
  = Set.null (labels1 `Set.intersection` labels2)
  where
    labels1 = Set.fromList (map fst fields1)
    labels2 = Set.fromList (map fst fields2)
disjoint ctxt (JClass c1) (JClass c2) = c1 /= c2
disjoint ctxt (Datatype n1 _ _) (Datatype n2 _ _) = n1 /= n2
disjoint ctxt s         t
  | toConstr s /= toConstr t = True
  | otherwise                = False -- FIXME


-- | Kinding.
kind :: TypeContext -> Type -> IO (Maybe Kind)
kind d (TVar a)     = case Map.lookup a d of Nothing        -> return Nothing
                                             Just (k, _, _) -> return (Just k)
kind _  Unit        = return (Just Star)
kind d (Fun t1 t2)  = justStarIffAllHaveKindStar d [t1, t2]
kind d (Forall (a,_) t) = kind (addTVarToContext [(a, Star)] d) t
kind d (TupleType ts)  = justStarIffAllHaveKindStar d ts
kind d (RecordType fs) = justStarIffAllHaveKindStar d (map snd fs)
kind d (And t1 t2)  = justStarIffAllHaveKindStar d [t1, t2]

{-
    Δ,x::* ⊢ t :: k
    -------------------- (K-Abs) Restriction compared to F_omega: x can only have kind *
    Δ ⊢ λx. t :: * => k
-}
kind d (OpAbs x t) = do
  maybe_k <- kind (addTVarToContext [(x, Star)] d) t
  case maybe_k of
    Nothing -> return Nothing
    Just k  -> return $ Just (KArrow Star k)

{-
    Δ ⊢ t1 :: k11 => k12  Δ ⊢ t2 :: k11
    ------------------------------------ (K-App)
    Δ ⊢ t1 t2 :: k12
-}
kind d (OpApp t1 t2) = do
  maybe_k1 <- kind d t1
  maybe_k2 <- kind d t2
  case (maybe_k1, maybe_k2) of
    (Just (KArrow k11 k12), Just k2) | k2 == k11 -> return (Just k12)
    _ -> return Nothing

kind _ t = return (Just Star) -- TODO

justStarIffAllHaveKindStar :: TypeContext -> [Type] -> IO (Maybe Kind)
justStarIffAllHaveKindStar d ts
  = do ps <- mapM (hasKindStar d) ts
       if and ps
          then return (Just Star)
          else return Nothing

hasKindStar :: TypeContext -> Type -> IO Bool
hasKindStar d t
  = do k <- kind d t
       return (k == Just Star)

-- | Typing.
inferExpr :: ParsedExpr -> Checker (Type, CheckedExpr)
inferExpr this@(L loc (Var name)) = do
  resVar <- lookupVar name
  case resVar of
    Just t -> return (t, Var (name, t) `withLoc` this)
    Nothing -> throwError (NotInScope name `withLoc` this)

inferExpr (L loc (Lit lit)) = return (srcLitType lit, L loc $ Lit lit)

inferExpr (L loc (Lam (L loc_x1 x1, t1) e))
  = do checkType t1
       d <- getTypeContext
       let t1' = expandType d t1
       (t', e') <- withLocalVars [(x1,t1')] (inferExpr e)
       return (Fun t1' t', L loc $ Lam (L loc_x1 x1, t1') e')

inferExpr (L loc (App e1 e2)) =
  do (t1, e1') <- inferExpr e1
     (t2, e2') <- inferExpr e2

     case e1' of
       L loc (ConstrOut (Constructor n ts) es) ->
           case t1 of
             Fun t11 t12 ->
                 do result <- subtype t2 t11
                    unless result $ throwError (TypeMismatch t11 t2 `withLoc` e2)
                    return (t12, L loc $ ConstrOut (Constructor n (init ts ++ [t11,t12])) (es ++ [e2']))
             _ -> throwError (NotAFunction t1 `withLoc` e1)
       _ ->
        case t1 of
          -- Local type inference:
          -- `f[T] e` can be written as `f e` if the type of e is just T.
          Forall _ _ -> inferExpr (App (TApp e1 t2 `withLoc` e1) e2 `withLoc` e1)

          Fun t11 t12 ->
            do result <- subtype t2 t11
               unless result (throwError (TypeMismatch t11 t2 `withLoc` e2))
               return (t12, L loc (App e1' e2'))

          _ -> throwError (NotAFunction t1 `withLoc` e1)

inferExpr (L loc (BLam (a,c) e)) -- TODO: Issue: #addconstrainttocontext, check constraint
  = do case c of
         Just t  -> checkType t
         Nothing -> return ()
       (t, e') <- withLocalConstrainedTVars [(a, c)] (inferExpr e)
       return (Forall (a,c) t, L loc $ BLam (a,c) e')

inferExpr this@(L loc (TApp e arg))
  = do (t, e') <- inferExpr e
       checkType arg
       d <- getTypeContext
       let arg' = expandType d arg
       case t of
         Forall (a,c) t1 -> do
           case c of
             Nothing -> return ()
             Just t  -> do
               result <- subtype arg t
               when result (throwError (General (text "failed to satisfy disjoint constraint") `withLoc` this))
           let t' = fsubstTT (a, arg') t1 -- TODO: Issue: #addconstrainttocontext
           case e' of
             L loc' (ConstrOut (Constructor n _) es) -> return (t', L loc' $ ConstrOut (Constructor n [t']) es)
             _ -> return (t', L loc $ TApp e' arg')
         _ -> throwError (General (code (pretty e) <+> text "does not take type parameters") `withLoc` this)

inferExpr (L loc (TupleCon es))
  | length es < 2 = panic "TypeCheck.inferExpr: Tuple: fewer than two items"
  | otherwise     = do (ts, es') <- mapAndUnzipM inferExpr es
                       return (TupleType ts, L loc $ TupleCon es')

inferExpr this@(L _ (TupleProj e i))
  = do (t, e') <- inferExpr e
       case t of
         TupleType ts
           | 1 <= i && i <= length ts -> return (ts !! (i - 1), TupleProj e' i `withLoc` this)
           | otherwise -> throwError (General (text "tuple index out of range") `withLoc` this)
         _ -> throwError (General (text "this is not of a product type") `withLoc` e)

inferExpr (L loc (PrimOp e1 op e2)) =
  case op of
    Arith _ ->
      do (_, e1') <- inferAgainst e1 (JClass "java.lang.Integer")
         (_, e2') <- inferAgainst e2 (JClass "java.lang.Integer")
         return (JClass "java.lang.Integer", L loc $ PrimOp e1' op e2')
    Compare _ ->
      do (t1, e1') <- inferExpr e1
         (_ , e2') <- inferAgainst e2 t1
         return (JClass "java.lang.Boolean", L loc $ PrimOp e1' op e2')
    Logic _ ->
      do (_, e1') <- inferAgainst e1 (JClass "java.lang.Boolean")
         (_, e2') <- inferAgainst e2 (JClass "java.lang.Boolean")
         return (JClass "java.lang.Boolean", L loc $ PrimOp e1' op e2')

inferExpr (L loc (If e1 e2 e3))
  = do (_, e1')  <- inferAgainst e1 (JClass "java.lang.Boolean")
       (t2, e2') <- inferExpr e2
       (t3, e3') <- inferExpr e3
       d <- getTypeContext
       return (fromMaybe (panic message) (leastUpperBound d t2 t3), L loc $ If e1' e2' e3')
  where
    message = "inferExpr: least upper bound of types of two branches does not exist"

inferExpr (L loc (LetIn rec_flag binds e)) =
  do checkDupNames (map bindName binds)
     binds' <- case rec_flag of
                 NonRec -> mapM normalizeBind binds
                 Rec    -> do sigs <- collectBindIdSigs binds
                              withLocalVars sigs (mapM normalizeBind binds)
     (t, e') <- withLocalVars (map (\ (f,t,_) -> (f,t)) binds') (inferExpr e)
     return (t, L loc $ LetOut rec_flag binds' e')

inferExpr (L _ LetOut{}) = panic "TypeCheck.inferExpr: LetOut"

--  Case           Possible interpretations
--  ---------------------------------------
--  e.x            Field access, record elim
--  e.x ( )        Method invocation
--  e.x ()         Method invocation, application (of a unit lit)
--  e.x (g y)      Method invocation, application
--  e.x (g y,...)  Method invocation, application (of a tuple)

-- In all the cases of application except the first, it is impossible for `e.x`
-- to be a field access since field accesses cannot return something that
-- accepts a value as its argument (But with Java 8 lambdas? TODO.). So `e.x`
-- can only be a method invocation.

-- e.x
inferExpr this@(L _ (Dot e x Nothing)) =
  do (t, _) <- inferExpr e
     case t of
       JClass _     -> inferExpr (JField (NonStatic e) x undefined `withLoc` this)
       RecordType _ -> inferExpr (RecordProj e x `withLoc` this)
       And _ _      -> inferExpr (RecordProj e x `withLoc` this)
       _            -> throwError (NotMember x t `withLoc` this) -- TODO: should be x's loc

-- e.x ( )
inferExpr this@(L loc (Dot e x (Just ([], UnitImpossible)))) =
  do (t, _) <- inferExpr e
     case t of
       JClass _ -> inferExpr (JMethod (NonStatic e) x [] undefined `withLoc` this)
       _        -> throwError (NotMember x t `withLoc` this) -- TODO: should be x's loc

-- e.x ()
inferExpr this@(L loc (Dot e x (Just ([], UnitPossible)))) =
  do (t, _) <- inferExpr e
     case t of
       JClass _      -> inferExpr (L loc $ JMethod (NonStatic e) x [] undefined)
       RecordType _  -> inferExpr (L loc $ App (L loc $ RecordProj e x) (noLoc $ Lit UnitLit))
       And _ _       -> inferExpr (L loc $ App (L loc $ RecordProj e x) (noLoc $ Lit UnitLit))
       _             -> throwError (NotMember x t `withLoc` this) -- TODO: should be x's loc

-- e.x (a)
inferExpr this@(L loc (Dot e x (Just ([arg], _)))) =
  do (t, _) <- inferExpr e
     case t of
       JClass _     -> inferExpr (L loc $ JMethod (NonStatic e) x [arg] undefined)
       RecordType _ -> inferExpr (L loc $ App (L loc $ RecordProj e x) arg)
       And _ _      -> inferExpr (L loc $ App (L loc $ RecordProj e x) arg)
       _            -> throwError (NotMember x t `withLoc` this) -- TODO: should be x's loc

-- e.x (a,...)
inferExpr this@(L loc (Dot e x (Just (args, _)))) =
  do (t, _) <- inferExpr e
     case t of
       JClass _     -> inferExpr (L loc $ JMethod (NonStatic e) x args undefined)
       RecordType _ -> inferExpr (L loc $ App (L loc $ RecordProj e x) tuple)
       And _ _      -> inferExpr (L loc $ App (L loc $ RecordProj e x) tuple)
       _            -> throwError (NotMember x t `withLoc` this) -- TODO: should be x's loc
    where tuple = TupleCon args `withLocs` args

-- JNew, JMethod, and JField

inferExpr this@(L loc (JNew c args))
  = do checkClassName c
       (arg_cs, args') <- mapAndUnzipM inferAgainstAnyJClass args
       checkConstruction c arg_cs
       return (JClass c, L loc (JNew c args'))

inferExpr (L loc (JMethod receiver m args _)) =
  case receiver of
    Static c ->
      do (arg_cs, args') <- mapAndUnzipM inferAgainstAnyJClass args
         ret_c <- checkMethodCall (Static c) m arg_cs
         let ret_type = case ret_c of "java.lang.Void" -> Unit
                                      -- "char" -> JType (JPrim "char")
                                      _ -> JClass ret_c
         return (ret_type, L loc $ JMethod (Static c) m args' ret_c)
    NonStatic e ->
      do (c, e')         <- inferAgainstAnyJClass e
         (arg_cs, args') <- mapAndUnzipM inferAgainstAnyJClass args
         ret_c <- checkMethodCall (NonStatic c) m arg_cs
         let ret_type = case ret_c of "java.lang.Void" -> Unit
                                      -- "char" -> JType (JPrim "char")
                                      _ -> JClass ret_c
         return (ret_type, L loc $ JMethod (NonStatic e') m args' ret_c)

inferExpr this@(L loc (JField receiver f _)) =
  case receiver of
    Static c ->
      do ret_c <- checkFieldAccess (Static c) f
         return (JClass ret_c, L loc $ JField (Static c) f (JClass ret_c))
         -- if ret_c == "char"
         --    then return (JType (JPrim ret_c), JField (Static c) f ret_c)
         --    else return (JType (JClass ret_c), JField (Static c) f ret_c)
    NonStatic e ->
      do (t, e') <- inferExpr e
         case t of
           JClass c ->
             do ret_c   <- checkFieldAccess (NonStatic c) f
                return (JClass ret_c, JField (NonStatic e') f (JClass ret_c) `withLoc` e')
                -- if ret_c == "char"
                --   then return (JType (JPrim "char"), JField (NonStatic e') f ret_c)
                --   else return (JType (JClass ret_c), JField (NonStatic e') f ret_c)
           _ -> throwError (NotMember f t `withLoc` this) -- TODO: should be f's loc

inferExpr (L loc (Seq es)) =
  do (ts, es') <- mapAndUnzipM inferExpr es
     return (last ts, Seq es' `withLocs` es')

inferExpr this@(L loc (Merge e1 e2)) =
 do (t1, e1') <- inferExpr e1
    (t2, e2') <- inferExpr e2
    typeContext <- getTypeContext
    if disjoint typeContext t1 t2
       then return (And t1 t2, Merge e1' e2' `withLoc` e1')
       else throwError (General
                 (text "The merge of two terms of types" <+>
                 code (pretty t1) <+> text "and" <+> code (pretty t2) <+>
                 text "is overlapping") `withLoc` this)

inferExpr (L loc (RecordCon fs)) =
  do checkDupNames (map fst fs)
       -- TODO: This will give imprecise error message: should be "label", not "param".
     (ts, es') <- mapAndUnzipM inferExpr (map snd fs)
     let fs' = zip (map fst fs) ts
     return (foldl (\acc (l,t) -> And acc (RecordType [(l,t)])) (RecordType [head fs']) (tail fs'), L loc $ RecordCon (zip (map fst fs) es'))

inferExpr this@(L _ (RecordProj e l)) =
  do (t, e') <- inferExpr e
     case Map.lookup l (recordFields t) of
       Just t1 -> return (t1, RecordProj e' l `withLoc` this)
       Nothing -> throwError (NotMember l t `withLoc` this) -- TODO: should be l's loc

inferExpr (L loc (RecordUpdate e fs)) =
  do (_, es') <- mapAndUnzipM inferExpr (map snd fs)
     (t, e')  <- inferExpr e
     return (t, L loc $ RecordUpdate e' (zip (map fst fs) es'))

-- Well, I know the desugaring is too early to happen here...
-- inferExpr (L loc (LetModule (Module m binds) e)) =
--   do let fs = map bindName binds
--      let letrec = L loc $ Let Rec binds (L loc $ RecordCon (map (\f -> (f, noLoc $ Var f)) fs))
--      inferExpr (L loc $ Let NonRec [Bind m [] [] letrec Nothing] e)
-- inferExpr (L loc (ModuleAccess m f)) = inferExpr (L loc $ RecordProj (L loc $ Var m) f)

inferExpr this@(L _ (Type typeBind e))
  = do checkDupNames params
       typeContext <- getTypeContext
       maybe_kind <- liftIO $ kind typeContext pulledRight
       case maybe_kind of
         Nothing -> throwError (NotWellKinded pulledRight `withLoc` this) -- FIXME
         Just k  -> withTypeSynonym [(name, pulledRight, k)] $ inferExpr e
  where
    pulledRight = pullRight params rhs

    params = typeBindParams typeBind
    name   = typeBindName typeBind
    rhs    = typeBindRhs typeBind

inferExpr (L loc (Error ty str)) = do
       d <- getTypeContext
       let ty' = expandType d ty
       (_, e) <- inferAgainst str (JClass "java.lang.String")
       return (ty', L loc $ Error ty' e)

-- data List A = Nil | Cons A (List A) and ...; e
-- gamma |- Nil: \/A. List A, Cons: \/A. A List A
-- delta |- List: \A. List A
inferExpr (L loc (Data recflag databinds e)) =
    do checkDupNames [ name | DataBind name _ _ <- databinds]
       mapM_ checkDupNames [ name:params | DataBind name params _ <- databinds]
       mapM_ checkDupNames [ map constrName cs | DataBind _ _ cs <- databinds]
       case recflag of
           -- datatype are new type synonym in context
           -- constructors are new var in context
           -- use new context to check expression `e`
           NonRec -> do
               binds <- mapM nonrecDatabind databinds
               let (tvars, vars, databinds') = unzip3 binds
               (t, e') <- withTypeSynonym tvars $ withLocalVars (concat vars) $ inferExpr e
               return (t, L loc $ Data recflag databinds' e')
           Rec -> do
               let tvars = map getDatatype databinds
               withTypeSynonym tvars $ do
                   (vars, databinds') <- mapAndUnzipM recDatabind databinds
                   (t ,e') <- withLocalVars (concat vars) (inferExpr e)
                   return (t, L loc $ Data recflag databinds' e')
    where nonrecDatabind bind@(DataBind name params _) =
               do let tvars = getDatatype bind
                  (cons, vars) <- withTypeSynonym [tvars] $ withLocalTVars (zip params (repeat Star)) $ getConstrbinds bind
                  return (tvars, vars, DataBind name params cons)
          recDatabind bind@(DataBind name params _) =
               do (cons, vars)<- withLocalTVars (zip params (repeat Star)) $ getConstrbinds bind
                  return (vars, DataBind name params cons)

          getDatatype (DataBind name params cs) = (name, ty, kind)
               where ty = pullRight params (Datatype name (map TVar params) (map constrName cs))
                     kind = foldr (\_ acc -> KArrow Star acc) Star params
          getConstrbinds (DataBind name params cs) =
               do type_ctxt <- getTypeContext
                  let dt = Datatype name (map TVar params) (map constrName cs)
                      vars = [(nam, pullRightForall params $ wrap Fun [expandType type_ctxt t | t <- ts] dt) | Constructor nam ts <- cs]
                      cons = [Constructor nam (map (expandType type_ctxt) ts ++ [dt]) | (Constructor nam ts) <- cs]
                  return (cons, vars)

inferExpr this@(L loc (ConstrIn name)) =
    do result <- lookupVar name
       case result of
         Just t  -> return (t, L loc $ ConstrOut (Constructor name [t]) []) -- the last type will always be the real type
         Nothing -> throwError (NotInScope name `withLoc` this)

inferExpr this@(L loc (PolyList es)) =
 do (ts, _) <- mapAndUnzipM inferExpr es
    d <- getTypeContext
    let t = head ts
    case findIndex (not . compatible d t) ts of
        Just i -> throwError (TypeMismatch t (ts !! i) `withLoc` this) -- FIXME
        Nothing -> let es' = map (L loc . App (L loc $ TApp (L loc . ConstrIn $ "Cons") t)) es
                       desugares = foldr (\e acc -> L loc $ App e acc) (L loc $ TApp (L loc $ ConstrIn "Nil") t) es'
                   in inferExpr desugares

inferExpr expr@(L loc (Case e alts)) =
 do
   (t, e') <- inferExpr e
   if not (isDatatype t)
    then if (== JClass "java.lang.String") t
           then inferString
           else throwError (TypeMismatch t (Datatype "Datatype" [] []) `withLoc` expr)
    else do let (pats, exps) = unzip [(pat, exp) | ConstrAlt pat exp <- alts]
            -- check patterns
            pats' <- mapM (checkPattern t) pats

            -- check each branch: no duplicate names; check expr
            let alts' = zipWith (\(ConstrAlt _ exp') p -> ConstrAlt p exp') alts pats'
            (ts, es) <- mapAndUnzipM checkBranch alts'

            -- result type check: case branches have same type
            let resType = head ts
            type_ctxt <- getTypeContext
            let i = findIndex (not . compatible type_ctxt resType) ts
            when (isJust i) $ throwError (TypeMismatch resType (ts !! fromJust i) `withLoc` expr)

            -- pattern exhaustive test
            let patmatrix = map (: []) pats'
            value_ctxt <- getValueContext
            let exhaust = exhaustiveTest value_ctxt patmatrix 1
            unless (null exhaust) $ throwError $
               General (text "patterns are not exhausive, missing patterns:" <$> vcat (map (hcat . map pretty) exhaust))
               `withLoc` expr

            -- pattern overlap test
            let overlap = map (pats' !! ) . filter (\i -> not $ usefulClause (take i patmatrix) (patmatrix !! i)) $ [1.. length patmatrix -1]
            unless (null overlap) $
              throwError $ General (text "patterns are overlapped:"<$> vcat (map pretty overlap))
               `withLoc` expr

            return (resType, L loc $ Case e' (zipWith (\(ConstrAlt c _) -> ConstrAlt c) alts' es))

  where isDatatype (Datatype{}) = True
        isDatatype _ = False

        checkBranch (ConstrAlt pat e2) = do
            let getLocalVars PWildcard  = []
                getLocalVars (PVar nam ty) = [(nam, ty)]
                getLocalVars (PConstr _ pats) = concatMap getLocalVars pats
            let newvars = getLocalVars pat
            checkDupNames . fst . unzip $ newvars
            withLocalVars newvars (inferExpr e2)

        checkPattern ty (PVar nam _) = return (PVar nam ty)
        checkPattern _  PWildcard    = return PWildcard
        checkPattern ty pctr@(PConstr (Constructor nam _) pats) = do
            -- datatype; constructor
            unless (isDatatype ty) (throwError (TypeMismatch ty (Datatype "Datatype" [] []) `withLoc` expr))
            result <- lookupVar nam
            unless (isJust result) (throwError (NotInScope nam `withLoc` expr))
            -- type check: ty is the expected type
            let constr = fromJust result
                Datatype _ feed _ = ty
                ts = unwrapFun (feedToForall constr feed)
            type_ctxt <- getTypeContext
            unless (compatible type_ctxt (last ts) ty) (throwError (TypeMismatch constr ty `withLoc` expr))
            -- constructor arguments
            let error_msg =  text "Constructor" <+> bquotes (text nam) <+> text "should have" <+> int (length ts -1) <+>
                        text "arguments, but has been given" <+> int (length pats) <+> text "in pattern" <+> pretty pctr
            unless (length ts - 1 == length pats) (throwError (General error_msg `withLoc` expr))
            subpat <- zipWithM checkPattern ts pats
            return (PConstr (Constructor nam ts) subpat)

        -- temporary hack: this is to deal with pattern match on String.
        -- inferString :: ParsedExpr -> Checker (Type, CheckedExpr)
        inferString =
          do
            (_, e') <- inferExpr e
            let empt = [ b1' | ConstrAlt (PConstr (Constructor "empty" _) []) b1' <-  alts]
            let cons = [ (sub1,sub2,b2') | ConstrAlt (PConstr (Constructor "cons" _) [sub1, sub2]) b2' <- alts]
            unless (length alts == 2 && length empt == 1 && length cons == 1) $
              throwError (L NoLoc (General (text "String should have two patterns [] and head:tail"))) -- FIXME

            let [b1]               = empt
                [(sub1, sub2, b2)] = cons
            unless (isWildcardOrVar sub1 && isWildcardOrVar sub2) $
              throwError (L NoLoc (General $ text "String should have two patterns [] and head:tail")) -- FIXME

            let localvar  = case sub1 of PVar nam _ -> [(nam, JClass "java.lang.Character")]
                                         _          -> []
                localvar' = case sub2 of PVar nam _ -> (nam, JClass "java.lang.String"):localvar
                                         _          -> []
            (t1, emptyexpr) <- inferExpr b1
            (_,  nonemptyexpr) <- withLocalVars localvar' $ inferAgainst b2 t1
            let emptyalt = ConstrAlt (PConstr (Constructor "empty" []) []) emptyexpr
                nonemptyalt = ConstrAlt (PConstr (Constructor "cons" []) [sub1, sub2]) nonemptyexpr
            return (t1, CaseString e' [emptyalt, nonemptyalt] `withLoc` e)

-- | "Pull" the type params at the LHS of the equal sign to the right.
-- A (high-level) example:
--   A B t  ->  \A. \B. t
-- Another concrete example:
--   ghci> pullRight ["A", "B"] (JClass "java.lang.Integer")
--   OpAbs "A" (OpAbs "B" (JClass "java.lang.Integer"))
pullRight :: [Name] -> Type -> Type
pullRight params t = foldr OpAbs t params

pullRightForall :: [Name] -> Type -> Type
pullRightForall params t = foldr (\a -> Forall (a,Nothing)) t params

inferAgainst :: ParsedExpr -> Type -> Checker (Type, CheckedExpr)
inferAgainst expr expected_ty
  = do (found_ty, expr') <- inferExpr expr
       d <- getTypeContext
       if compatible d found_ty expected_ty
          then return (found_ty, expr')
          else throwError (TypeMismatch expected_ty found_ty `withLoc` expr)

inferAgainstAnyJClass :: ParsedExpr -> Checker (ClassName, CheckedExpr)
inferAgainstAnyJClass expr
  = do (ty, expr') <- inferExpr expr
       case ty of
        -- JType (JPrim "char") -> return ("java.lang.Character", expr')
        JClass c -> return (c, expr')
        _ -> throwError (L NoLoc (TypeMismatch ty (JClass "<any Java class>"))) -- FIXME

-- | Check "f [A1,...,An] (x1:t1) ... (xn:tn): t = e"
normalizeBind :: ReadBind -> Checker CheckedBind
normalizeBind bind
  = do bind' <- checkBindLHS bind
       (bindRhsTy, bindRhs') <- withLocalConstrainedTVars (bindTyParams bind') $ -- TODO: Issue: #addconstrainttocontext
                                  do expandedBindArgs <- mapM (\(L _ x,t) -> do { d <- getTypeContext; return (x,expandType d t) }) (bindParams bind')
                                     withLocalVars expandedBindArgs (inferExpr (bindRhs bind'))
       case bindRhsTyAscription bind' of
         Nothing -> return ( bindName bind'
                           , wrap Forall (bindTyParams bind') (wrap Fun (map snd (bindParams bind')) bindRhsTy)
                           , wrap (\x acc -> BLam x acc `withLoc` acc) (bindTyParams bind') (wrap (\x acc -> Lam x acc `withLoc` acc) (bindParams bind') bindRhs'))
         Just ty_ascription ->
           withLocalConstrainedTVars (bindTyParams bind') $ -- TODO: Issue: #addconstrainttocontext
             do checkType ty_ascription
                d <- getTypeContext
                let ty_ascription' = expandType d ty_ascription
                if compatible d ty_ascription' bindRhsTy
                   then return (bindName bind'
                               , wrap Forall (bindTyParams bind') (wrap Fun (map snd (bindParams bind')) bindRhsTy)
                               , wrap (\x acc -> BLam x acc `withLoc` acc) (bindTyParams bind') (wrap (\x acc -> Lam x acc `withLoc` acc) (bindParams bind') bindRhs'))
                   else throwError (L NoLoc (TypeMismatch (expandType d ty_ascription') bindRhsTy)) -- FIXME

-- | Check the LHS to the "=" sign of a bind, i.e., "f A1 ... An (x1:t1) ... (xn:tn)".
-- First make sure the names of type params and those of value params are distinct, respectively.
-- Then check and expand the types of value params.
checkBindLHS :: ReadBind -> Checker ReadBind
checkBindLHS Bind{..}
  = do checkDupNames (map fst bindTyParams)
       checkDupNames (map (unLoc . fst) bindParams)
       bindParams' <- withLocalConstrainedTVars bindTyParams $ -- TODO: Issue: #addconstrainttocontext
                    -- Restriction: type params have kind *
                    do d <- getTypeContext
                       forM bindParams (\(x,t) ->
                         do checkType t
                            return (x, expandType d t))
       return Bind { bindParams = bindParams', .. }

collectBindIdSigs :: [ReadBind] -> Checker [(Name, Type)]
collectBindIdSigs
  = mapM (\ Bind{..} ->
            case bindRhsTyAscription of
              Nothing    -> throwError (L NoLoc (MissingTyAscription bindName)) -- FIXME
              Just tyAscription ->
                do d <- getTypeContext
                   let d' = foldr (\(a,_) acc -> addTVarToContext [(a, Star)] acc) d bindTyParams -- TODO: Issue: #addconstrainttocontext
                   return (bindName,
                           wrap Forall bindTyParams $
                           wrap Fun [expandType d' ty |  (_,ty) <- bindParams] $
                           expandType d' tyAscription))

-- | Normalize later bindings with typing contexts augmented with
-- previous bindings
normalizeBindAndAccum :: [ReadModuleBind] -> Checker [Definition]
normalizeBindAndAccum = normalize []
  where
    normalize :: [Definition]
              -> [ReadModuleBind]
              -> Checker [Definition]
    normalize binds [] = return (reverse binds)
    normalize binds (b:bs) = do
      let env = concatMap getEnv binds
      binds' <- case b of
                  BindNonRec b -> withLocalVars env (fmap Def (normalizeBind b))
                  BindRec bb -> do
                    sigs <- collectBindIdSigs bb
                    withLocalVars (env ++ sigs) (fmap DefRec (mapM normalizeBind bb))
      normalize (binds' : binds) bs
    getEnv :: Definition -> [(Name, Type)]
    getEnv (Def (a, b, c)) = [(a, b)]
    getEnv (DefRec bs) = map (\(a, b, c) -> (a, b)) bs

-- | Check that a type has kind *.
checkType :: Type -> Checker ()
checkType t =
  case t of
    And t1 t2 -> do
      checkType t1
      checkType t2
      ctxt <- getTypeContext
      unless (disjoint ctxt t1 t2) $
        throwError (L NoLoc (General (text "intersection of types is not disjoint"))) -- FIXME
    JClass c -> checkClassName c
    _ -> do
      delta <- getTypeContext
      maybe_kind <- liftIO $ kind delta t
      case maybe_kind of
        Nothing   -> throwError (L NoLoc (NotWellKinded t)) -- FIXME
        Just Star -> return ()
        Just k    -> throwError (L NoLoc (KindMismatch Star k t)) -- FIXME

unlessIO :: (Monad m, MonadIO m) => IO Bool -> m () -> m ()
unlessIO test do_this
  = do ok <- liftIO test
       unless ok do_this

-- Client library of typeserver API

checkClassName :: ClassName -> Checker ()
checkClassName c
  = do memoized_java_classes <- getMemoizedJavaClasses
       unless (c `Set.member` memoized_java_classes) $
         do conn <- getTypeServer
            res <- liftIO (JvmTypeQuery.definedClass conn c)
            if res
               then memoizeJavaClass c
               else throwError (L NoLoc (NoSuchClass c)) -- FIXME

checkConstruction :: ClassName -> [ClassName] -> Checker ()
checkConstruction c args
  = do conn <- getTypeServer
       unlessIO (JvmTypeQuery.definedConstructor conn c args) $
         throwError (L NoLoc (NoSuchConstructor c args)) -- FIXME

checkMethodCall :: JReceiver ClassName -> MethodName -> [ClassName] -> Checker ClassName
checkMethodCall receiver m args
  = do conn <- getTypeServer
       res <- liftIO (JvmTypeQuery.findMethodReturnType conn c (is_static, m) args)
       case res of
         Nothing           -> throwError (L NoLoc (NoSuchMethod receiver m args)) -- FIXME: Should be the location of the method name
         Just return_class -> return return_class
    where
       (is_static, c) = unwrapJReceiver receiver

checkFieldAccess :: JReceiver ClassName -> FieldName -> Checker ClassName
checkFieldAccess receiver f
  = do conn <- getTypeServer
       res <- liftIO (JvmTypeQuery.findFieldType conn c (is_static, f))
       case res of
         Nothing           -> throwError (L NoLoc (NoSuchField receiver f)) -- FIXME: Should be the location of the field name
         Just return_class -> return return_class
    where
       (is_static, c) = unwrapJReceiver receiver

checkModuleFunction :: PoorMensImport ModuleName -> Checker [(Name, ModuleMapInfo)]
checkModuleFunction (PoorMensImport m) =
  do
    typeserver <- getTypeServer
    res <- liftIO (JvmTypeQuery.extractModuleInfo typeserver (pName, moduleName))
    case res of
      Nothing  -> throwError (L NoLoc (ImportFail m)) -- FIXME: Should be the location of the module path
      Just ret -> return (map flatInfo (fst ret))

  where
    (packageName, moduleName) =
      let w = splitOn "." m
      in (intercalate "." (init w), last w)
    pName = if null packageName then Nothing else Just packageName
    flatInfo (JvmTypeQuery.ModuleInfo f g t) = (f, (pName, t, g, capitalize moduleName))

unwrapJReceiver :: JReceiver ClassName -> (Bool, ClassName)
unwrapJReceiver (NonStatic c) = (False, c)
unwrapJReceiver (Static    c) = (True, c)

srcLitType :: Lit -> Type
srcLitType (Int _)    = JClass "java.lang.Integer"
srcLitType (String _) = JClass "java.lang.String"
srcLitType (Bool _)   = JClass "java.lang.Boolean"
srcLitType (Char _)   = JClass "java.lang.Character"
srcLitType UnitLit    = Unit

checkDupNames :: [Name] -> Checker ()
checkDupNames names
  = case findOneDup names of
      Nothing   -> return ()
      Just name -> throwError (L NoLoc (DuplicateParam name))
                  -- FIXME: Should be the locations of the conflicting names

-- | Find one instance of duplicate in a list.
findOneDup :: Ord a => [a] -> Maybe a
findOneDup xs = go xs Set.empty
  where
    go []      _ = Nothing
    go (x:xs') s = if Set.member x s
                     then Just x
                     else go xs' (Set.insert x s)

unwrapFun :: Type -> [Type]
unwrapFun (Fun t t') = t : unwrapFun t'
unwrapFun t = [t]

feedToForall :: Type -> [Type] -> Type
feedToForall =
    foldl (\t t_feed -> case t of
                          Forall (a,_) t' -> fsubstTT (a, t_feed) t'
                          _ -> prettySorry "TypeCheck.feedToForall" (pretty t))

---- find the arity of a constructor
ctrArity :: Map.Map Name Type -> Name -> Int
ctrArity value_ctxt name =
    length (removeForall t) - 1
    where Just t =  lookupVarType name value_ctxt
          removeForall (Forall _ b) = removeForall b
          removeForall x            = unwrapFun x

-- Useful clause detect
-- whether clause q is useful with respect to matrix P
usefulClause :: [[Pattern]] -> [Pattern] -> Bool

---- base case
usefulClause [] _ = True
usefulClause _ [] = False

---- q begins with a constructor c
---- U (P, q) = U (S(c,P), S(c,q))
usefulClause pats clause@(PConstr ctr _:_) =
    usefulClause (specializedMatrix ctr pats)
                 (head (specializedMatrix ctr [clause]))

---- q begins with a wildcard(or a variable)
usefulClause pats clause =
    -- P has some missing constructor
    -- U (P, _:rest) = U (D(p), rest))
    if all isWildcardOrVar patshead || missingconstrs /= []
    then usefulClause (defaultMatrix pats) (tail clause)
    -- P has all constructors appeared
    -- U (P, q) = whether exists a constructor c that U (S(c,P), S(c,q)) is true
    else any (\ctr -> usefulClause (specializedMatrix ctr pats)
                                   (head (specializedMatrix ctr [clause])))
             appearconstrs
    where patshead = map head pats
          (appearconstrs, missingconstrs) = constrInfo patshead

-- Test a Pattern matrix is exhaustive
exhaustiveTest :: Map.Map Name Type -> [[Pattern]] -> Int -> [[Pattern]]

---- basic cases
exhaustiveTest _ [] 0 = [[]]
exhaustiveTest _ _  0 = []

exhaustiveTest value_ctxt pats n
    -- Simga is the set of appearing constructors
    -- Sigma is empty
    -- if I(D(p), n-1) returns (p2,...,pn)
    -- I(P, n) = (_, p2,...,pn)
    | all isWildcardOrVar patshead = map (PWildcard :) $ exhaustiveTest value_ctxt (map tail pats) (n-1)
    | null missingconstrs          = exhaustivityForCtr
    | otherwise                    = exhaustivityForCtr ++ exhaustivityForWildcard
    -- if I(S(c, P), a+n-1) returns (r1,...,ra, p2,...,pn)
    -- I(P, n) = (c(r1,...ra), p2,...,pn)
    where patshead = map head pats
          (appearconstrs, missingconstrs) = constrInfo patshead
          exhaustivityForCtr =
                 [ PConstr ctr component: eachres' |
                         ctr <- appearconstrs,
                         let arity = length (constrParams ctr) - 1,
                         eachres <- exhaustiveTest value_ctxt (specializedMatrix ctr pats) (arity + n - 1),
                         let (component, eachres') = splitAt arity eachres ]
    -- Sigma is incomplete
    -- if I(D(P), n-1) returns (p2,...,pn)
    -- I(P, n) = (c(_,...,_), p2,...,pn) for each missing constructor c
          exhaustivityForWildcard =
                 [ cur: eachres | eachres <- exhaustiveTest value_ctxt (defaultMatrix pats) (n-1),
                                  name <- missingconstrs,
                                  let arity = ctrArity value_ctxt name,
                                  let cur = PConstr (Constructor name []) $ replicate arity PWildcard ]
