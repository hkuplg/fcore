{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds #-}

module TypeCheck
  ( typeCheck

  -- For REPL
  , typeCheckWithEnv
  , mkInitTcEnvWithEnv
  , TypeError
  ) where

import Src

import IOEnv
import JavaUtils
import JvmTypeQuery
import Panic
import StringPrefixes


import System.IO
import System.Process

import Control.Monad.Error

import Data.Maybe (fromJust)
import qualified Data.Map  as Map
import qualified Data.Set  as Set

import Prelude hiding (pred)

type Connection = (Handle, Handle)

typeCheck :: Expr Name -> IO (Either TypeError (Expr TcId, Type))
-- type_server is (Handle, Handle)
typeCheck e = withTypeServer (\type_server ->
  (evalIOEnv (mkInitTcEnv type_server) . runErrorT . tcExpr) e)

-- Temporary hack for REPL
typeCheckWithEnv :: ValueContext -> Expr Name -> IO (Either TypeError (Expr TcId, Type))
-- type_server is (Handle, Handle)
typeCheckWithEnv value_ctxt e = withTypeServer (\type_server ->
  (evalIOEnv (mkInitTcEnvWithEnv value_ctxt type_server) . runErrorT . tcExpr) e)

withTypeServer :: (Connection -> IO a) -> IO a
withTypeServer do_this =
  do cp <- getClassPath
     let p = (proc "java" ["-cp", cp, (namespace ++ "TypeServer")])
               { std_in = CreatePipe, std_out = CreatePipe }
     (Just inp, Just out, _, proch) <- createProcess p
     hSetBuffering inp NoBuffering
     hSetBuffering out NoBuffering
     res <- do_this (inp, out)
     terminateProcess proch
     return res

data TcEnv
  = TcEnv
  { tceTypeCtxt     :: TypeContext
  , tceValueCtxt    :: ValueContext
  , tceTypeserver   :: Connection
  , tceMemoizedJavaClasses :: Set.Set ClassName -- Memoized Java class names
  }

mkInitTcEnv :: Connection -> TcEnv
mkInitTcEnv type_server
  = TcEnv
  { tceTypeCtxt     = Set.empty
  , tceValueCtxt    = Map.empty
  , tceTypeserver   = type_server
  , tceMemoizedJavaClasses = Set.empty
  }

-- Temporary hack for REPL
mkInitTcEnvWithEnv :: ValueContext -> Connection -> TcEnv
mkInitTcEnvWithEnv value_ctxt type_server
  = TcEnv
  { tceTypeCtxt     = Set.empty
  , tceValueCtxt    = value_ctxt
  , tceTypeserver   = type_server
  , tceMemoizedJavaClasses = Set.empty
  }

data TypeError
  = General String
  | ConflictingDefinitions Name
  | ExpectJClass
  | IndexTooLarge
  | Mismatch { expectedTy :: Type, actualTy :: Type }
  | ExpectedSubtype
  | MissingRHSAnnot
  | NotInScopeTyVar Name
  | NotInScopeVar   Name
  | ProjectionOfNonProduct

  -- Java-specific type errors
  | NoSuchClass       ClassName
  | NoSuchConstructor ClassName [ClassName]
  | NoSuchMethod      ClassName MethodName Bool [ClassName]
  | NoSuchField       ClassName FieldName  Bool
  deriving (Show)

instance Error TypeError where
  -- strMsg

type TcM a = ErrorT TypeError (IOEnv TcEnv) a

getTcEnv :: TcM TcEnv
getTcEnv = lift getEnv

setTcEnv :: TcEnv -> TcM ()
setTcEnv tc_env = lift $ setEnv tc_env

getTypeCtxt :: TcM TypeContext
getTypeCtxt = liftM tceTypeCtxt getTcEnv

getValueCtxt :: TcM ValueContext
getValueCtxt = liftM tceValueCtxt getTcEnv

getTypeServer :: TcM (Handle, Handle)
getTypeServer = liftM tceTypeserver getTcEnv

getMemoizedJavaClasses :: TcM (Set.Set ClassName)
getMemoizedJavaClasses = liftM tceMemoizedJavaClasses getTcEnv

memoizeJavaClass :: ClassName -> TcM ()
memoizeJavaClass c
  = do TcEnv{..} <- getTcEnv
       memoized_java_classes <- getMemoizedJavaClasses
       setTcEnv TcEnv{ tceMemoizedJavaClasses = c `Set.insert` memoized_java_classes, ..}

withLocalTyVars :: [Name] -> TcM a -> TcM a
withLocalTyVars tyvars do_this
  = do delta <- getTypeCtxt
       let delta' = Set.fromList tyvars `Set.union` delta
       TcEnv {..} <- getTcEnv
       setTcEnv TcEnv { tceTypeCtxt = delta', ..}
       r <- do_this
       TcEnv {..} <- getTcEnv
       setTcEnv TcEnv { tceTypeCtxt = delta, ..}
       return r

withLocalVars :: [(Name, Type)]-> TcM a -> TcM a
withLocalVars vars do_this
  = do gamma <- getValueCtxt
       let gamma' = Map.fromList vars `Map.union` gamma
       TcEnv {..} <- getTcEnv
       setTcEnv TcEnv { tceValueCtxt = gamma', ..}
       r <- do_this
       TcEnv {..} <- getTcEnv
       setTcEnv TcEnv { tceValueCtxt = gamma, ..}
       return r

tcExpr :: Expr Name -> TcM (Expr TcId, Type)
tcExpr (Var name)
  = do value_ctxt <- getValueCtxt
       case Map.lookup name value_ctxt of
         Just t  -> return (Var (name,t), t)
         Nothing -> throwError (NotInScopeVar name)

tcExpr (Lit lit) = return (Lit lit, srcLitType lit)

tcExpr (Lam (x1,t1) e)
  = do checkType t1
       (e', t) <- withLocalVars [(x1,t1)] (tcExpr e)
       return (Lam (x1,t1) e', Fun t1 t)

tcExpr (App e1 e2)
  = do (e1', t1) <- tcExpr e1
       (e2', t2) <- tcExpr e2
       case t1 of
         Fun t3 t4 -> do checkSubtype t2 t3
                         return (App e1' e2', t4)
         _         -> throwError (General (show e1 ++ "::" ++ show t1))

tcExpr (BLam a e)
  = do (e', t) <- withLocalTyVars [a] (tcExpr e)
       return (BLam a e', Forall a t)

tcExpr (TApp e targ)
  = do (e', t) <- tcExpr e
       checkType targ
       case t of
         Forall a t1 -> return (TApp e' targ, substFreeTyVars (a, targ) t1)
         _           -> sorry "Src.TypeCheck.tcExpr: TApp"

tcExpr (Tuple es)
  | length es < 2 = panic "Src.TypeCheck.tcExpr: Tuple: fewer than two items"
  | otherwise     = do (es', ts) <- mapAndUnzipM tcExpr es
                       return (Tuple es', Product ts)

tcExpr (Proj e i)
  = do (e', t) <- tcExpr e
       case t of
         Product ts
           | 1 <= i && i <= length ts -> return (Proj e' i, ts !! (i - 1))
           | otherwise -> throwError IndexTooLarge
         _ -> throwError ProjectionOfNonProduct

tcExpr (PrimOp e1 op e2)
  = case op of
      Arith _ ->
        do (e1', _t1) <- tcExprAgainst e1 (JClass "java.lang.Integer")
           (e2', _t2) <- tcExprAgainst e2 (JClass "java.lang.Integer")
           return (PrimOp e1' op e2', JClass "java.lang.Integer")
      Compare _ ->
        do (e1', t1)  <- tcExpr e1
           (e2', _t2) <- tcExprAgainst e2 t1
           return (PrimOp e1' op e2', JClass "java.lang.Boolean")
      Logic _ ->
        do (e1', _t1) <- tcExprAgainst e1 (JClass "java.lang.Boolean")
           (e2', _t2) <- tcExprAgainst e2 (JClass "java.lang.Boolean")
           return (PrimOp e1' op e2', JClass "java.lang.Boolean")

tcExpr (If pred b1 b2)
  = do (pred', _pred_ty) <- tcExprAgainst pred (JClass "java.lang.Boolean")
       (b1', t1)         <- tcExpr b1
       (b2', _t2)        <- tcExprAgainst b2 t1
       return (If pred' b1' b2', t1)

tcExpr (Let rec_flag binds e) =
  do checkBindNames binds
     binds' <- case rec_flag of
                 NonRec -> mapM tcBind binds
                 Rec    -> do sigs <- collectBindNameSigs binds
                              withLocalVars sigs (mapM tcBind binds)
     (e', t) <- withLocalVars (map (\ (f,t,_e) -> (f,t)) binds') $ tcExpr e
     return (LetOut rec_flag binds' e', t)

tcExpr (LetOut{..}) = panic "Src.TypeCheck.tcExpr: LetOut"

tcExpr (JNewObj c args)
  = do checkClassName c -- ToDo: Needed?
       (args', arg_cs) <- mapAndUnzipM tcExprAgainstAnyJClass args
       checkNew c arg_cs
       return (JNewObj c args', JClass c)

tcExpr (JMethod object m args _)
  = case object of
      Static c ->
        do (args', arg_cs) <- mapAndUnzipM tcExprAgainstAnyJClass args
           ret_c <- checkStaticMethodCall c m arg_cs
           return (JMethod (Static c) m args' ret_c, JClass ret_c)
      NonStatic e ->
        do (e', c)         <- tcExprAgainstAnyJClass e
           (args', arg_cs) <- mapAndUnzipM tcExprAgainstAnyJClass args
           ret_c <- checkMethodCall c m arg_cs
           return (JMethod (NonStatic e') m args' ret_c, JClass ret_c)

tcExpr (JField object f _)
  = case object of
      Static c ->
        do ret_c <- checkStaticFieldAccess c f
           return (JField (Static c) f ret_c, JClass ret_c)
      NonStatic e ->
        do (e', t) <- tcExpr e
           case t of
             RecordTy _ -> tcExpr (RecordAccess e f) -- Then the typechecker realized!
             JClass c   ->
               do ret_c   <- checkFieldAccess c f
                  return (JField (NonStatic e') f ret_c, JClass ret_c)
             _          -> throwError $ General "The thing before dot is neither a record nor a JVM object"

tcExpr (Seq es) = do (es', ts) <- mapAndUnzipM tcExpr es
                     return (Seq es', last ts)

tcExpr (Merge e1 e2) =
  do (e1', t1) <- tcExpr e1
     (e2', t2) <- tcExpr e2
     return (Merge e1' e2', And t1 t2)

tcExpr (PrimList l) =
      do (es, ts) <- mapAndUnzipM tcExpr l
         case ts of [] -> return (PrimList es, (JClass (namespace ++ "FunctionalList")))
                    _  -> if (all (`alphaEquiv` (ts !! 0)) ts)
                            then return (PrimList es, (JClass (namespace ++ "FunctionalList")))
                            else throwError $ General ("Primitive List Type Mismatch" ++ show (PrimList l))

tcExpr (Record fs) =
  do (es', ts) <- mapAndUnzipM tcExpr (map snd fs)
     return (Record (zip (map fst fs) es'), RecordTy (zip (map fst fs) ts))

tcExpr (RecordAccess e l) =
  do (e', t) <- tcExpr e
     return (RecordAccess e' l, fromJust (lookup (Just l) (fields t)))

tcExpr (RecordUpdate e fs) =
  do (es', ts) <- mapAndUnzipM tcExpr (map snd fs)
     (e', t) <- tcExpr e
     return (RecordUpdate e' (zip (map fst fs) es'), t)

-- Well, I know the desugaring is too early to happen here...
tcExpr (LetModule (Module m binds) e) =
  do let fs = map bindId binds
     let letrec = Let Rec binds (Record (map (\f -> (f, Var f)) fs))
     tcExpr $ Let NonRec [Bind m [] [] letrec Nothing] e
tcExpr (ModuleAccess m f) = tcExpr (RecordAccess (Var m) f)

tcExprAgainst :: Expr Name -> Type -> TcM (Expr TcId, Type)
tcExprAgainst expr expected_ty
  = do (expr', actual_ty) <- tcExpr expr
       if actual_ty `alphaEquiv` expected_ty
          then return (expr', actual_ty)
          else throwError (Mismatch expected_ty actual_ty)

tcExprAgainstAnyJClass :: Expr Name -> TcM (Expr TcId, ClassName)
tcExprAgainstAnyJClass expr
  = do (expr', ty) <- tcExpr expr
       case ty of
         JClass c -> return (expr', c)
         _        -> sorry "tcExprAgainstAnyJClass"

tcExprAgainstMaybe :: Expr Name -> Maybe Type -> TcM (Expr TcId, Type)
tcExprAgainstMaybe e Nothing  = tcExpr e
tcExprAgainstMaybe e (Just t) = tcExprAgainst e t

-- f A1 ... An (x1:T1) ... (xn:Tn) = e
tcBind :: Bind Name -> TcM (Name, Type, Expr TcId)
tcBind Bind{..}
  = do checkBindLHS Bind{..}
       (bindRhs', bindRhsTy) <- withLocalTyVars bindTargs
                                  (withLocalVars bindArgs
                                     (tcExpr bindRhs))
       return ( bindId
              , wrap Forall bindTargs (wrap Fun (map snd bindArgs) bindRhsTy)
              , wrap BLam bindTargs (wrap Lam bindArgs bindRhs'))

checkBindLHS :: Bind Name -> TcM ()
checkBindLHS Bind{..}
  = do checkDupNames bindTargs
       checkDupNames [arg_name | (arg_name, _) <- bindArgs]
       withLocalTyVars bindTargs $
         mapM_ (\(_, arg_ty) -> checkType arg_ty) bindArgs

collectBindNameSigs :: [Bind Name] -> TcM [(Name, Type)]
collectBindNameSigs
  = mapM (\ Bind{..} ->
            case bindRhsAnnot of
              Nothing    -> throwError MissingRHSAnnot
              Just rhsTy -> return (bindId,
                                    wrap Forall bindTargs $
                                    wrap Fun [ty |  (_,ty) <- bindArgs]
                                    rhsTy))

checkBindNames :: [Bind Name] -> TcM ()
checkBindNames bnds = checkDupNames (map bindId bnds)

checkSubtype :: Type -> Type -> TcM ()
checkSubtype t1 t2
  | t1 `subtype` t2 = return ()
  | otherwise       = throwError ExpectedSubtype

checkType :: Type -> TcM ()
checkType t
  = case t of
      JClass c -> checkClassName c
      _        ->
        do type_ctxt <- getTypeCtxt
           let free_ty_vars = freeTyVars t `Set.difference` type_ctxt
           unless (Set.null free_ty_vars) $
             throwError (NotInScopeTyVar (head (Set.toList free_ty_vars)))

unlessIO :: (Monad m, MonadIO m) => IO Bool -> m () -> m ()
unlessIO test do_this
  = do ok <- liftIO test
       unless ok do_this

checkClassName :: ClassName -> TcM ()
checkClassName c
  = do memoized_java_classes <- getMemoizedJavaClasses
       unless (c `Set.member` memoized_java_classes) $
         do h  <- getTypeServer
            res <- liftIO (isJvmType h c)
            if res
               then memoizeJavaClass c
               else throwError (NoSuchClass c)

checkNew :: ClassName -> [ClassName] -> TcM ()
checkNew c args
  = do h <- getTypeServer
       unlessIO (hasConstructor h c args) $
         throwError (NoSuchConstructor c args)

checkMethodCall :: ClassName -> MethodName -> [ClassName] -> TcM ClassName
checkMethodCall c m args
  = do h <- getTypeServer
       res <- liftIO $ methodTypeOf h c (m, False) args
       case res of
         Nothing -> throwError (NoSuchMethod c m False args)
         Just ret_c -> return ret_c

checkStaticMethodCall :: ClassName -> MethodName -> [ClassName] -> TcM ClassName
checkStaticMethodCall c m args
  = do h <- getTypeServer
       res <- liftIO $ methodTypeOf h c (m, True) args
       case res of
         Nothing -> throwError (NoSuchMethod c m True args)
         Just ret_c -> return ret_c

checkFieldAccess :: ClassName -> FieldName -> TcM ClassName
checkFieldAccess c f
  = do h <- getTypeServer
       res <- liftIO $ fieldTypeOf h c (f, False)
       case res of
         Nothing -> throwError (NoSuchField c f False)
         Just ret_c -> return ret_c

checkStaticFieldAccess :: ClassName -> FieldName -> TcM ClassName
checkStaticFieldAccess c f
  = do h <- getTypeServer
       res <- liftIO $ fieldTypeOf h c (f, True)
       case res of
         Nothing -> throwError (NoSuchField c f True)
         Just ret_c -> return ret_c

srcLitType :: Lit -> Type
srcLitType (Integer _) = JClass "java.lang.Integer"
srcLitType (String _)  = JClass "java.lang.String"
srcLitType (Boolean _) = JClass "java.lang.Boolean"
srcLitType (Char _)    = JClass "java.lang.Character"
srcLitType Unit        = UnitType

checkDupNames :: [Name] -> TcM ()
checkDupNames names
  = case findDup names of
      Nothing   -> return ()
      Just name -> throwError (ConflictingDefinitions name)

findDup :: Ord a => [a] -> Maybe a
findDup xs = go xs Set.empty
  where
    go []      _ = Nothing
    go (x:xs') s = if Set.member x s
                     then Just x
                     else go xs' (Set.insert x s)
