{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds #-}

module TypeCheck
  ( typeCheck

  -- For REPL
  , typeCheckWithEnv
  , mkInitTcEnvWithEnv
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
  (evalIOEnv (mkInitTcEnv type_server) . runErrorT . infer) e)

-- Temporary hack for REPL
typeCheckWithEnv :: ValueContext -> Expr Name -> IO (Either TypeError (Expr TcId, Type))
-- type_server is (Handle, Handle)
typeCheckWithEnv value_ctxt e = withTypeServer (\type_server ->
  (evalIOEnv (mkInitTcEnvWithEnv value_ctxt type_server) . runErrorT . infer) e)

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
  { tceTypeCtxt     = Map.empty
  , tceValueCtxt    = Map.empty
  , tceTypeserver   = type_server
  , tceMemoizedJavaClasses = Set.empty
  }

-- Temporary hack for REPL
mkInitTcEnvWithEnv :: ValueContext -> Connection -> TcEnv
mkInitTcEnvWithEnv value_ctxt type_server
  = TcEnv
  { tceTypeCtxt     = Map.empty
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
  | NotInScopeTVar Name
  | NotInScopeVar   Name
  | ProjectionOfNonProduct

  -- Java-specific type errors
  | NoSuchClass       ClassName
  | NoSuchConstructor ClassName [ClassName]
  | NoSuchMethod      (JCallee ClassName) MethodName [ClassName]
  | NoSuchField       (JCallee ClassName) FieldName
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

withLocalTVars :: [(Name, Kind)] -> TcM a -> TcM a
withLocalTVars tvars do_this
  = do delta <- getTypeCtxt
       let delta' = Map.fromList tvars `Map.union` delta
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

infer :: Expr Name -> TcM (Expr TcId, Type)
infer (Var name)
  = do value_ctxt <- getValueCtxt
       case Map.lookup name value_ctxt of
         Just t  -> return (Var (name,t), t)
         Nothing -> throwError (NotInScopeVar name)

infer (Lit lit) = return (Lit lit, srcLitType lit)

infer (Lam (x1,t1) e)
  = do checkType t1
       (e', t) <- withLocalVars [(x1,t1)] (infer e)
       return (Lam (x1,t1) e', Fun t1 t)

infer (App e1 e2)
  = do (e1', t1) <- infer e1
       (e2', t2) <- infer e2
       case t1 of
         Fun t3 t4 -> do checkSubtype t2 t3
                         return (App e1' e2', t4)
         _         -> throwError (General (show e1 ++ "::" ++ show t1))

infer (BLam a e)
  = do (e', t) <- withLocalTVars [(a, Star)] (infer e)
       return (BLam a e', Forall a t)

infer (TApp e targ)
  = do (e', t) <- infer e
       checkType targ
       case t of
         Forall a t1 -> return (TApp e' targ, fsubstTT (a, targ) t1)
         _           -> sorry "Src.TypeCheck.infer: TApp"

infer (Tuple es)
  | length es < 2 = panic "Src.TypeCheck.infer: Tuple: fewer than two items"
  | otherwise     = do (es', ts) <- mapAndUnzipM infer es
                       return (Tuple es', Product ts)

infer (Proj e i)
  = do (e', t) <- infer e
       case t of
         Product ts
           | 1 <= i && i <= length ts -> return (Proj e' i, ts !! (i - 1))
           | otherwise -> throwError IndexTooLarge
         _ -> throwError ProjectionOfNonProduct

infer (PrimOp e1 op e2)
  = case op of
      Arith _ ->
        do (e1', _t1) <- inferAgainst e1 (JClass "java.lang.Integer")
           (e2', _t2) <- inferAgainst e2 (JClass "java.lang.Integer")
           return (PrimOp e1' op e2', JClass "java.lang.Integer")
      Compare _ ->
        do (e1', t1)  <- infer e1
           (e2', _t2) <- inferAgainst e2 t1
           return (PrimOp e1' op e2', JClass "java.lang.Boolean")
      Logic _ ->
        do (e1', _t1) <- inferAgainst e1 (JClass "java.lang.Boolean")
           (e2', _t2) <- inferAgainst e2 (JClass "java.lang.Boolean")
           return (PrimOp e1' op e2', JClass "java.lang.Boolean")

infer (If pred b1 b2)
  = do (pred', _pred_ty) <- inferAgainst pred (JClass "java.lang.Boolean")
       (b1', t1)         <- infer b1
       (b2', _t2)        <- inferAgainst b2 t1
       return (If pred' b1' b2', t1)

infer (Let rec_flag binds e) =
  do checkBindNames binds
     binds' <- case rec_flag of
                 NonRec -> mapM inferBind binds
                 Rec    -> do sigs <- collectBindNameSigs binds
                              withLocalVars sigs (mapM inferBind binds)
     (e', t) <- withLocalVars (map (\ (f,t,_e) -> (f,t)) binds') $ infer e
     return (LetOut rec_flag binds' e', t)

infer (LetOut{..}) = panic "Src.TypeCheck.infer: LetOut"

infer (JNew c args)
  = do checkClassName c -- ToDo: Needed?
       (args', arg_cs) <- mapAndUnzipM inferAgainstAnyJClass args
       checkNew c arg_cs
       return (JNew c args', JClass c)

infer (JMethod callee m args _)
  = case callee of
      Static c ->
        do (args', arg_cs) <- mapAndUnzipM inferAgainstAnyJClass args
           ret_c <- checkMethodCall (Static c) m arg_cs
           return (JMethod (Static c) m args' ret_c, JClass ret_c)
      NonStatic e ->
        do (e', c)         <- inferAgainstAnyJClass e
           (args', arg_cs) <- mapAndUnzipM inferAgainstAnyJClass args
           ret_c <- checkMethodCall (NonStatic c) m arg_cs
           return (JMethod (NonStatic e') m args' ret_c, JClass ret_c)

infer (JField callee f _)
  = case callee of
      Static c ->
        do ret_c <- checkFieldAccess (Static c) f
           return (JField (Static c) f ret_c, JClass ret_c)
      NonStatic e ->
        do (e', t) <- infer e
           case t of
             Record _ -> infer (RecordAccess e f) -- Then the typechecker realized!
             JClass c   ->
               do ret_c   <- checkFieldAccess (NonStatic c) f
                  return (JField (NonStatic e') f ret_c, JClass ret_c)
             _          -> throwError (General "The thing before dot is neither a record nor a JVM object")

infer (Seq es) = do
  (es', ts) <- mapAndUnzipM infer es
  return (Seq es', last ts)

infer (Merge e1 e2) =
  do (e1', t1) <- infer e1
     (e2', t2) <- infer e2
     return (Merge e1' e2', And t1 t2)

infer (PrimList l) =
      do (es, ts) <- mapAndUnzipM infer l
         case ts of [] -> return (PrimList es, JClass (namespace ++ "FunctionalList"))
                    _  -> if all (`alphaEq` (ts !! 0)) ts
                            then return (PrimList es, JClass (namespace ++ "FunctionalList"))
                            else throwError $ General ("Primitive List Type Mismatch" ++ show (PrimList l))

infer (RecordLit fs) =
  do (es', ts) <- mapAndUnzipM infer (map snd fs)
     return (RecordLit (zip (map fst fs) es'), Record (zip (map fst fs) ts))

infer (RecordAccess e l) =
  do (e', t) <- infer e
     return (RecordAccess e' l, fromJust (lookup (Just l) (fields t)))

infer (RecordUpdate e fs) =
  do (es', ts) <- mapAndUnzipM infer (map snd fs)
     (e', t) <- infer e
     return (RecordUpdate e' (zip (map fst fs) es'), t)

-- Well, I know the desugaring is too early to happen here...
infer (LetModule (Module m binds) e) =
  do let fs = map bindId binds
     let letrec = Let Rec binds (RecordLit (map (\f -> (f, Var f)) fs))
     infer $ Let NonRec [Bind m [] [] letrec Nothing] e
infer (ModuleAccess m f) = infer (RecordAccess (Var m) f)

inferAgainst :: Expr Name -> Type -> TcM (Expr TcId, Type)
inferAgainst expr expected_ty
  = do (expr', actual_ty) <- infer expr
       if actual_ty `alphaEq` expected_ty
          then return (expr', actual_ty)
          else throwError (Mismatch expected_ty actual_ty)

inferAgainstAnyJClass :: Expr Name -> TcM (Expr TcId, ClassName)
inferAgainstAnyJClass expr
  = do (expr', ty) <- infer expr
       case ty of
         JClass c -> return (expr', c)
         _        -> sorry "inferAgainstAnyJClass"

inferAgainstMaybe :: Expr Name -> Maybe Type -> TcM (Expr TcId, Type)
inferAgainstMaybe e Nothing  = infer e
inferAgainstMaybe e (Just t) = inferAgainst e t

-- f A1 ... An (x1:T1) ... (xn:Tn) = e
inferBind :: Bind Name -> TcM (Name, Type, Expr TcId)
inferBind Bind{..}
  = do checkBindLHS Bind{..}
       (bindRhs', bindRhsTy) <- withLocalTVars (zip bindTargs (repeat Star))
                                  (withLocalVars bindArgs
                                     (infer bindRhs))
       return ( bindId
              , wrap Forall bindTargs (wrap Fun (map snd bindArgs) bindRhsTy)
              , wrap BLam bindTargs (wrap Lam bindArgs bindRhs'))

checkBindLHS :: Bind Name -> TcM ()
checkBindLHS Bind{..}
  = do checkDupNames bindTargs
       checkDupNames [arg_name | (arg_name, _) <- bindArgs]
       withLocalTVars (zip bindTargs (repeat Star)) $
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
  | deThunk t1 `subtype` deThunk t2 = return ()
  | otherwise                       = throwError ExpectedSubtype

checkType :: Type -> TcM ()
checkType t
  = case t of
      JClass c -> checkClassName c
      _        ->
        do type_ctxt <- getTypeCtxt
           let free_ty_vars = freeTVars t `Set.difference` Set.fromList (map fst (filter (\(_,k) -> k == Star) (Map.toList type_ctxt)))
           unless (Set.null free_ty_vars) $
             throwError (NotInScopeTVar (head (Set.toList free_ty_vars)))

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

checkMethodCall :: JCallee ClassName -> MethodName -> [ClassName] -> TcM ClassName
checkMethodCall callee m args
  = do typeserver <- getTypeServer
       res <- liftIO (methodTypeOf typeserver c (m, static_flag) args)
       case res of
         Nothing           -> throwError (NoSuchMethod callee m args)
         Just return_class -> return return_class
    where
       (static_flag, c) = unwrapJCallee callee

checkFieldAccess :: JCallee ClassName -> FieldName -> TcM ClassName
checkFieldAccess callee f
  = do typeserver <- getTypeServer
       res <- liftIO (fieldTypeOf typeserver c (f, static_flag))
       case res of
         Nothing           -> throwError (NoSuchField callee f)
         Just return_class -> return return_class
    where
       (static_flag, c) = unwrapJCallee callee

unwrapJCallee :: JCallee ClassName -> (Bool, ClassName)
unwrapJCallee (NonStatic c) = (False, c)
unwrapJCallee (Static    c) = (True, c)

srcLitType :: Lit -> Type
srcLitType (Int _)    = JClass "java.lang.Integer"
srcLitType (String _) = JClass "java.lang.String"
srcLitType (Bool _)   = JClass "java.lang.Boolean"
srcLitType (Char _)   = JClass "java.lang.Character"
srcLitType UnitLit    = Unit

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
