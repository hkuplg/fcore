{-
Infrastructure for typechecking.
-}
{-# LANGUAGE RecordWildCards #-}
module Checker where

import           IOEnv
import           JavaUtils
import qualified JvmTypeQuery
import           Src
import           TypeErrors

import           Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.Set as Set

type Checker a = ExceptT LTypeErrorExpr (IOEnv CheckerState) a

getCheckerState :: Checker CheckerState
getCheckerState = lift getEnv

setCheckerState :: CheckerState -> Checker ()
setCheckerState tc_env = lift $ setEnv tc_env

getTypeContext :: Checker TypeContext
getTypeContext = liftM checkerTypeContext getCheckerState

getValueContext :: Checker ValueContext
getValueContext = liftM checkerValueContext getCheckerState

getModuleContext :: Checker ModuleContext
getModuleContext = liftM checkerModuleContext getCheckerState

getTypeServer :: Checker JvmTypeQuery.Connection
getTypeServer = liftM checkerTypeServer getCheckerState

getMemoizedJavaClasses :: Checker (Set.Set ClassName)
getMemoizedJavaClasses = liftM checkerMemoizedJavaClasses getCheckerState

getCompilationMethods :: Checker String
getCompilationMethods = liftM compilationMethods getCheckerState

memoizeJavaClass :: ClassName -> Checker ()
memoizeJavaClass c
  = do CheckerState{..} <- getCheckerState
       memoized_java_classes <- getMemoizedJavaClasses
       setCheckerState CheckerState{ checkerMemoizedJavaClasses = c `Set.insert` memoized_java_classes, ..}

withLocalTVars :: [(ReadId, Kind)] -> Checker a -> Checker a
withLocalTVars tvars do_this
  = do delta <- getTypeContext
       let delta' = addTVarToContext tvars delta
       CheckerState {..} <- getCheckerState
       setCheckerState CheckerState { checkerTypeContext = delta', ..}
       r <- do_this
       CheckerState {..} <- getCheckerState
       setCheckerState CheckerState { checkerTypeContext = delta, ..}
       return r

withLocalConstrainedTVars :: [(ReadId, Maybe Type)] -> Checker a -> Checker a
withLocalConstrainedTVars tvars do_this
 = do delta <- getTypeContext
      let delta' = addConstrainedTVarToContext tvars delta
      CheckerState {..} <- getCheckerState
      setCheckerState CheckerState { checkerTypeContext = delta', ..}
      r <- do_this
      CheckerState {..} <- getCheckerState
      setCheckerState CheckerState { checkerTypeContext = delta, ..}
      return r

withTypeSynonym :: [(ReadId, Type, Kind)] -> Checker a -> Checker a
withTypeSynonym tvars do_this
  = do delta <- getTypeContext
       let delta' = addTypeSynonym tvars delta
       CheckerState {..} <- getCheckerState
       setCheckerState CheckerState { checkerTypeContext = delta', ..}
       r <- do_this
       CheckerState {..} <- getCheckerState
       setCheckerState CheckerState { checkerTypeContext = delta, ..}
       return r

withLocalVars :: [(ReadId, Type)]-> Checker a -> Checker a
withLocalVars vars do_this
  = do gamma <- getValueContext
       let gamma' = addVarToContext vars gamma
       CheckerState {..} <- getCheckerState
       setCheckerState CheckerState { checkerValueContext = gamma', ..}
       r <- do_this
       CheckerState {..} <- getCheckerState
       setCheckerState CheckerState { checkerValueContext = gamma, ..}
       return r

withLocalMVars :: [(ReadId, ModuleMapInfo)]-> Checker a -> Checker a
withLocalMVars vars do_this
  = do sigma <- getModuleContext
       let sigma' = addModuleInfo vars sigma
       CheckerState {..} <- getCheckerState
       setCheckerState CheckerState { checkerModuleContext = sigma', ..}
       r <- do_this
       CheckerState {..} <- getCheckerState
       setCheckerState CheckerState { checkerModuleContext = sigma, ..}
       return r


data CheckerState
  = CheckerState
  { checkerTypeContext  :: TypeContext
  , checkerValueContext :: ValueContext
  , checkerModuleContext :: ModuleContext
  , checkerTypeServer   :: JvmTypeQuery.Connection
  , checkerMemoizedJavaClasses :: Set.Set ClassName -- Memoized Java class names
  , compilationMethods :: String
  }

mkInitCheckerState :: String -> ModuleContext -> JvmTypeQuery.Connection -> CheckerState
mkInitCheckerState m module_ctxt type_server
  = CheckerState
  { checkerTypeContext     = Map.empty
  , checkerValueContext    = Map.empty
  , checkerModuleContext    = module_ctxt
  , checkerTypeServer   = type_server
  , checkerMemoizedJavaClasses = Set.empty
  , compilationMethods = m
  }

-- Temporary hack for REPL
mkInitCheckerStateWithEnv :: ValueContext -> JvmTypeQuery.Connection -> CheckerState
mkInitCheckerStateWithEnv value_ctxt type_server
  = CheckerState
  { checkerTypeContext     = Map.empty
  , checkerValueContext    = value_ctxt
  , checkerModuleContext    = Map.empty
  , checkerTypeServer   = type_server
  , checkerMemoizedJavaClasses = Set.empty
  , compilationMethods = ""
  }

lookupVar :: Name -> Checker (Maybe Type)
lookupVar x
  = do valueCtxt <- getValueContext
       return (Map.lookup x valueCtxt)

lookupModuleVar :: Name -> Checker (Maybe ModuleMapInfo)
lookupModuleVar x
  = do moduleCtxt <- getModuleContext
       return (Map.lookup x moduleCtxt)

lookupTVarKind :: Name -> Checker (Maybe Kind)
lookupTVarKind a
 = do typeCtxt <- getTypeContext
      return (fmap (\(kind,_,_) -> kind) (Map.lookup a typeCtxt))

subtype :: Type -> Type -> Checker Bool
subtype t1 t2
  = do typeCtxt <- getTypeContext
       return (Src.subtype typeCtxt t1 t2)
