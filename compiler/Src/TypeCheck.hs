{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-unused-binds #-}
{-# LANGUAGE RecordWildCards #-}

module Src.TypeCheck
  ( infer
  ) where

import Src.Syntax

import JvmTypeQuery
import JavaUtils
import Panic

import Text.PrettyPrint.Leijen

import System.IO
import System.Process

import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.IO.Class (liftIO)

import Data.Maybe       (fromJust)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Prelude hiding (pred)

-- The monad for typechecking
type TcM = ErrorT TypeError IO

-- State TcEnv (Either TypeError ?)

data TypeError
  = NotInScope        { msg       :: String }
  | General           { msg       :: String }
  | NotAJvmType       { msg       :: ClassName }
  | NoSuchConstructor { className :: ClassName
                      , argsExpr  :: [Expr Name]
                      , argsType  :: [Type] }
  | NoSuchMethod      { className :: ClassName
                      , mName     :: MethodName
                      , argsExpr  :: [Expr Name]
                      , argsType  :: [Type] }
  | NoSuchField       { className :: ClassName
                      , fName     :: FieldName
                      , static    :: Bool }
  | Mismatch          { expr      :: Expr Name
                      , expected  :: Type
                      , actual    :: Type }
  deriving (Show)

instance Error TypeError where
--   strMsg

instance Pretty TypeError where
  pretty Mismatch{..} =
    text "Type mismatch:" <$>
    text "Expected type:" <+> pretty expected <$>
    text "  Actual type:" <+> pretty actual <$>
    text "In the expression:" <+> pretty expr
  pretty NotInScope{..}  = text "Not in scope:" <+> string msg
  pretty General{..}     = string msg
  pretty (NotAJvmType c) = text (q c) <+> text "is not a JVM type"
  pretty (NoSuchConstructor c _ t) =
    text "Class" <+> text (q c) <+> text "has no such constructor:" <$>
    text "Parameters:" <+> text "(" <> hsep (map pretty t) <> text ")"
  pretty (NoSuchMethod c m _ t) =
    text "Class" <+> text (q c) <+> text "has no such method:" <$>
    text "Name:" <+> text (q m) <$>
    text "Parameters:" <+> text "(" <> hsep (map pretty t) <> text ")"
  pretty (NoSuchField c f b) =
    let msg = text "Class" <+> text (q c) <+>
              text "has no such field:" <+> text (q f)
    in
    if b then msg <+> text "(static)" else msg

q :: Name -> String
q x = "`" ++ x ++ "'"

--throwE :: TypeError Name -> TcM ()
--throwE te = throwError . show . pretty te

checkWellformed :: (Handle, Handle) -> TypeContext -> Type -> TcM ()
checkWellformed io _ (JClass c) = do ok <- liftIO $ isJvmType io c
                                     unless ok (throwError (NotAJvmType c))
checkWellformed _  d t =
  let s = freeTyVars t `Set.difference` d in
  unless (Set.null s) $
    throwError NotInScope { msg = "type variable " ++ q (head $ Set.toList s) }

withTypeServer do_this =
  do cp <- liftIO classpath
     let p = (proc "java" ["-cp", cp, "hk.hku.cs.f2j.TypeServer"])
               { std_in = CreatePipe, std_out = CreatePipe }
     (Just inp, Just out, _, proch) <- liftIO $ createProcess p
     liftIO $ hSetBuffering inp NoBuffering
     liftIO $ hSetBuffering out NoBuffering
     res <- do_this (inp, out)
     liftIO $ terminateProcess proch
     return res

infer :: Expr Name -> TcM (Expr TcId, Type)
infer e = withTypeServer
            (\h -> tcExpr h (Set.empty, Map.empty) e)

tcLit :: Lit -> TcM (Expr TcId, Type)
tcLit (Integer n) = return (Lit (Integer n), JClass "java.lang.Integer")
tcLit (String s)  = return (Lit (String s),  JClass "java.lang.String")
tcLit (Boolean b) = return (Lit (Boolean b), JClass "java.lang.Boolean")
tcLit (Char c)    = return (Lit (Char c),    JClass "java.lang.Character")

tcExpr :: (Handle, Handle)
       -> (TypeContext, ValueContext)
       -> Expr Name
       -> TcM (Expr TcId, Type)
tcExpr io (d, g) = go
  where
    go (Var x) = case Map.lookup x g of
                   Just t  -> return (Var (x,t), t)
                   Nothing -> throwError NotInScope { msg = "variable " ++ q x }

    go (Lit lit) = tcLit lit

    go (App e1 e2) =
      do (e1', t)  <- go e1
         (e2', t') <- go e2
         case t of
           Fun t1 t2 | t' `subtype` t1 ->
                         return (App e1' e2', t2) -- TODO: need var renaming?
           Fun t1 _ -> throwError Mismatch { expr = e2, expected = t1, actual = t' }
           _        -> throwError Mismatch { expr     = e1
                                           , expected = Fun t' (TyVar "_")
                                           , actual   = t }

    go (BLam a e)
      | a `Set.member` d =
          throwError
            General { msg = "This type variable " ++ q a ++
                            " shadows an existing type variable" ++
                            " in an outer scope"
                    }
      | otherwise       = do (e', t) <- tcExpr io (Set.insert a d, g) e
                             return (BLam a e', Forall a t)

    go (Lam (x,t) e) =
      do checkWellformed io d t
         (e', t') <- tcExpr io (d, Map.insert x t g) e
         return (Lam (x,t) e', Fun t t')

    go (TApp e t) =
      do checkWellformed io d t
         (e', t1) <- go e
         case t1 of
           Forall a t' -> return (TApp e' t, substFreeTyVars (a, t) t')
           _           -> throwError
                            Mismatch { expr     = TApp e t
                                     , expected = Forall "_" (TyVar "_")
                                     , actual   = t1 }

    go (Tuple es)
      | length es < 2 = panic $ "Src.TypeCheck.tcExpr: Tuple: " ++
                                "fewer than two items"
      | otherwise     = do (es', ts) <- mapAndUnzipM go es
                           return (Tuple es', Product ts)

    go (Proj e i) =
      do (e', t) <- go e
         case t of
           Product ts ->
             if 1 <= i && i <= length ts
               then return (Proj e' i, ts !! (i-1))
               else throwError General { msg = "Index too large in projection" }
           _          ->
             throwError $
                General "Projection of a expression that is not of product type"

    go (PrimOp e1 op e2) =
      do exp1@(e1', t1) <- go e1
         exp2@(e2', t2) <- go e2
         case op of
           (Arith _)   -> shouldAllBe exp1 exp2 (JClass javaIntClass)
           (Compare _) ->
             if alphaEqTy t1 t2
               then return (PrimOp e1' op e2', JClass javaBoolClass)
               else throwError Mismatch { expr = e2, expected = t1, actual = t2 }
           (Logic _)   -> shouldAllBe exp1 exp2 (JClass javaBoolClass)
       where
         shouldAllBe (e1', t1) (e2', t2) t =
           case (alphaEqTy t t1, alphaEqTy t t2) of
             (True, True) -> return (PrimOp e1' op e2', t)
             (True, _)    -> throwError
                               Mismatch { expr = e2, expected = t, actual = t2 }
             (_, _)       -> throwError
                               Mismatch { expr = e1, expected = t, actual = t1 }

    go (If pred b1 b2) = do
      (pred', predTy) <- go pred
      case predTy of
        JClass "java.lang.Boolean" ->
          do (b1', b1Ty) <- go b1
             (b2', b2Ty) <- go b2
             if b1Ty `alphaEqTy` b2Ty
               then return (If pred' b1' b2', b1Ty)
               else throwError
                      Mismatch { expr     = b2
                               , expected = b1Ty
                               , actual   = b2Ty }
        _   -> throwError
                 Mismatch { expr     = pred
                          , expected = JClass javaBoolClass
                          , actual   = predTy }

    go (Let NonRec bs e) =
      do checkBinds io d bs
         bs' <-
           forM bs (\Bind{..} ->
             do let d_local = Set.fromList bindTargs
                    g_local = Map.fromList bindArgs
                (rhs, inferredRhsTy) <-
                  tcExpr
                    io
                    (d_local `Set.union` d,g_local `Map.union` g)
                    bindRhs
                case bindRhsAnnot of
                  Nothing           -> return ()
                  Just claimedRhsTy ->
                    unless (claimedRhsTy `alphaEqTy` inferredRhsTy) $
                      throwError
                        Mismatch { expr     = bindRhs
                                 , expected = claimedRhsTy
                                 , actual   = inferredRhsTy }
                return ( bindId
                       , wrap Forall bindTargs $
                           wrap Fun [t | (_,t) <- bindArgs] inferredRhsTy
                       , wrap BLam bindTargs $ wrap Lam bindArgs rhs))
         let (fs, ts, _es) = unzip3 bs'
         (e', t) <- tcExpr io (d, Map.fromList (zip fs ts) `Map.union` g) e
         return (LetOut NonRec bs' e', t)

    go (Let Rec bs e) =
      do checkBinds io d bs
         sigs <-
           liftM Map.fromList $
             forM bs (\Bind{..} ->
               case bindRhsAnnot of
                 Nothing    ->
                   throwError
                     (General "Missing type annotation for the right hand side")
                 Just rhsTy ->
                   return ( bindId
                          , wrap Forall bindTargs $
                              wrap Fun [t | (_,t) <- bindArgs] rhsTy))
         bs' <-
           forM bs (\Bind{..} ->
             do let d_local = Set.fromList bindTargs
                    g_local = Map.fromList bindArgs
                (rhs, inferredRhsTy) <- tcExpr
                                          io
                                          (d_local `Set.union` d
                                          ,g_local `Map.union` sigs `Map.union` g)
                                          bindRhs
                let claimedRhsTy = fromJust bindRhsAnnot
                unless (claimedRhsTy `alphaEqTy` inferredRhsTy) $
                  throwError
                    Mismatch { expr     = bindRhs
                             , expected = claimedRhsTy
                             , actual   = inferredRhsTy }
                return ( bindId
                       , wrap Forall bindTargs $
                           wrap Fun [t | (_,t) <- bindArgs] inferredRhsTy
                       , wrap BLam bindTargs $ wrap Lam bindArgs rhs))
         let (fs, ts, _es) = unzip3 bs'
         (e', t) <- tcExpr io (d, Map.fromList (zip fs ts) `Map.union` g) e
         return (LetOut Rec bs' e', t)

    go (LetOut{..}) = panic "Src.TypeCheck.tcExpr: LetOut"

    go (JNewObj c args) =
      do ok <- liftIO $ isJvmType io c
         if ok
           then do (args', typs') <- mapAndUnzipM go args
                   strArgs <- checkJavaArgs typs'
                   ok' <- liftIO $ hasConstructor io c strArgs
                   if ok'
                     then return (JNewObj c args', JClass c)
                     else throwError
                            NoSuchConstructor { className = c
                                              , argsExpr = args
                                              , argsType = typs' }
           else throwError (NotAJvmType c)

    go (JMethod expr m args _) =
      case expr of
        (Left e) ->
          do (e', t') <- go e
             case t' of
               JClass cls ->
                 do (args', typs') <- mapAndUnzipM go args
                    strArgs <- checkJavaArgs typs'
                    retName <- liftIO $ methodTypeOf io cls (m, False) strArgs
                    case retName of
                      Just r  -> return (JMethod (Left e') m args' r, JClass r)
                      Nothing -> throwError
                                   NoSuchMethod { className = cls
                                                , mName     = m
                                                , argsExpr  = args
                                                , argsType  = typs' }
               otherType -> throwError (NotAJvmType (show otherType))
        (Right className) ->
          do (args', typs') <- mapAndUnzipM go args
             strArgs <- checkJavaArgs typs'
             retName <- liftIO $ methodTypeOf io className (m, True) strArgs
             case retName of
               Just r  -> return (JMethod (Right className) m args' r, JClass r)
               Nothing -> throwError
                            NoSuchMethod { className = className
                                         , mName     = m
                                         , argsExpr  = args
                                         , argsType  = typs' }

    go (SeqExprs es) = do (es', typs') <- mapAndUnzipM go es
                          return (SeqExprs es', last typs')

    go (JField expr f _) =
      case expr of
        (Left e) ->
          do (e', t') <- go e
             case t' of
               JClass cls -> do retName <- liftIO $ fieldTypeOf io cls (f, False)
                                case retName of
                                  Just r  -> return (JField (Left e') f r, JClass r)
                                  Nothing -> throwError
                                               NoSuchField { className = cls
                                                           , fName     = f
                                                           , static    = False }
               otherType -> throwError $ NotAJvmType (show otherType)
        (Right cls) ->
          do retName <- liftIO $ fieldTypeOf io cls (f, True)
             case retName of Just r  -> return (JField (Right cls) f r, JClass r)
                             Nothing -> throwError
                                          NoSuchField { className = cls
                                                      , fName     = f
                                                      , static    = True }

    go (Merge e1 e2) =
      do (e1', t1) <- go e1
         (e2', t2) <- go e2
         return (Merge e1' e2', And t1 t2)

checkJavaArgs :: [Type] -> TcM [Name]
checkJavaArgs = mapM check
  where
    check (JClass name) = return name
    check otherType     = throwError $ NotAJvmType $ show otherType


checkBinds :: (Handle, Handle) -> TypeContext -> [Bind Name] -> TcM ()
checkBinds io d bs =
  do checkForDup "identifiers" (map bindId bs)
     forM_ bs (\Bind{..} ->
       do checkForDup "type arguments" bindTargs
          checkForDup "arguments"      [x | (x, _) <- bindArgs]
          forM_ bindArgs (\(_,t) ->
            do let d' = Set.fromList bindTargs `Set.union` d
               checkWellformed io d' t))


checkForDup :: String -> [String] -> TcM ()
checkForDup what xs =
  case findFirstDup xs of
    Just x  -> throwError General { msg = "Duplicate " ++ what ++ ": " ++ q x }
    Nothing -> return ()


findFirstDup :: Ord a => [a] -> Maybe a
findFirstDup xs = go xs Set.empty
  where
    go []      _ = Nothing
    go (x:xs') s = if Set.member x s
                     then Just x
                     else go xs' (Set.insert x s)