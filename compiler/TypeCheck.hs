-- The source language typechecker

{- We make typechecking (this file) happen before desugaring (Desugar.hs) so
that the error messages presented to the programmer can be clearer. However, an
undesired consequence of such approach for the compiler implementer is that the
implementation of the typing rules does not follow strictly the formalization.
For instance, in the formalization there is no rule for handling multi-field
records as they are desugared into intersections of single-field records first.
But here we have to handle such cases.-}

{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

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
import PrettyUtils
import JvmTypeQuery
import Panic
import StringPrefixes

import Text.PrettyPrint.ANSI.Leijen

import System.IO
import System.Process

import Control.Monad.Error

import Data.Maybe (fromMaybe)
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import Data.List (intersperse)

import Prelude hiding (pred)

type Connection = (Handle, Handle)

typeCheck :: ReaderExpr -> IO (Either TypeError (Type, CheckedExpr))
-- type_server is (Handle, Handle)
typeCheck e = withTypeServer (\type_server ->
  (evalIOEnv (mkInitTcEnv type_server) . runErrorT . infer) e)

-- Temporary hack for REPL
typeCheckWithEnv :: ValueContext -> ReaderExpr -> IO (Either TypeError (Type, CheckedExpr))
-- type_server is (Handle, Handle)
typeCheckWithEnv value_ctxt e = withTypeServer (\type_server ->
  (evalIOEnv (mkInitTcEnvWithEnv value_ctxt type_server) . runErrorT . infer) e)

withTypeServer :: (Connection -> IO a) -> IO a
withTypeServer do_this =
  do cp <- getClassPath
     let p = (proc "java" ["-cp", cp, namespace ++ "TypeServer"])
               { std_in = CreatePipe, std_out = CreatePipe }
     (Just inp, Just out, _, proch) <- createProcess p
     hSetBuffering inp NoBuffering
     hSetBuffering out NoBuffering
     res <- do_this (inp, out)
     terminateProcess proch
     return res

data TcEnv
  = TcEnv
  { tceTypeContext     :: TypeContext
  , tceValueContext    :: ValueContext
  , tceTypeserver   :: Connection
  , tceMemoizedJavaClasses :: Set.Set ClassName -- Memoized Java class names
  }

mkInitTcEnv :: Connection -> TcEnv
mkInitTcEnv type_server
  = TcEnv
  { tceTypeContext     = Map.empty
  , tceValueContext    = Map.empty
  , tceTypeserver   = type_server
  , tceMemoizedJavaClasses = Set.empty
  }

-- Temporary hack for REPL
mkInitTcEnvWithEnv :: ValueContext -> Connection -> TcEnv
mkInitTcEnvWithEnv value_ctxt type_server
  = TcEnv
  { tceTypeContext     = Map.empty
  , tceValueContext    = value_ctxt
  , tceTypeserver   = type_server
  , tceMemoizedJavaClasses = Set.empty
  }

data TypeError
  = General Doc
  | ConflictingDefinitions Name
  | ExpectJClass
  | IndexTooLarge
  | TypeMismatch Type Type
  | KindMismatch Kind Kind
  | MissingRHSAnnot
  | NotInScope Name
  | ProjectionOfNonProduct
  | NotWellKinded Type
  | NotMember Name Type
  | NotAFunction Type

  -- Java-specific type errors
  | NoSuchClass       ClassName
  | NoSuchConstructor ClassName [ClassName]
  | NoSuchMethod      (JCallee ClassName) MethodName [ClassName]
  | NoSuchField       (JCallee ClassName) FieldName
  deriving (Show)

instance Pretty TypeError where
  pretty (General doc)      = prettyError <+> doc
  pretty (NotInScope x)  = prettyError <+> code (text x) <+> text "is not in scope"
  pretty (NotWellKinded t)  = prettyError <+> code (pretty t) <+> text "is not well-kinded"
  pretty (KindMismatch expected actual) =
    prettyError <+> text "kind mismatch" <> colon <$>
    indent 2 (text "expected:" <+> code (pretty expected) <$>
              text "  actual:" <+> code (pretty actual))
  pretty (TypeMismatch expected actual) =
    prettyError <+> text "type mismatch" <> colon <$>
    indent 2 (text "expected:" <+> code (pretty expected) <$>
              text "  actual:" <+> code (pretty actual))

  pretty (NoSuchClass c)  = prettyError <+> text "no such class:" <+> code (text c)
  pretty (NotMember x t)  = prettyError <+> code (text x) <+> text "is not a member of the type" <+> code (pretty t)
  pretty (NotAFunction t) = prettyError <+> code (pretty t) <+> text "is not a function; it cannot be applied"

  -- Java-specific type errors
  pretty (NoSuchMethod (NonStatic c) m cs) =
    prettyError <+> text "no such method" <+> code (text m) <+>
    text "on" <+> code (pretty (JType (JClass c))) <+>
    text "with parameters of type" <+> commas (map (code . pretty . JType . JClass) cs)
  pretty (NoSuchMethod (Static c) m cs) =
    prettyError <+> text "no such static method" <+> code (text m) <+>
    text "on" <+> code (pretty (JType (JClass c))) <+>
    text "with parameters of type" <+> commas (map (code . pretty . JType . JClass) cs)

  pretty (NoSuchField (NonStatic c) f) =
    prettyError <+> text "no such field" <+> code (text f) <+>
    text "on" <+> code (pretty (JType (JClass c)))
  pretty (NoSuchField (Static c) f) =
    prettyError <+> text "no such static field" <+> code (text f) <+>
    text "on" <+> code (pretty (JType (JClass c)))

  pretty e = prettyError <+> text (show e)

instance Error TypeError where
  -- strMsg

type Checker a = ErrorT TypeError (IOEnv TcEnv) a

getTcEnv :: Checker TcEnv
getTcEnv = lift getEnv

setTcEnv :: TcEnv -> Checker ()
setTcEnv tc_env = lift $ setEnv tc_env

getTypeContext :: Checker TypeContext
getTypeContext = liftM tceTypeContext getTcEnv

getValueContext :: Checker ValueContext
getValueContext = liftM tceValueContext getTcEnv

getTypeServer :: Checker (Handle, Handle)
getTypeServer = liftM tceTypeserver getTcEnv

getMemoizedJavaClasses :: Checker (Set.Set ClassName)
getMemoizedJavaClasses = liftM tceMemoizedJavaClasses getTcEnv

memoizeJavaClass :: ClassName -> Checker ()
memoizeJavaClass c
  = do TcEnv{..} <- getTcEnv
       memoized_java_classes <- getMemoizedJavaClasses
       setTcEnv TcEnv{ tceMemoizedJavaClasses = c `Set.insert` memoized_java_classes, ..}

withLocalTVars :: [(ReaderId, (Kind, TypeValue))] -> Checker a -> Checker a
withLocalTVars tvars do_this
  = do delta <- getTypeContext
       let delta' = Map.fromList tvars `Map.union` delta
                -- `Map.fromList` is right-biased and `Map.union` is left-biased.
       TcEnv {..} <- getTcEnv
       setTcEnv TcEnv { tceTypeContext = delta', ..}
       r <- do_this
       TcEnv {..} <- getTcEnv
       setTcEnv TcEnv { tceTypeContext = delta, ..}
       return r

withLocalVars :: [(ReaderId, Type)]-> Checker a -> Checker a
withLocalVars vars do_this
  = do gamma <- getValueContext
       let gamma' = Map.fromList vars `Map.union` gamma
                -- `Map.fromList` is right-biased and `Map.union` is left-biased.
       TcEnv {..} <- getTcEnv
       setTcEnv TcEnv { tceValueContext = gamma', ..}
       r <- do_this
       TcEnv {..} <- getTcEnv
       setTcEnv TcEnv { tceValueContext = gamma, ..}
       return r

type TypeSubstitution = Map.Map Name Type

applyTSubst :: TypeSubstitution -> Type -> Type
applyTSubst s (TVar a)     = fromMaybe (TVar a) (Map.lookup a s)
-- applyTSubst _ (JClass c)   = JClass c
applyTSubst _ (JType c)    = JType c
applyTSubst s (Fun t1 t2)  = Fun (applyTSubst s t1) (applyTSubst s t2)
applyTSubst s (Forall a t) = Forall a (applyTSubst s' t) where s' = Map.delete a s
applyTSubst _ _            = sorry "TypeCheck.applyTSubst"

-- | Kinding.
kind :: TypeContext -> Type -> IO (Maybe Kind)
kind d (TVar a)     = case Map.lookup a d of Nothing     -> return Nothing
                                             Just (k, _) -> return (Just k)
kind _  Unit        = return (Just Star)
kind d (Fun t1 t2)  = justStarIffAllHaveKindStar d [t1, t2]
kind d (Forall a t) = kind d' t where d' = Map.insert a (Star, TerminalType) d
kind d (Product ts) = justStarIffAllHaveKindStar d ts
kind d (Record fs)  = justStarIffAllHaveKindStar d (map snd fs)
kind d (ListOf t)   = kind d t
kind d (And t1 t2)  = justStarIffAllHaveKindStar d [t1, t2]
kind d (Thunk t)    = kind d t

-- Δ,x::* ⊢ t :: k
-- -------------------- (K-Abs) Restriction compared to F_omega: x can only have kind *
-- Δ ⊢ λx. t :: * => k
kind d (OpAbs x t) = do
  maybe_k <- kind (Map.insert x (Star, TerminalType) d) t
  case maybe_k of
    Nothing -> return Nothing
    Just k  -> return $ Just (KArrow Star k)

-- Δ ⊢ t1 :: k11 => k12  Δ ⊢ t2 :: k11
-- ------------------------------------ (K-App)
-- Δ ⊢ t1 t2 :: k12
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
infer :: ReaderExpr -> Checker (Type, CheckedExpr)
infer (Var name)
  = do value_ctxt <- getValueContext
       case Map.lookup name value_ctxt of
         Just t  -> return (t, Var (name,t))
         Nothing -> throwError (NotInScope name)

infer (Lit lit) = return (srcLitType lit, Lit lit)

infer (Lam (x1,t1) e)
  = do checkType t1
       d <- getTypeContext
       let t1' = expandType d t1
       (t', e') <- withLocalVars [(x1,t1')] (infer e)
       return (Fun t1' t', Lam (x1,t1') e')

infer (App e1 e2) =
  do (t1, e1') <- infer e1
     (t2, e2') <- infer e2
     case t1 of
       Fun t11 t12 ->
         do d <- getTypeContext
            unless (subtype d t2 t11) $ throwError (TypeMismatch t11 t2)
            case (t11, t2) of
              (Thunk _, Thunk _) -> return (t12, App e1' e2')
              (Thunk _, _)       -> return (t12, App e1' (Lam ("_", Unit) e2'))
              (_, Thunk _)       -> return (t12, App e1' (App e2' (Lit UnitLit)))
              (_, _)             -> return (t12, App e1' e2')

       Thunk (Fun t11 t12) ->
         do d <- getTypeContext
            unless (subtype d t2 t11) $ throwError (TypeMismatch t11 t2)
            case (t11, t2) of
              (Thunk _, Thunk _) -> return (t12, App e1'' e2')
              (Thunk _, _)       -> return (t12, App e1'' (Lam ("_", Unit) e2'))
              (_, Thunk _)       -> return (t12, App e1'' (App e2' (Lit UnitLit)))
              (_, _)             -> return (t12, App e1'' e2')
         where e1'' = App e1' (Lit UnitLit)

       _ -> throwError (NotAFunction t1)

infer (BLam a e)
  = do (t, e') <- withLocalTVars [(a, (Star, TerminalType))] (infer e)
       return (Forall a t, BLam a e')

infer (TApp e arg)
  = do (t, e') <- infer e
       checkType arg
       d <- getTypeContext
       let arg' = expandType d arg
       case t of
         Forall a t1 -> return (fsubstTT (a, arg') t1, TApp e' arg')
         _           -> sorry "TypeCheck.infer: TApp"

infer (Tuple es)
  | length es < 2 = panic "Src.TypeCheck.infer: Tuple: fewer than two items"
  | otherwise     = do (ts, es') <- mapAndUnzipM infer es
                       return (Product ts, Tuple es')

infer (Proj e i)
  = do (t, e') <- infer e
       case t of
         Product ts
           | 1 <= i && i <= length ts -> return (ts !! (i - 1), Proj e' i)
           | otherwise -> throwError IndexTooLarge
         _ -> throwError ProjectionOfNonProduct

infer (PrimOp e1 op e2) =
  case op of
    Arith _ ->
      do (_, e1') <- inferAgainst e1 (JType (JClass "java.lang.Integer"))
         (_, e2') <- inferAgainst e2 (JType (JClass "java.lang.Integer"))
         return (JType (JClass "java.lang.Integer"), PrimOp e1' op e2')
    Compare _ ->
      do (t1, e1') <- infer e1
         (_ , e2') <- inferAgainst e2 t1
         return (JType (JClass "java.lang.Boolean"), PrimOp e1' op e2')
    Logic _ ->
      do (_, e1') <- inferAgainst e1 (JType (JClass "java.lang.Boolean"))
         (_, e2') <- inferAgainst e2 (JType (JClass "java.lang.Boolean"))
         return (JType (JClass "java.lang.Boolean"), PrimOp e1' op e2')

infer (If pred b1 b2) =
  do (_, pred') <- inferAgainst pred (JType (JClass "java.lang.Boolean"))
     (t1, b1')  <- infer b1
     (_,  b2')  <- inferAgainst b2 t1
     return (t1, If pred' b1' b2')

infer (Let rec_flag binds e) =
  do checkDupNames (map bindId binds)
     -- type_ctxt <- getTypeContext
     -- when True $
     --     throwError (General $ text (show type_ctxt))
     binds' <- case rec_flag of
                 NonRec -> mapM normalizeBind binds
                 Rec    -> do sigs <- collectBindIdSigs binds
                              withLocalVars sigs (mapM normalizeBind binds)
     -- when True $ throwError (General $ text (show binds'))
     (t, e') <- withLocalVars (map (\ (f,t,_) -> (f,t)) binds') (infer e)
     return (t, LetOut rec_flag binds' e')

infer (LetOut{..}) = panic "TypeCheck.infer: LetOut"

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
infer (Dot e x Nothing) =
  do (t, _) <- infer e
     case deThunkOnce t of
       JType (JClass _) -> infer (JField (NonStatic e) x undefined)
       Record _         -> infer (RecordElim e x)
       And _ _          -> infer (RecordElim e x)
       _                -> throwError (NotMember x t)

-- e.x ( )
infer (Dot e x (Just ([], UnitImpossible))) =
  do (t, _) <- infer e
     case deThunkOnce t of
       JType (JClass _) -> infer (JMethod (NonStatic e) x [] undefined)
       _                -> throwError (NotMember x t)

-- e.x ()
infer (Dot e x (Just ([], UnitPossible))) =
  do (t, _) <- infer e
     case deThunkOnce t of
       JType (JClass _) -> infer (JMethod (NonStatic e) x [] undefined)
       Record _         -> infer (App (RecordElim e x) (Lit UnitLit))
       And _ _          -> infer (App (RecordElim e x) (Lit UnitLit))
       _                -> throwError (NotMember x t)

-- e.x (a)
infer (Dot e x (Just ([arg], _))) =
  do (t, _) <- infer e
     case deThunkOnce t of
       JType (JClass _) -> infer (JMethod (NonStatic e) x [arg] undefined)
       Record _         -> infer (App (RecordElim e x) arg)
       And _ _          -> infer (App (RecordElim e x) arg)
       _                -> throwError (NotMember x t)

-- e.x (a,...)
infer (Dot e x (Just (args, _))) =
  do (t, _) <- infer e
     case deThunkOnce t of
       JType (JClass _) -> infer (JMethod (NonStatic e) x args undefined)
       Record _         -> infer (App (RecordElim e x) (Tuple args))
       And _ _          -> infer (App (RecordElim e x) (Tuple args))
       _                -> throwError (NotMember x t)

-- JNew, JMethod, and JField

infer (JNew c args)
  = do checkClassName c
       (arg_cs, args') <- mapAndUnzipM inferAgainstAnyJClass args
       checkConstruction c arg_cs
       return (JType (JClass c), JNew c args')

infer (JMethod callee m args _) =
  case callee of
    Static c ->
      do (arg_cs, args') <- mapAndUnzipM inferAgainstAnyJClass args
         ret_c <- checkMethodCall (Static c) m arg_cs
         let ret_type = case ret_c of "java.lang.Void" -> Unit
                                      "char" -> JType (JPrim "char")
                                      _ -> JType (JClass ret_c)
         return (ret_type, JMethod (Static c) m args' ret_c)
    NonStatic e ->
      do (c, e')         <- inferAgainstAnyJClass e
         (arg_cs, args') <- mapAndUnzipM inferAgainstAnyJClass args
         ret_c <- checkMethodCall (NonStatic c) m arg_cs
         let ret_type = case ret_c of "java.lang.Void" -> Unit
                                      "char" -> JType (JPrim "char")
                                      _ -> JType (JClass ret_c)
         return (ret_type, JMethod (NonStatic e') m args' ret_c)

infer (JField callee f _) =
  case callee of
    Static c ->
      do ret_c <- checkFieldAccess (Static c) f
         if ret_c == "char"
            then return (JType (JPrim ret_c), JField (Static c) f ret_c)
            else return (JType (JClass ret_c), JField (Static c) f ret_c)
    NonStatic e ->
      do (t, e') <- infer e
         case t of
           JType (JClass c) ->
             do ret_c   <- checkFieldAccess (NonStatic c) f
                if ret_c == "char"
                  then return (JType (JPrim "char"), JField (NonStatic e') f ret_c)
                  else return (JType (JClass ret_c), JField (NonStatic e') f ret_c)
           _ -> throwError (NotMember f t)

infer (Seq es) =
  do (ts, es') <- mapAndUnzipM infer es
     return (last ts, Seq es')

infer (Merge e1 e2) =
  do (t1, e1') <- infer e1
     (t2, e2') <- infer e2
     return (And t1 t2, Merge e1' e2')

infer (PolyList l t) =
  do (ts, es) <- mapAndUnzipM infer l
     case ts of [] -> return (ListOf t, PolyList es t)
                _  ->
                     do d <- getTypeContext
                        if all (alphaEq d t) ts
                          then return (ListOf t, PolyList es t)
                          else throwError $ General (text "List Type mismatch" <+> pretty (PolyList l t))

infer (JProxyCall (JNew c args) t) =
    if c /= (namespace ++ "FunctionalList")
    then
        throwError $ General (text (show c ++ " from JProxyCall: not supported"))
    else
        do ([t1, t2], expr') <- mapAndUnzipM infer args
           d <- getTypeContext
           if ( alphaEq d (ListOf t1) t2)
             then return (t2, JProxyCall (JNew c expr') t2)
             else throwError $ TypeMismatch t1 t2

infer (JProxyCall jmethod t) =
    case jmethod of
        JMethod (NonStatic e) methodname _ _ -> do
            ty <- case methodname of
                "head" -> do (ListOf a, _) <- infer e
                             return a
                "tail" -> do (a, _) <- infer e
                             return a
                _      -> throwError $ General (text (show methodname ++ " from JProxyCall: not supported"))
            d <- getTypeContext
            m <- infer jmethod
            return (ty, JProxyCall (snd m) ty)

infer (RecordIntro fs) =
  do (ts, es') <- mapAndUnzipM infer (map snd fs)
     let fs' = zip (map fst fs) ts
     return (foldl (\acc (l,t) -> And acc (Record [(l,t)])) (Record [head fs']) (tail fs'), RecordIntro (zip (map fst fs) es'))

infer (RecordElim e l) =
  do (t, e') <- infer e
     case Map.lookup l (recordFields t) of
       Just t1 -> return (t1, RecordElim e' l)
       Nothing -> throwError (NotMember l t)

infer (RecordUpdate e fs) =
  do (_, es') <- mapAndUnzipM infer (map snd fs)
     (t, e')  <- infer e
     return (t, RecordUpdate e' (zip (map fst fs) es'))

-- Well, I know the desugaring is too early to happen here...
infer (LetModule (Module m binds) e) =
  do let fs = map bindId binds
     let letrec = Let Rec binds (RecordIntro (map (\f -> (f, Var f)) fs))
     infer $ Let NonRec [Bind m [] [] letrec Nothing] e
infer (ModuleAccess m f) = infer (RecordElim (Var m) f)

-- Type synonyms: type T A1 ... An = t in e
-- First make sure that A1 ... An are distinct.
-- Then rewrite to "type T = \A1. ... An. t in e" and kind-check \A1. ... \An. t.
infer (Type t params rhs e)
  = do checkDupNames params
       typeContext <- getTypeContext
       maybe_kind <- liftIO $ kind typeContext pulledRight
       case maybe_kind of
         Nothing -> throwError $ NotWellKinded pulledRight
         Just k  -> withLocalTVars [(t, (k, NonTerminalType pulledRight))] $ infer e
  where
    pulledRight = pullRight params rhs

infer (Data name cs e) =
    do let names = map constrName cs
       checkDupNames names
       type_ctxt <- getTypeContext

       let t = Datatype name names
           dt = (Star, NonTerminalType t)
           type_ctxt' = Map.insert name dt type_ctxt
           types' = map (map (expandType type_ctxt') . constrParams) cs
           constrBindings = zip names (map (foldTypes t) types')
       withLocalTVars [(name, dt)] (withLocalVars constrBindings (infer e))

infer (Constr c es) =
  do value_ctxt <- getValueContext
     let n = constrName c
     ts <- case Map.lookup n value_ctxt of
                Just t -> return $ unfoldTypes t
                Nothing -> throwError (NotInScope n)
     let (len_expected, len_actual) = (length ts - 1, length es)
     unless (len_expected == len_actual) $
            throwError (General $ text "Constructor" <+> bquotes (text n) <+> text "should have" <+> int len_expected <+> text "arguments, but has been given" <+> int len_actual)
     (_, es') <- mapAndUnzipM (uncurry inferAgainst) (zip es ts)
     return (last ts, Constr (Constructor n ts) es')

infer (Case e alts) =
  do (t, e') <- infer e
     unless (isDatatype t) $
       throwError (General (bquotes (pretty e) <+> text "is of type" <+> bquotes (pretty t) <> comma <+> text "which is not a datatype"))
     value_ctxt <- getValueContext
     d <- getTypeContext
     let ns = (\(Datatype _ xs) -> xs) t
         constrs = map (\(ConstrAlt c _ _) -> c) alts
     constrs' <- mapM (\c -> let n = constrName c
                             in case Map.lookup n value_ctxt of
                                  Just t' -> let ts = unfoldTypes t'
                                             in if alphaEq d (last ts) t
                                                then return (Constructor n ts)
                                                else throwError (TypeMismatch t t')
                                  Nothing -> throwError (NotInScope n))
                 constrs
     let alts' = zipWith substAltConstr alts constrs'

     (ts, es) <- mapAndUnzipM
                 (\(ConstrAlt c ns e2) ->
                      let n = constrName c
                          ts = init $ constrParams c
                      in if length ts == length ns
                         then withLocalVars (zip ns ts) (infer e2)
                         else throwError (General $ text "Constructor" <+> bquotes (text n) <+> text "should have" <+> int (length ts)
                                                                   <+> text "arguments, bus has been given" <+> int (length ns)))
                 alts'

     let resType = head ts
     unless (all (alphaEq d resType) ts) $
            throwError (General $ text "All the alternatives should be of the same type")

     let allConstrs = Set.fromList ns
     let matchedConstrs = Set.fromList $ map constrName constrs
     let unmatchedConstrs = allConstrs Set.\\ matchedConstrs
     unless (Set.null unmatchedConstrs) $
            throwError (General $ text "Pattern match(es) are non-exhaustive." <+> vcat (intersperse space (map (bquotes . text) $ Set.elems unmatchedConstrs)))

     return (resType, Case e' (zipWith substAltExpr alts' es))

  where substAltExpr (ConstrAlt c ns _) = ConstrAlt c ns
        substAltConstr (ConstrAlt _ ns expr) c = ConstrAlt c ns expr

        isDatatype (Datatype _ _) = True
        isDatatype _ = False

-- | "Pull" the type params at the LHS of the equal sign to the right.
-- A (high-level) example:
--   A B t  ->  \A. \B. t
-- Another concrete example:
--   ghci> pullRight ["A", "B"] (JType (JClass "java.lang.Integer"))
--   OpAbs "A" (OpAbs "B" (JType (JClass "java.lang.Integer")))
pullRight :: [Name] -> Type -> Type
pullRight params t = foldr OpAbs t params

inferAgainst :: ReaderExpr -> Type -> Checker (Type, CheckedExpr)
inferAgainst expr expected_ty
  = do (actual_ty, expr') <- infer expr
       d <- getTypeContext
       if alphaEq d actual_ty expected_ty
          then return (actual_ty, expr')
          else throwError (TypeMismatch expected_ty actual_ty)

inferAgainstAnyJClass :: ReaderExpr -> Checker (ClassName, CheckedExpr)
inferAgainstAnyJClass expr
  = do (ty, expr') <- infer expr
       case deThunkOnce ty of
        JType (JPrim "char") -> return ("java.lang.Character", expr')
        JType (JClass c) -> return (c, expr')
        ListOf _         -> return (namespace ++ "FunctionalList", expr')
        _ -> throwError $
             General
             (code (pretty expr) <+> text "has type" <+> code (pretty ty) <> comma <+>
              text "but is expected to be of some Java class")

-- | Check "f A1 ... An (x1:T1) ... (xn:Tn) : t = e"
normalizeBind :: ReaderBind -> Checker (Name, Type, CheckedExpr)
normalizeBind bind
  = do bind' <- checkBindLHS bind
       (bindRhsTy, bindRhs') <- withLocalTVars (map (\a -> (a, (Star, TerminalType))) (bindTargs bind')) $
                                  do expandedBindArgs <- mapM (\(x,t) -> do { d <- getTypeContext; return (x,expandType d t) }) (bindArgs bind')
                                     withLocalVars expandedBindArgs (infer (bindRhs bind'))
       case bindRhsAnnot bind' of
         Nothing -> return ( bindId bind'
                           , wrap Forall (bindTargs bind') (wrap Fun (map snd (bindArgs bind')) bindRhsTy)
                           , wrap BLam (bindTargs bind') (wrap Lam (bindArgs bind') bindRhs'))
         Just annot ->
           withLocalTVars (map (\a -> (a, (Star, TerminalType))) (bindTargs bind')) $
             do checkType annot
                d <- getTypeContext
                if alphaEq d annot bindRhsTy
                   then return (bindId bind'
                               , wrap Forall (bindTargs bind') (wrap Fun (map snd (bindArgs bind')) bindRhsTy)
                               , wrap BLam (bindTargs bind') (wrap Lam (bindArgs bind') bindRhs'))
                   else throwError (TypeMismatch (expandType d annot) bindRhsTy)

-- | Check the LHS to the "=" sign of a bind, i.e., "f A1 ... An (x1:t1) ... (xn:tn)".
-- First make sure the names of type params and those of value params are distinct, respectively.
-- Then check and expand the types of value params.
checkBindLHS :: ReaderBind -> Checker ReaderBind
checkBindLHS Bind{..}
  = do checkDupNames bindTargs
       checkDupNames (map fst bindArgs)
       bindArgs' <- withLocalTVars (map (\a -> (a, (Star, TerminalType))) bindTargs) $
                    -- Restriction: type params have kind *
                    do d <- getTypeContext
                       forM bindArgs (\(x,t) ->
                         do checkType t
                            return (x, expandType d t))
       return Bind { bindArgs = bindArgs', .. }

collectBindIdSigs :: [ReaderBind] -> Checker [(Name, Type)]
collectBindIdSigs
  = mapM (\ Bind{..} ->
            case bindRhsAnnot of
              Nothing    -> throwError MissingRHSAnnot
              Just rhsTy ->
                do d <- getTypeContext
                   let d' = foldr (\a acc -> Map.insert a (Star, TerminalType) acc) d bindTargs
                   return (bindId,
                           wrap Forall bindTargs $
                           wrap Fun [expandType d' ty |  (_,ty) <- bindArgs]
                           rhsTy))

-- | Check that a type has kind *.
checkType :: Type -> Checker ()
checkType t =
  case t of
    JType (JClass c) -> checkClassName c
    JType (JPrim _)  -> prettySorry "TypeCheck.checkType" (pretty t)
    _ -> do
      delta <- getTypeContext
      maybe_kind <- liftIO $ kind delta t
      case maybe_kind of
        Nothing   -> throwError (NotWellKinded t)
        Just Star -> return ()
        Just k    -> throwError (KindMismatch Star k)

unlessIO :: (Monad m, MonadIO m) => IO Bool -> m () -> m ()
unlessIO test do_this
  = do ok <- liftIO test
       unless ok do_this

-- Client library of typeserver API

checkClassName :: ClassName -> Checker ()
checkClassName c
  = do memoized_java_classes <- getMemoizedJavaClasses
       unless (c `Set.member` memoized_java_classes) $
         do h  <- getTypeServer
            res <- liftIO (isJvmType h c)
            if res
               then memoizeJavaClass c
               else throwError (NoSuchClass c)

checkConstruction :: ClassName -> [ClassName] -> Checker ()
checkConstruction c args
  = do h <- getTypeServer
       unlessIO (hasConstructor h c args) $
         throwError (NoSuchConstructor c args)

checkMethodCall :: JCallee ClassName -> MethodName -> [ClassName] -> Checker ClassName
checkMethodCall callee m args
  = do typeserver <- getTypeServer
       res <- liftIO (methodTypeOf typeserver c (m, static_flag) args)
       case res of
         Nothing           -> throwError (NoSuchMethod callee m args)
         Just return_class -> return return_class
    where
       (static_flag, c) = unwrapJCallee callee

checkFieldAccess :: JCallee ClassName -> FieldName -> Checker ClassName
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
srcLitType (Int _)    = JType $ JClass "java.lang.Integer"
srcLitType (String _) = JType $ JClass "java.lang.String"
srcLitType (Bool _)   = JType $ JClass "java.lang.Boolean"
srcLitType (Char _)   = JType $ JClass "java.lang.Character"
srcLitType UnitLit    = Unit

checkDupNames :: [Name] -> Checker ()
checkDupNames names
  = case findOneDup names of
      Nothing   -> return ()
      Just name -> throwError (ConflictingDefinitions name)

-- | Find one instance of duplicate in a list.
findOneDup :: Ord a => [a] -> Maybe a
findOneDup xs = go xs Set.empty
  where
    go []      _ = Nothing
    go (x:xs') s = if Set.member x s
                     then Just x
                     else go xs' (Set.insert x s)

foldTypes :: Type -> [Type] -> Type
foldTypes = foldr Fun

unfoldTypes :: Type -> [Type]
unfoldTypes t@(Datatype _ _) = [t]
unfoldTypes (Fun t t') = t : unfoldTypes t'
