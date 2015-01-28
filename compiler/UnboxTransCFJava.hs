{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, TypeOperators, MultiParamTypeClasses, ConstraintKinds, ScopedTypeVariables #-}

module UnboxTransCFJava where

import           Prelude hiding (init, last)
import qualified Language.Java.Syntax as J

import           BaseTransCFJava
import           ClosureF
import           Inheritance
import           JavaEDSL
import           JavaUtils
import           MonadLib
import           Panic
import qualified Src as S
import           StringPrefixes

data UnboxTranslate m = UT {toUT :: Translate m}

instance (:<) (UnboxTranslate m) (Translate m) where
   up              = up . toUT

instance (:<) (UnboxTranslate m) (UnboxTranslate m) where
   up              = id

getClassType :: (Monad m) => Translate m -> Type t -> Type t -> m ClassName
getClassType this t1 t2 = do
  closureClass <- liftM2 (++) (getPrefix this) (return "Closure")
  case (t1, t2) of
   (CFInt, CFInt) -> return (closureClass ++ "IntInt")
   (CFInt, _) -> return (closureClass ++ "IntBox")
   (_, CFInt) -> return (closureClass ++ "BoxInt")
   (_, _) -> return (closureClass ++ "BoxBox")

boxType :: (Monad m) => Translate m -> m ClassName
boxType this = do
  closureClass <- liftM2 (++) (getPrefix this) (return "Closure")
  return (closureClass ++ "BoxBox")


getFunType :: Type t -> (Type t, Type t)
getFunType (Forall (Type b f)) = (b, scope2ctyp (f ()))
getFunType _ = panic "UnboxTranslate.getFunType: expect Forall construcr"

--TODO: generate wrappers when types T1~T2
-- wrap :: J.Exp -> Type t -> Type t -> ([J.BlockStmt], J.Exp)
-- wrap e t1 t2 = ([], e)

transUnbox :: (MonadState Int m, selfType :< UnboxTranslate m, selfType :< Translate m) => Mixin selfType (Translate m) (UnboxTranslate m)
transUnbox this super =
  UT {toUT =
        super {translateM =
                 \e ->
                   case e of
                     TApp expr CFInt ->
                       do n <- get
                          (s,je,Forall (Kind f)) <- translateM (up this) expr
                          return (s,je,scope2ctyp (substScope n CFInteger (f n)))
                     Lit lit ->
                       case lit of
                         (S.Int i) -> return ([] ,Right $ J.Lit $ J.Int i ,CFInt)
                         (S.Char i) -> return ([] ,Right $ J.Lit $ J.Char i ,CFChar)
                         _ -> translateM super e
                     PrimOp e1 op e2 ->
                       do (s1,j1,_) <- translateM (up this) e1
                          (s2,j2,_) <- translateM (up this) e2
                          let j1' = unwrap j1
                          let j2' = unwrap j2
                          let (je,typ) =
                                case op of
                                  S.Arith realOp -> (J.BinOp j1' realOp j2',CFInt)
                                  S.Compare realOp -> (J.BinOp j1' realOp j2',JClass "boolean")
                                  S.Logic realOp -> (J.BinOp j1' realOp j2',JClass "boolean")
                          newVarName <- getNewVarName super
                          aType <- javaType (up this) typ
                          return (s1 ++ s2 ++ [localVar aType (varDecl newVarName je)],var newVarName,typ)
                     LetRec t xs body ->
                       do (n :: Int) <- get
                          let needed = length (xs (zip [n ..] t))
                          put (n + 2 + needed)
                          mfuns <- return (\defs -> forM (xs defs) (translateM (up this)))
                          let vars = liftM (map (\(_,b,c) -> (b,c))) (mfuns (zip [n ..] t))
                          let (bindings :: [Var]) = [n + 2 .. n + 1 + needed]
                          newvars <- liftM (pairUp bindings) vars
                          cNames <- mapM (\(_, typ) ->
                                           let (t1, t2) = getFunType typ
                                           in getClassType (up this) t1 t2)
                                    newvars
                          let varTypes = zip bindings cNames
                          let mDecls = map (\(x, typ) ->
                                              memberDecl (fieldDecl (classTy typ)
                                                                    (varDeclNoInit (localvarstr ++ show x))))
                                           varTypes
                          let finalFuns = mfuns newvars
                          let appliedBody = body newvars
                          let varnums = map fst newvars
                          (bindStmts,bindExprs,_) <- liftM unzip3 finalFuns
                          (bodyStmts,bodyExpr,t') <- translateM (up this) appliedBody
                          typ <- javaType (up this) t'
                          -- assign new created closures bindings to variables
                          let assm = map (\(i,jz) -> assign (name [localvarstr ++ show i]) jz)
                                         (varnums `zip` (map unwrap bindExprs))
                          let stasm = concatMap (\(a,b) -> a ++ [b]) (bindStmts `zip` assm) ++ bodyStmts ++ [assign (name ["out"]) (left bodyExpr)]
                          let letClass =
                                [localClass ("Let" ++ show n)
                                             (classBody (memberDecl (fieldDecl objClassTy (varDeclNoInit "out")) :
                                                         mDecls ++ [J.InitDecl False (J.Block stasm)]))
                                ,localVar (classTy ("Let" ++ show n))
                                          (varDecl (localvarstr ++ show n)
                                                   (instCreat (classTyp ("Let" ++ show n)) []))
                                ,localVar typ (varDecl (localvarstr ++ show (n + 1))
                                                       (cast typ (J.ExpName (name [localvarstr ++ show n, "out"]))))]
                          return (letClass,var (localvarstr ++ show (n + 1)),t')
                     _ -> translateM super e
              ,translateScopeM = \e m -> case e of
                   Type t g ->
                     do  n <- get
                         let (v,n')  = maybe (n+1,n+2) (\(i,_) -> (i,n+1)) m -- decide whether we have found the fixpoint closure or not
                         put (n' + 1)
                         let nextInClosure = g (n',t)

                         aType <- javaType (up this) t
                         let accessField = fieldAccess (left $ var (localvarstr ++ show v)) closureInput
                         let js = localFinalVar aType (varDecl (localvarstr ++ show n') (cast aType accessField))

                         let ostmts = translateScopeM (up this) nextInClosure Nothing
                         (_,_,tt) <- ostmts
                         cName <- getClassType (up this) t (scope2ctyp tt)

                         (cvar,t1) <- translateScopeTyp (up this) v n [js] nextInClosure ostmts cName
                         return (cvar,var (localvarstr ++ show n), Type t (\_ -> t1) )

                   _ -> translateScopeM super e m
              ,translateApply = \flag m1 m2 ->
                    do  (s1,j1', Forall (Type t1 g)) <- m1
                        (s2,j2,_) <- m2
                        -- let (wrapS, jS) = wrap j2 t1 t2
                        let retTyp = g ()
                        j1 <- genClosureVar (up this) flag (getArity retTyp) j1'
                        cName <- getClassType (up this) t1 (scope2ctyp retTyp)
                        (s3, nje3) <- getS3 (up this) j1 (unwrap j2) retTyp (classTy cName)
                        return (s1 ++ s2 ++ s3, nje3, scope2ctyp retTyp)
              ,javaType = \typ ->
                            case typ of
                              CFInt -> return $ J.PrimType J.IntT
                              CFChar -> return $ J.PrimType J.CharT
                              (Forall (Type t1 f)) -> case f () of
                                                        Body t2 -> liftM classTy (getClassType (up this) t1 t2)
                                                        _ -> liftM classTy (getClassType (up this) t1 CFInteger)
                              (Forall (Kind _)) -> liftM classTy (boxType (up this))
                              x -> javaType super x
              ,getPrefix = return (namespace ++ "unbox.")
              ,chooseCastBox = \typ ->
                                 case typ of
                                   CFInt -> return (\s e -> localFinalVar (J.PrimType J.IntT)
                                                                            (varDecl s (cast (J.PrimType J.IntT) e))
                                                   ,J.PrimType J.IntT)
                                   CFChar -> return (\s e -> localFinalVar (J.PrimType J.CharT)
                                                                            (varDecl s (cast (J.PrimType J.CharT) e))
                                                   ,J.PrimType J.CharT)
                                   (Forall (Type t1 f)) ->
                                     case f () of
                                       (Body t2) -> do
                                         typ1 <- getClassType (up this) t1 t2
                                         return (initClass typ1,classTy typ1)
                                       _ -> do
                                         typ1 <- getClassType (up this) t1 CFInteger
                                         return (initClass typ1,classTy typ1)
                                   t -> chooseCastBox super t
             }}
