{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, TypeOperators, MultiParamTypeClasses, ConstraintKinds, ScopedTypeVariables #-}

module UnboxTransCFJava where

import qualified Language.Java.Syntax as J
import           Prelude              hiding (init, last)

import           BaseTransCFJava
import           ClosureF
import           Inheritance
import           JavaEDSL
import           MonadLib
import qualified Src                  as S
import           StringPrefixes

data UnboxTranslate m = UT {toUT :: Translate m}

instance (:<) (UnboxTranslate m) (Translate m) where
   up              = up . toUT

instance (:<) (UnboxTranslate m) (UnboxTranslate m) where
   up              = id

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
                     _ -> translateM super e
              ,translateScopeM = \e m -> case e of
                    Type t g ->
                      do n <- get
                         let (x1,x2) = maybe (n + 1,n + 2) (\(i,_) -> (i,n+1)) m -- decide whether we have found the fixpoint closure or not
                         put (x2 + 1)
                         let nextInClosure = g (x2,t)

                         typT1 <- javaType (up this) t
                         let flag = typT1 == objClassTy
                         let accessField = fieldAccess (left $ var (localvarstr ++ show x1)) closureInput
                         let xf = localFinalVar typT1 (varDecl (localvarstr ++ show x2)
                                                               (if flag
                                                                   then accessField
                                                                   else cast typT1 accessField))
                         closureClass <- liftM2 (++) (getPrefix (up this)) (return "Closure")
                         (cvar,t1) <- translateScopeTyp (up this) x1 n [xf] nextInClosure (translateScopeM (up this) nextInClosure Nothing) closureClass

                         return (cvar,var ("Fun" ++  show n),Type t (const t1))
                    _ -> translateScopeM super e m
              ,translateApply =
                    \m1 m2 ->
                      do (s1, j1',Forall (Type _ g)) <- m1
                         (s2,j2,_) <- m2
                         let retTyp = g ()
                         j1 <- genClosureVar (up this) (getArity retTyp) j1'
                         (s3,nje3) <- getS3 (up this) j1 (unwrap j2) retTyp closureType
                         return (s2 ++ s1 ++ s3,nje3,scope2ctyp retTyp)
              ,javaType = \typ ->
                            case typ of
                              CFInt -> return $ J.PrimType J.LongT
                              CFChar -> return $ J.PrimType J.CharT
                              x -> javaType super x
              ,getPrefix = return (namespace ++ "unbox.")
              ,chooseCastBox = \typ ->
                                 case typ of
                                   CFInt -> return (\s e -> localFinalVar (J.PrimType J.LongT)
                                                                            (varDecl s (cast (J.PrimType J.LongT) e))
                                                   ,J.PrimType J.LongT)
                                   CFChar -> return (\s e -> localFinalVar (J.PrimType J.CharT)
                                                                            (varDecl s (cast (J.PrimType J.CharT) e))
                                                   ,J.PrimType J.CharT)

                                   t -> chooseCastBox super t
             }}
