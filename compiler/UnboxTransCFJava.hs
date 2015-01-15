{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

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

getS3L :: MonadState Int m
   => Translate m
   -> J.Exp
   -> J.Exp
   -> TScope Int
   -> J.Type
   -> Type Int
   -> m ([J.BlockStmt],TransJavaExp)
getS3L this j1 j2 retTyp ctempCastTyp argT =
{-
SET := ArgSet(T2, f, J2, J2′ ) OUTSAVE :=OutSave(T3,f,xf,x′f)

OutSave(Long,f,ob,pr)=longpr=f.ires; Objectob;
OutSave(∀∆.T, f, ob, pr) = Function ob = (Function) f.ores; long pr;
OutSave(α,f,ob,pr) = long pr = f.ires; Object ob = f.ores;
ArgSet(Long, f, pr, ob) = f .larg = pr;
ArgSet(∀∆.T, f, pr, ob) = f .oarg = ob;
ArgSet(α, f, pr, ob) = f .larg = pr; f .oarg = ob;

Function f = J1 ;
SET
f .apply ();
OUTSAVE }
-}
     do  (n :: Int) <- get
         put (n+2)
         let f = localvarstr ++ show n
         let xf = localvarstr ++ show (n+1)
         let fexp = left . var $ f
         let fd = localVar ctempCastTyp (varDecl f j1)
         let fs = [assignField (fieldAccExp fexp closureInput) j2]
         typT1 <- javaType (up this) argT
         let assigns = case argT of CFInt -> [assignField (fieldAccExp fexp closureInputL) (cast typT1 j2)]
                                    TVar _ -> [assignField (fieldAccExp fexp closureInputL) (cast typT1 j2),assignField (fieldAccExp fexp closureInputO) j2]
                                    _ -> [assignField (fieldAccExp fexp closureInputO) j2]
         (castBox,typ) <- chooseCastBox this (scope2ctyp retTyp)
         apply <- genApply this fexp retTyp xf typ ctempCastTyp
         let flag = ctempCastTyp == objClassTy
         let fout = fieldAccess fexp (if flag then closureOutputO else closureOutputL)

         res <- genRes (up this) retTyp [castBox xf fout]
         let r = fd : fs ++ apply ++ res
         return (r, var xf)

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
                     {-
                     SET := ArgSave(T1, f, x2, x3) OUT := OutSet(T,J,J′)

                     ArgSave(Long, f, ob, pr) = long pr = f .iarg;
                     ArgSave(∀∆.T, f, ob, pr) = Function ob = (Function) f.oarg;
                     ArgSave(α,f,ob,pr) = long pr = f.iarg; Object ob = f.oarg;
                     OutSet(Long, pr, ob) = lres = pr;
                     OutSet(∀∆.T, pr, ob) = ores = ob;
                     OutSet(α, pr, ob) = lres = pr; ores = ob;

                     SET
                     S;
                     OUT
                     -}
              ,translateScopeM = \e m -> case e of
                    Type t g ->
                      do n <- get
                         let (x1,x2) = maybe (n + 1,n + 2) (\(i,_) -> (i,n+1)) m -- decide whether we have found the fixpoint closure or not
                         put (x2 + 1)
                         let nextInClosure = g (x2,t)
                         typT1 <- javaType (up this) t
                         let flag = typT1 == objClassTy
                         let accessField = fieldAccess (left $ var (localvarstr ++ show x1)) (if flag then closureInputO else closureInputL)
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
                         (s2,j2,t2) <- m2
                         let retTyp = g ()
                         j1 <- genClosureVar (up this) (getArity retTyp) j1'
                         (s3,nje3) <- getS3L (up this) j1 (unwrap j2) retTyp closureType t2
                         return (s2 ++ s1 ++ s3,nje3,scope2ctyp retTyp)
              ,translateScopeTyp =
                           \x1 f initVars _ otherStmts closureClass ->
                           do  b <- genClone (up this)
                               (ostmts,oexpr,t1) <- otherStmts
                               typT1 <- javaType (up this) (scope2ctyp t1)

                               let assigns = case (scope2ctyp t1) of CFInt -> [assign (name [closureOutputL]) (cast typT1 $ unwrap oexpr)]
                                                                     TVar _ -> [assign (name [closureOutputL]) (cast typT1 $ unwrap oexpr),assign (name [closureOutputO]) (unwrap oexpr)]
                                                                     _ -> [assign (name [closureOutputO]) (unwrap oexpr)]
                               let fc = f
                               return ([localClassDecl ("Fun" ++ show fc)
                                 closureClass
                                 (closureBodyGen [memberDecl $ fieldDecl (classTy closureClass)
                                 (varDecl (localvarstr ++ show x1) J.This)]
                                 (initVars ++ ostmts ++ assigns)
                                 fc
                                 b
                                 (classTy closureClass))]
                                 ,t1)
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
