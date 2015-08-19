{-# LANGUAGE FlexibleContexts, TypeOperators, MultiParamTypeClasses, ScopedTypeVariables #-}
{- |
Module      :  BaseTransCFJava
Description :  Basic translation of FCore to Java
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Jeremy <bixuanxbi@gmail.com>, Tomas <tomtau@connect.hku.hk>
Stability   :  stable
Portability :  non-portable (MPTC)

This module implements the basic translation of FCore to Java. For
more information, please refer to the paper on wiki.
-}


module BaseTransCFJava where
-- translation that does not pre-initialize Closures that are ininitalised in apply() methods of other Closures
-- TODO: isolate all hardcoded strings to StringPrefixes (e.g. Fun)

import           ClosureF

import           Data.Char (toLower)
import           Data.List (zip4, elemIndex)
import           Inheritance
import           JavaEDSL
import qualified Language.Java.Syntax as J
import           MonadLib hiding (Alt)
import           Panic
import qualified Src as S
import           StringPrefixes

instance (:<) (Translate m) (Translate m) where
   up = id

type InitVars = [J.BlockStmt]

-- Closure F to Java

createCUB :: t -> [J.TypeDecl] -> J.CompilationUnit
createCUB _ compDef = cu
  where cu = J.CompilationUnit Nothing [] compDef


initClass :: String -> String -> J.Exp -> J.BlockStmt
initClass className tempName expr =
  let ty = classTy className
  in localFinalVar ty (varDecl tempName (if ty == objClassTy
                                            then expr
                                            else cast ty expr))

type Var = Int -- Either normal variable or class name

type TransJavaExp = Either J.Name J.Literal -- either variable or special case: Lit

type TransType = ([J.BlockStmt], TransJavaExp, Type Int)

data Translate m =
  T {translateM :: Expr Int (Var,Type Int) -> m TransType
    ,translateScopeM :: EScope Int (Var,Type Int) -> Maybe (Int,Type Int) -> m ([J.BlockStmt],TransJavaExp,TScope Int)
    ,translateApply :: Bool -> m TransType -> m TransType -> m TransType
    ,translateIf :: m TransType -> m TransType -> m TransType -> m TransType
    ,translateLet :: TransType -> ((Var,Type Int) -> Expr Int (Var,Type Int)) -> m TransType
    ,translateScopeTyp :: Int -> Int -> [J.BlockStmt] -> EScope Int (Var,Type Int) -> m ([J.BlockStmt],TransJavaExp,TScope Int) -> String -> m ([J.BlockStmt],TScope Int)
    ,genApply :: J.Exp -> TScope Int -> String -> J.Type -> J.Type -> m [J.BlockStmt]
    ,genRes :: TScope Int -> [J.BlockStmt] -> m [J.BlockStmt]
    ,applyRetType :: Type Int -> m (Maybe J.Type)
    ,genClone :: m Bool
    ,transAlts :: TransJavaExp -> [Alt Int (Var,Type Int)] -> m TransType
    ,mapAlt :: J.Exp -> J.Name -> Alt Int (Var,Type Int) -> m (J.SwitchBlock,Type Int)
    ,translateDatabind :: DataBind Int -> m (J.BlockStmt,J.BlockStmt)
    ,translateCtr :: String -> Constructor Int -> Integer -> m [J.Decl]
    ,translateCtrParams :: String -> [Type Int] -> [J.Argument] -> m ([J.BlockStmt],TransJavaExp)
    ,withApply :: m Bool
    ,getPrefix :: m String
    ,javaType :: Type Int -> m J.Type
    ,chooseCastBox :: Type Int -> m (String -> J.Exp -> J.BlockStmt,J.Type)
    ,stackMainBody :: Type Int -> m [J.BlockStmt]
    ,genClosureVar :: Bool -> Int -> TransJavaExp -> m J.Exp
    ,transDefs :: Definition Int (Var,Type Int) -> m [J.BlockStmt]
    ,createWrap :: String -> Expr Int (Var,Type Int) -> m (J.CompilationUnit,Type Int)}

-- needed
getTupleClassName :: [a] -> String
getTupleClassName tuple =
  if lengthOfTuple > 50
     then panic "The length of tuple is too long (>50)!"
     else namespace ++ "tuples.Tuple" ++ show lengthOfTuple
  where lengthOfTuple = length tuple

getS3 :: MonadState Int m
      => Translate m
      -> J.Exp
      -> J.Exp
      -> TScope Int
      -> J.Type
      -> m ([J.BlockStmt],TransJavaExp)
getS3 this j1 j2 retTyp ctempCastTyp =
  do (n :: Int) <- get
     put (n+2)
     let f = localvarstr ++ show n
     let xf = localvarstr ++ show (n+1)
     let fexp = left . var $ f
     let fd = localVar ctempCastTyp (varDecl f j1)
     let fs = assignField (fieldAccExp fexp closureInput) j2
     (castBox,typ) <- chooseCastBox this (scope2ctyp retTyp)
     apply <- genApply this fexp retTyp xf typ ctempCastTyp
     let fout = fieldAccess fexp closureOutput
     res <- genRes this retTyp [castBox xf fout]
     let r = fd : fs : apply ++ res
     return (r, var xf)

genIfBody :: MonadState Int m
          => Translate m
          -> m TransType
          -> m TransType
          -> ([J.BlockStmt],TransJavaExp)
          -> Int
          -> m TransType
genIfBody this m2 m3 (s1,j1) n =
  do (s2,j2,t2) <- m2 {-translateM this e2-}
     (s3,j3,_) <- m3 {-translateM this e3-}
     let ifvarname = ifresultstr ++ show n
     aType <- javaType this t2
     let ifresdecl = localVar aType (varDeclNoInit ifvarname)
     let thenPart  = J.StmtBlock $ block (s2 ++ [assign (name [ifvarname]) (unwrap j2)])
     let elsePart  = J.StmtBlock $ block (s3 ++ [assign (name [ifvarname]) (unwrap j3)])
     let ifstmt    = bStmt $ J.IfThenElse (unwrap j1) thenPart elsePart

     return (s1 ++ [ifresdecl,ifstmt],var ifvarname ,t2) -- need to check t2 == t3

initializeRec this types binds = do
  (n :: Int) <- get
  let needed = length types
  put (n + 1 + needed)
  let mfuns xs = forM (binds xs) (translateM this)
  -- let (bindings :: [Var]) = [n + 1.. n + needed]
  let newvars = zip [n + 1 ..] types
  let finalFuns = mfuns newvars
  -- let appliedBody = body newvars
  rBinds <- liftM unzip3 finalFuns
  -- rBody <- translateM this appliedBody
  return (n, newvars, rBinds)


-- needed
assignVar :: Monad m => Translate m -> Type Int -> String -> J.Exp -> m J.BlockStmt
assignVar this t varId e = do aType <- javaType this t
                              return $ localFinalVar aType (varDecl varId e)


pairUp :: [Var] -> [(TransJavaExp, Type Int)] -> [(Var, Type Int)]
pairUp bindings vars = exchanged
  where z = bindings `zip` vars
        exchanged = map (\(a,(_,c)) -> (a,c)) z

-- needed
concatFirst :: ([[a]], [b], [c]) -> ([a], [b], [c])
concatFirst (xs, y, z) = (concat xs, y, z)

-- Needed
getNewVarName :: MonadState Int m => Translate m -> m String
getNewVarName _ = do (n :: Int) <- get
                     put (n + 1)
                     return $ localvarstr ++ show n

isModule :: Expr t e -> Bool
isModule (Module{}) = True
isModule _ = False

trans :: (MonadState Int m, selfType :< Translate m) => Base selfType (Translate m)
trans self =
  let this = up self
  in T {translateM =
          \e ->
            case e of
{-
    (x1 : T1 -> x2) in ∆
    -------------------------- :: cj-var
    Γ |-  x1 : T1 ~> x2 in {}
-}
              -- TODO: propagate names
              Var _ (i,t) -> return ([],var (localvarstr ++ show i),t)
              Lit lit ->
                case lit of
                  (S.Int i)    -> return ([], Right $ J.Int i,     JClass "java.lang.Integer")
                  (S.UnitLit)  -> return ([], Right J.Null, Unit)
                  (S.String s) -> return ([], Right $ J.String s,  JClass "java.lang.String")
                  (S.Bool b)   -> return ([], Right $ J.Boolean b, JClass "java.lang.Boolean")
                  (S.Char c)   -> return ([], Right $ J.Char c,    JClass "java.lang.Character")
              PrimOp e1 op e2 ->
                do (s1,j1,_) <- translateM this e1
                   (s2,j2,_) <- translateM this e2
                   let j1' = unwrap j1
                   let j2' = unwrap j2
                   let (jexpr,typ) = case op of
                                       (S.Arith realOp) -> (J.BinOp j1' realOp j2',JClass "java.lang.Integer")
                                       (S.Compare realOp) -> (J.BinOp j1' realOp j2',JClass "java.lang.Boolean")
                                       (S.Logic realOp) -> (J.BinOp j1' realOp j2',JClass "java.lang.Boolean")
                   newVarName <- getNewVarName this
                   assignExpr <- assignVar this typ newVarName jexpr
                   return (s1 ++ s2 ++ [assignExpr],var newVarName,typ)
              If e1 e2 e3 ->
                translateIf this
                            (translateM this e1)
                            (translateM this e2)
                            (translateM this e3)
              Tuple tuple ->
                case tuple of
                  [t] ->
                    do (s1,j1,t1) <- translateM this t
                       return (s1,j1,TupleType [t1])
                  _ ->
                    do tuple' <- mapM (translateM this) tuple
                       let (statements,exprs,types) = concatFirst (unzip3 tuple')
                       newVarName <- getNewVarName this
                       let c = getTupleClassName tuple
                       let rhs = instCreat (classTyp c) (map unwrap exprs)
                       assignExpr <- assignVar this (JClass c) newVarName rhs
                       return (statements ++ [assignExpr],var newVarName,TupleType types)
              Proj index expr ->
                do ret@(statement,javaExpr,exprType) <- translateM this expr
                   case exprType of
                     TupleType [_] -> return ret
                     TupleType types ->
                       do newVarName <- getNewVarName this
                          let typ = types !! (index - 1)
                          aType <- javaType this typ
                          let rhs = cast aType (fieldAccess (unwrap javaExpr) ("_" ++ show index))
                          assignExpr <- assignVar this typ newVarName rhs
                          return (statement ++ [assignExpr] ,var newVarName ,typ)
                     _ ->
                       panic "BaseTransCFJava.trans: expected tuple type"
{-
    E : ∀α∆.T2 ~> J in S  ∆;T2 ⇓ T3
    -------------------------------- :: cj-tapp
    Γ |- E T1 : T3[T1/α] ~> J in S
-}
              TApp expr t ->
                do n <- get
                   put (n + 1) -- needed to distinguish different type variables
                   (s,je,Forall (Kind f)) <- translateM this expr
                   return (s,je,scope2ctyp (substScope n t (f n)))
{-
    Γ; ∆ |- E : T ~> J in S
    ------------------------------- :: cj-abs
    Γ |- λ∆ . E : ∀∆ . T ~> J in S
-}
              -- TODO: propagate names
              Lam _ se ->
                do (s,je,t) <- translateScopeM this se Nothing
                   return (s,je,Forall t)
              -- TODO: propagate names
              -- Fix _ _ t s ->
              --   do (n :: Int) <- get
              --      put (n + 1)
              --      (expr,je,t') <- translateScopeM this (s (n,t)) (Just (n,t)) -- weird!
              --      return (expr,je,Forall t')
              -- TODO: propagate names
              Let _ expr body ->
                do (s1, j1, t1) <- translateM this expr
                   translateLet this (s1,j1,t1) body
              -- TODO: propagate names
              LetRec _ types xs body ->
                do (n, newvars, (bindStmts,bindExprs,tBinds)) <- initializeRec this types xs
                   (bodyStmts,bodyExpr,tBody) <- translateM this (body newvars)
                   bindtyps <- mapM (javaType this) tBinds
                   typ <- javaType this tBody

                   -- initialize declarations
                   let bindings = map fst newvars
                   let mDecls = map (\(x, btyp) -> memberDecl (fieldDecl btyp (varDeclNoInit (localvarstr ++ show x))))
                                                              (zip bindings bindtyps)

                   -- assign new created closures bindings to variables
                   let letClassName = letTransName ++ show n
                   let letVarName = localvarstr ++ show n
                   let assm = map (\(i,jz) -> assign (name [localvarstr ++ show i]) jz)
                                  (bindings `zip` map unwrap bindExprs)
                   let stasm = concatMap (\(a,b) -> a ++ [b]) (bindStmts `zip` assm)

                   let letClass = localClass letClassName (classBody (mDecls ++ [J.InitDecl False (J.Block stasm)]))

                   -- after let class, some variable initialization
                   let letVarInit = localVar (classTy (letTransName ++ show n))
                                    (varDecl letVarName (instCreat (classTyp letClassName) []))
                   let mVarsInit = map (\(x, btyp) -> (localVar btyp (varDecl x (fieldAccess (varExp letVarName) x))))
                                       (zip (map ((localvarstr ++) . show) bindings) bindtyps)


                   return (letClass : letVarInit : mVarsInit ++ bodyStmts, bodyExpr, tBody)
{-
    Γ |- E1 : ∀(x:T2)∆.T1 ~> J1 in S1
    Γ |- E2 : T2 ~> J2 in S2
    ∆;T1 ⇓ T3     f,xf fresh
    ----------------------------------- :: cj-app
    Γ |- E1 E2 : T3 in S1⊎S2⊎S3
    (S3 := see translateApply)
-}
              App e1 e2 ->
                   translateApply this False
                                  (translateM this e1)
                                  (translateM this e2)

              -- Module
              Module defs -> do defStmts <- transDefs this defs
                                return (defStmts, var tempvarstr, Unit)

              -- InstanceCreation [TypeArgument] ClassType [Argument] (Maybe ClassBody)
              JNew c args ->
                do args' <- mapM (translateM this) args
                   let (statements,exprs,types) = concatFirst $ unzip3 args'
                   let rhs =
                         J.InstanceCreation
                           (map (\y -> case y of
                                         -- JClass "char" -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Character", [])]
                                         JClass x -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident x, [])]
                                         -- CFInt -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Integer", [])]
                                         -- CFInteger -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Integer", [])]
                                         -- CFChar -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Character", [])]
                                         -- CFCharacter -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Character", [])]
                                         _ -> sorry "BaseTransCFJava.trans.JNew: no idea how to do")
                                types)
                           (J.ClassType [(J.Ident c,[])])
                           (map unwrap exprs)
                           Nothing
                   let typ = JClass c
                   newVarName <- getNewVarName this
                   assignExpr <- assignVar this typ newVarName rhs
                   return (statements ++
                           [assignExpr]
                          ,var newVarName
                          ,typ)
              Error ty str -> do
                   (stmt, oexpr, _) <- translateM this str
                   let J.ExpStmt errormsg = classMethodCall (stringExp "Error: ") "concat" [unwrap oexpr]
                   let erro = bStmt $ methodCall ["System","err","println"] [errormsg]
                   let exit = bStmt $ methodCall ["System", "exit"] [integerExp 0]
                   return (stmt ++ [erro, exit], Right J.Null, ty)
              JMethod c m args r ->
                do args' <- mapM (translateM this) args
                   let (statements,exprs,types) = concatFirst $ unzip3 args'
                   let exprs' = map unwrap exprs
                   let refTypes =
                         map (\y -> case y of
                                     JClass x -> J.ClassRefType $ J.ClassType [(J.Ident x, [])]
                                     -- CFInt -> J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Integer", [])]
                                     -- CFInteger -> J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Integer", [])]
                                     -- CFChar -> J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Character", [])]
                                     -- CFCharacter -> J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Character", [])]
                                     Datatype x _ _ -> J.ClassRefType $ J.ClassType [(J.Ident x, [])]
                                     _ -> sorry "BaseTransCFJava.trans.JMethod: no idea how to do")
                             types
                   (classStatement,rhs) <- case c of
                                             Right ce ->
                                               do (classS,classE,_) <- translateM this ce
                                                  return (classS ,J.MethodInv $ J.PrimaryMethodCall (unwrap classE) refTypes (J.Ident m) exprs')
                                             Left cn ->
                                               return ([] ,J.MethodInv $ J.TypeMethodCall (J.Name [J.Ident cn]) refTypes (J.Ident m) exprs')
                   if r /= "java.lang.Void"
                      then do let typ = JClass r
                              newVarName <- getNewVarName this
                              assignExpr <- assignVar this typ newVarName rhs
                              return (statements ++ classStatement ++ [assignExpr] ,var newVarName ,typ)
                      else return (statements ++ classStatement ++ [J.BlockStmt $ J.ExpStmt rhs], Right J.Null, Unit)
              JField c fName r ->
                do (classStatement,classExpr,_) <- case c of
                                                     Right ce ->
                                                       translateM this ce
                                                     Left cn ->
                                                       return ([],Left $ J.Name [J.Ident cn] ,undefined)
                   newVarName <- getNewVarName this
                   aType <- javaType this r
                   let rhs = J.Cast aType $ J.FieldAccess $ J.PrimaryFieldAccess (unwrap classExpr) (J.Ident fName)
                       assignExpr = localFinalVar aType $ varDecl newVarName rhs
                   return (classStatement ++ [assignExpr],var newVarName,r)
              SeqExprs es ->
                do es' <- mapM (translateM this) es
                   let (_,lastExp,lastType) = last es'
                   let statements = concatMap (\(x,_,_) -> x) es'
                   return (statements,lastExp,lastType)
              FVar _ -> sorry "BaseTransCFJava.trans.FVar: no idea how to do"
              Constr (Constructor ctrName' ctrParams) es -> do
                   newVar <- getNewVarName this
                   let realType = last ctrParams
                   let getdt ty = case ty of Kind f -> getdt (f 0)
                                             Type _ f -> getdt (f ())
                                             Body (Datatype nam _ _) -> nam
                                             _ -> sorry "Constr.gentdt: no idea how to do"
                   let nam = case realType of Datatype nam' _ _ -> nam'
                                              Forall t -> getdt t
                                              _ -> sorry "Constr.nam: no idea how to do"
                   let ctrName = nam ++ ctrName'
                   case realType of
                       Datatype{} | null es -> do
                          let inst = localVar (classTy nam) (varDecl newVar $ methodCallExp [map toLower nam,ctrName] [])
                          return ([inst], var newVar, realType)
                       _ -> do
                          let st1 = localVar closureType (varDecl newVar $ methodCallExp [map toLower nam,ctrName] [])
                          let functiontype = case realType of
                                Datatype{} -> Forall $ foldr (\a b -> Type a (\()->b)) (Body realType) (init ctrParams)
                                Forall ts -> Forall $ foldr (\a b -> Type a (\()->b)) ts (init ctrParams)
                                _ -> sorry "Constr.functiontype: no idea how to do"
                          foldl (translateApply this False)
                                (return ([st1], var newVar, functiontype))
                                (map (translateM this) es)
              Case scrut alts ->
                do (scrutStmts, scrutExpr, _) <- translateM this scrut
                   (altsStmts, varName, typ) <- transAlts this scrutExpr alts
                   return (scrutStmts ++ altsStmts, varName, typ)
              Data recflag databinds expr -> do
                (databindclass, databindproxy) <- mapAndUnzipM (translateDatabind this) databinds
                (s', e', t') <- translateM this expr
                case recflag of
                    S.NonRec -> return (databindclass ++ databindproxy ++ s', e', t')
                    S.Rec -> do
                        (n::Int) <- get
                        put (n+2)
                        typ <- javaType this t'
                        let datatypeclass =
                             [localClass ("Datatype" ++ show n)
                                         (classBody ( memberDecl (fieldDecl objClassTy (varDeclNoInit tempvarstr)) :
                                                      map localToMember databindclass ++
                                                      [J.InitDecl False (block $ databindproxy ++ s' ++ [assign (name [tempvarstr]) (unwrap e')])]
                                                    ))
                             ,localVar (classTy ("Datatype" ++ show n))
                                       (varDecl (localvarstr ++ show n)
                                                (instCreat (classTyp ("Datatype" ++ show n)) []))
                             ,localFinalVar typ (varDecl (localvarstr ++ show (n + 1))
                                                         (cast typ (J.ExpName (name [localvarstr ++ show n, tempvarstr]))))]
                        return (datatypeclass,var (localvarstr ++ show (n + 1)),t')
        ,translateScopeM =
          \e m ->
            case e of
{-
    Γ |- E : T ~> J in S
    --------------------------- :: cjd-empty
    Γ;empty |- E : T ~> J in S
-}
              Body t ->
                do (s,je,t1) <- translateM this t
                   return (s,je,Body t1)

{-
    Γα;∆ ⊢ E : T ~> J in S
    ----------------------- :: cjd-bind2
    Γ;α∆ ⊢ E : T ~> J in S
-}
              Kind f ->
                do n <- get
                   put (n + 1) -- needed to distingush type variable
                   (s,je,t1) <- translateScopeM this (f n) m
                   return (s,je,Kind (\a -> substScope n (TVar a) t1))

{-
    Γ(y : T1 -> x2);∆ |- E : T ~> J in S
    FC, x1, x2, f fresh
    ------------------------------------- :: cjd-bind1
    Γ;(y : T1)∆ |- E : T ~> f in S'
    S' := (cvar)
-}
              Type t g ->
                do n <- get
                   let (x1,x2) = maybe (n + 1,n + 2) (\(i,_) -> (i,n+1)) m -- decide whether we have found the fixpoint closure or not
                   put (x2 + 1)
                   let nextInClosure = g (x2,t)

                   typT1 <- javaType this t
                   let flag = typT1 == objClassTy
                   let accessField = fieldAccess (left $ var (localvarstr ++ show x1)) closureInput
                   let xf = localFinalVar typT1 (varDecl (localvarstr ++ show x2)
                                                         (if flag
                                                             then accessField
                                                             else cast typT1 accessField))
                   closureClass <- liftM2 (++) (getPrefix this) (return "Closure")
                   (cvar,t1) <- translateScopeTyp this x1 n [xf] nextInClosure (translateScopeM this nextInClosure Nothing) closureClass

                   let fstmt = [localVar closureType (varDecl (localvarstr ++ show n) (funInstCreate n))]

                   return (cvar ++ fstmt,var (localvarstr ++ show n),Type t (const t1))
       ,translateApply =
          \flag m1 m2 ->
            do (s1, j1',Forall (Type _ g)) <- m1
               (s2,j2,_) <- m2
               let retTyp = g ()
               j1 <- genClosureVar this flag (getArity retTyp) j1'
               (s3,nje3) <- getS3 this j1 (unwrap j2) retTyp closureType
               return (s2 ++ s1 ++ s3,nje3,scope2ctyp retTyp)
       ,translateIf =
          \m1 m2 m3 ->
            do n <- get
               put (n + 1)
               (s1,j1,_) <- m1 {- translateM this e1 -}
               genIfBody this m2 m3 (s1, j1) n
       ,translateLet =
            \(s1,j1,t1) body ->
             do (n :: Int) <- get
                put (n + 1)
                (s2, j2, t2) <- translateM this (body (n,t1))
                let x = localvarstr ++ show n
                jt1 <- javaType this t1
                let xDecl = localVar jt1 (varDecl x $ unwrap j1)
                return (s1 ++ [xDecl] ++ s2, j2, t2)
       ,translateDatabind =
            \(DataBind nam params ctrs) ->
                do n <- get
                   put (length params + n)
                   let tag = memberDecl $ finalFieldDecl intTy (varDeclNoInit datatypetag)
                       classctr = memberDecl $ constructorDecl nam [paramDecl intTy datatypetag] Nothing [initField datatypetag]
                   ctrs' <-  zipWithM (translateCtr this nam) (ctrs [n..length params +n-1]) [1..]
                   let classdef = localClass nam (classBody ([tag, classctr] ++ concat ctrs'))
                       proxy = localVar (classTy nam) (varDecl (map toLower nam) (instCreat (classTyp nam) [integerExp 0]) )
                   return (classdef,proxy)
       ,translateCtr =
            \nam (Constructor ctrname' types') tagnum ->
               case types' of
                 [_] -> do
                          let ctrname = nam ++ ctrname'
                              fielddecl = memberDecl $ fieldDecl (classTy nam) (varDeclNoInit ctrname )
                              singleton = bStmt $ ifthen (eq (varExp ctrname) nullExp)
                                                          (assignE (name [ctrname]) (instCreat (classTyp nam) [integerExp tagnum]))
                              methoddecl = memberDecl $ methodDecl [] (Just $ classTy nam) ctrname []
                                                        (Just $ block [singleton, returnExpS $ varExp ctrname])
                          return [fielddecl, methoddecl]
                 _ -> do
                         let types = init types'
                         javaty <- mapM (javaType this) types
                         let ctrname = nam ++ ctrname'
                             fields = map ((fieldtag ++ ) . show) [1..length types]
                             fieldsdec = map memberDecl $ zipWith  fieldDecl javaty (map varDeclNoInit fields)
                             ctrdecl = memberDecl $ constructorDecl ctrname (zipWith paramDecl javaty fields)
                                                    (Just $ J.SuperInvoke [] [integerExp tagnum])
                                                    (map initField fields)
                             ctrclass = memberDecl $ memberClassDecl ctrname nam (classBody ( fieldsdec ++ [ctrdecl] ))

                         (stmts, oexpr) <- translateCtrParams this ctrname types []
                         let methodbody = Just . block $ stmts ++ [returnExpS (unwrap oexpr)]
                         let methoddecl = memberDecl $ methodDecl [] (Just closureType) ctrname [] methodbody
                         return [ctrclass, methoddecl]
       ,translateCtrParams =
           \classnam types params ->
             case types of
                [] -> do
                    newVarName <- getNewVarName this
                    return ([localFinalVar (classTy classnam) $ varDecl newVarName $ instCreat (classTyp classnam) params], var newVarName)
                hd:tl -> do
                    n <- get
                    put (n+3)
                    typ <- javaType this hd
                    (stmt, oexpr) <- translateCtrParams this classnam tl (params++[varExp (localvarstr ++ show (n+2))])
                    let accessField = fieldAccess (left $ var (localvarstr ++ show (n+1))) closureInput
                    let initVar = localFinalVar typ (varDecl (localvarstr ++ show (n+2))
                                                             (if typ == objClassTy then accessField else cast typ accessField))
                    let classbody = closureBodyGen [memberDecl $ fieldDecl closureType (varDecl (localvarstr ++ show (n+1)) J.This)]
                                                   (initVar:stmt ++ [assign (name [closureOutput]) (unwrap oexpr)])
                                                   n
                                                   False
                                                   closureType
                    let innerclassdef = localClassDecl (closureTransName ++ show n) ifoClass classbody
                    let funInst = localVar closureType (varDecl (localvarstr ++ show n) (funInstCreate n))
                    return ([innerclassdef, funInst], var (localvarstr ++ show n))
       ,transAlts =
           \scrut alts -> do
             let tagaccess = fieldAccess (unwrap scrut) datatypetag --tag access, to be switched
             newVar <- getNewVarName this --record result
             blocks <- mapM (mapAlt this (unwrap scrut) (name [newVar])) alts -- all the blocks
             let (blocks', type') = unzip blocks
             jtype <- javaType this $ head type'
             let newvardecl = localVar jtype $ varDecl newVar nullExp
                 switchstmt = bStmt $ switchStmt tagaccess blocks'
             return ([newvardecl, switchstmt], var newVar, head type')
       ,mapAlt =
           \scrut resultName alt ->
               case alt of
                   Default e1 -> do
                       (altstmt, alte, altt) <- translateM this e1
                       let result = assign resultName (unwrap alte)
                       return (switchBlock Nothing $ altstmt ++ [result], altt)
                   ConstrAlt (Constructor ctrname types) e1 -> do
                       let (Datatype _ _ ctrnames) = last types
                           Just label = elemIndex ctrname ctrnames
                       (altstmt, alte, altt) <- translateM this e1
                       let result = assign resultName (unwrap alte)
                       return (switchBlock
                                  (Just (integerExp $ fromIntegral label + 1))
                                  (altstmt ++ [result]),
                               altt)
       ,translateScopeTyp =
          \x1 f initVars _ otherStmts closureClass ->
            do b <- genClone this
               (ostmts,oexpr,t1) <- otherStmts
               let fc = f
               return ([localClassDecl (closureTransName ++ show fc)
                                       closureClass
                                       (closureBodyGen [memberDecl $ fieldDecl (classTy closureClass)
                                                                               (varDecl (localvarstr ++ show x1) J.This)]
                                                       (initVars ++ ostmts ++ [assign (name [closureOutput]) (unwrap oexpr)])
                                                       fc
                                                       b
                                                       (classTy closureClass))]
                      ,t1)
       ,genApply = \f _ _ _ _ -> return [bStmt $ applyMethodCall f]
       ,genRes = const return
       ,genClosureVar = \_ _ j1 ->  return (unwrap j1)
       ,javaType = \typ -> case typ of
                             (JClass c) -> return $ classTy c
                             (Forall (Kind f)) -> case f 0 of -- TODO: could be a bug
                                                   Body typ' -> javaType this typ'
                                                   _ -> do closureClass <- liftM2 (++) (getPrefix this) (return "Closure")
                                                           return (classTy closureClass)
                             (Forall _) -> do closureClass <- liftM2 (++) (getPrefix this) (return "Closure")
                                              return (classTy closureClass)
                             (TupleType tuple) -> case tuple of
                                                    [t] -> javaType this t
                                                    _ -> return $ classTy $ getTupleClassName tuple
                             -- CFInt -> return $ classTy "java.lang.Integer"
                             -- CFInteger -> return $ classTy "java.lang.Integer"
                             -- CFChar -> return $ classTy "java.lang.Character"
                             -- CFCharacter -> return $ classTy "java.lang.Character"
                             (ListType _) -> return $ classTy (namespace ++ "FunctionalList")
                             (Datatype nam _ _) -> return $ classTy nam
                             _ -> return objClassTy
       ,chooseCastBox = \typ -> case typ of
                                  (JClass c) -> return (initClass c, classTy c)
                                  -- CFInt -> return (initClass "java.lang.Integer", classTy "java.lang.Integer")
                                  -- CFInteger -> return (initClass "java.lang.Integer", classTy "java.lang.Integer")
                                  -- CFChar -> return (initClass "java.lang.Integer", classTy "java.lang.Character")
                                  -- CFCharacter -> return (initClass "java.lang.Integer", classTy "java.lang.Character")
                                  (Forall _) -> do closureClass <- liftM2 (++) (getPrefix this) (return "Closure")
                                                   return (initClass closureClass, classTy closureClass)
                                  (TupleType tuple) -> case tuple of
                                                         [t] -> chooseCastBox this t
                                                         _ -> do let tupleClassName = getTupleClassName tuple
                                                                 return (initClass tupleClassName, classTy tupleClassName)
                                  (ListType _) -> return (initClass (namespace ++ "FunctionalList"), classTy (namespace ++ "FunctionalList"))
                                  (Datatype nam _ _) -> return (initClass nam, classTy nam)
                                  _ -> return (initClass "Object", objClassTy)
       ,getPrefix = return namespace
       ,genClone = return False -- do not generate clone method
       ,withApply = return True
       ,stackMainBody = \_ -> return []
       ,applyRetType = \t -> (case t of
                               JClass "java.lang.Integer" -> return $ Just $ J.PrimType J.IntT
                               JClass "java.lang.Boolean" -> return $ Just $ J.PrimType J.BooleanT
                               -- CFInt -> return $ Just $ J.PrimType J.IntT
                               -- CFChar -> return $ Just $ J.PrimType J.CharT
                               _ -> return $ Just objClassTy)
       ,createWrap =
          \nam expr ->
              do (bs,e,t) <- translateM this expr
                 returnType <- applyRetType this t
                 let flag = isModule expr
                 let javaCode = if isModule expr
                                then wrapperClass flag nam bs returnType moduleMainBody
                                else let returnStmt = [bStmt $ J.Return $ Just (unwrap e)]
                                     in wrapperClass flag nam (bs ++ returnStmt) returnType mainBody
                 return (createCUB this [javaCode], t)

       ,transDefs =
         \ defs -> case defs of
                     Def fname srcTyp expr otherDef ->
                       do (bs,e,t) <- translateM this expr
                          (n :: Int) <- get
                          put (n + 1)
                          otherDefStmts <- transDefs this (otherDef (n, t))
                          let x = localvarstr ++ show n
                          typ <- javaType this t
                          let anno = normalAnno fname x (show srcTyp)
                          let xDecl = localVarWithAnno anno typ (varDecl x $ unwrap e)
                          return (bs ++ [xDecl] ++ otherDefStmts)
                     DefRec names types exprs body -> do
                       (n, newvars, (bindStmts, bindExprs, tBinds)) <- initializeRec this (map snd types) exprs
                       bodyStmts <- transDefs this (body newvars)
                       bindtyps <- mapM (javaType this) tBinds
                       let srcTypes = map fst types

                       -- create annotations for each variable
                       let varnums = map ((localvarstr ++) . show . fst) newvars
                       let annos = map (\(fname, x, srcTyp) -> normalAnno fname x (show srcTyp)) (zip3 names varnums srcTypes)

                       -- assign new created closures bindings to variables
                       let assm = map (\(anno,i,typ,jz) -> localVarWithAnno anno typ (varDecl i jz))
                                      (zip4 annos varnums bindtyps (map unwrap bindExprs))
                       return (concatMap (\(a,b) -> a ++ [b]) (bindStmts `zip` assm) ++ bodyStmts)
                     Null -> return []

       }
