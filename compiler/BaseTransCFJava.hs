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
import qualified Language.Java.Syntax as J

import           ClosureF
import           Inheritance
import           JavaEDSL
import           MonadLib
import           Panic
import qualified Src as S
import           StringPrefixes
import           Data.Char (toLower)
import           Data.List (elemIndex)

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
    ,translateScopeM :: EScope Int (Var, Type Int) -> Maybe (Int,Type Int) -> m ([J.BlockStmt],TransJavaExp,TScope Int)
    ,translateApply :: Bool -> m TransType -> m TransType -> m TransType
    ,translateIf :: m TransType -> m TransType -> m TransType -> m TransType
    ,translateLet :: TransType -> ((Var, Type Int) -> Expr Int (Var, Type Int)) -> m TransType
    ,translateScopeTyp :: Int -> Int -> [J.BlockStmt] -> EScope Int (Var, Type Int) -> m ([J.BlockStmt],TransJavaExp,TScope Int) -> String -> m ([J.BlockStmt],TScope Int)
    ,genApply :: J.Exp -> TScope Int -> String -> J.Type -> J.Type -> m [J.BlockStmt]
    ,genRes :: TScope Int -> [J.BlockStmt] -> m [J.BlockStmt]
    ,applyRetType :: Type Int -> m (Maybe J.Type)
    ,genClone :: m Bool
    ,genTest :: m Bool
    ,withApply :: m Bool
    ,getPrefix :: m String
    ,getBox :: Type Int -> m String
    ,javaType :: Type Int -> m J.Type
    ,chooseCastBox :: Type Int -> m (String -> J.Exp -> J.BlockStmt, J.Type)
    ,stackMainBody :: Type Int -> m [J.BlockStmt]
    ,genClosureVar :: Bool -> Int -> TransJavaExp -> m J.Exp
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
              PolyList l t ->
                do l'  <- mapM (translateM this) l
                   let (statements,exprs, _) = concatFirst (unzip3 l')
                   newVarName <- getNewVarName this
                   let c = namespace ++ "FunctionalList"
                   let rhs = instCreat (classTyp c) (map unwrap exprs)
                   assignExpr <- assignVar this (JClass c) newVarName rhs
                   return (statements ++ [assignExpr], var newVarName, ListType t)
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
              Fix _ _ t s ->
                do (n :: Int) <- get
                   put (n + 1)
                   (expr,je,t') <- translateScopeM this (s (n,t)) (Just (n,t)) -- weird!
                   return (expr,je,Forall t')
              -- TODO: propagate names
              Let _ expr body ->
                do (s1, j1, t1) <- translateM this expr
                   translateLet this (s1,j1,t1) body
              -- TODO: propagate names
              LetRec _ t xs body ->
                do (n :: Int) <- get
                   let needed = length t
                   put (n + 2 + needed)
                   mfuns <- return (\defs -> forM (xs defs) (translateM this))
                   let vars = liftM (map (\(_,b,c) -> (b,c))) (mfuns (zip [n ..] t))
                   let (bindings :: [Var]) = [n + 2 .. n + 1 + needed]
                   newvars <- liftM (pairUp bindings) vars
                   closureClass <- liftM2 (++) (getPrefix this) (return "Closure")
                   let mDecls = map (\x -> memberDecl (fieldDecl (classTy closureClass) (varDeclNoInit (localvarstr ++ show x)))) bindings

                   let finalFuns = mfuns newvars
                   let appliedBody = body newvars
                   let varnums = map fst newvars
                   (bindStmts,bindExprs,_) <- liftM unzip3 finalFuns
                   (bodyStmts,bodyExpr,t') <- translateM this appliedBody
                   typ <- javaType this t'
                   -- assign new created closures bindings to variables
                   let assm = map (\(i,jz) -> assign (name [localvarstr ++ show i]) jz)
                                  (varnums `zip` map unwrap bindExprs)

                   let stasm = concatMap (\(a,b) -> a ++ [b]) (bindStmts `zip` assm) ++ bodyStmts ++ [assign (name [tempvarstr]) (unwrap bodyExpr)]
                   let letClass =
                         [localClass ("Let" ++ show n)
                                      (classBody (memberDecl (fieldDecl objClassTy (varDeclNoInit tempvarstr)) :
                                                  mDecls ++ [J.InitDecl False (J.Block stasm)]))

                         ,localVar (classTy ("Let" ++ show n))
                                   (varDecl (localvarstr ++ show n)
                                            (instCreat (classTyp ("Let" ++ show n)) []))
                         ,localFinalVar typ (varDecl (localvarstr ++ show (n + 1))
                                                     (cast typ (J.ExpName (name [localvarstr ++ show n, tempvarstr]))))]
                   return (letClass,var (localvarstr ++ show (n + 1)),t')
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
              -- InstanceCreation [TypeArgument] ClassType [Argument] (Maybe ClassBody)
              JNew c args ->
                do args' <- mapM (translateM this) args
                   let (statements,exprs,types) = concatFirst $ unzip3 args'
                   let rhs =
                         J.InstanceCreation
                           (map (\y -> case y of
                                         JClass "char" -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Character", [])]
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

              JProxyCall (JNew c args) realType ->
                do args' <- mapM (translateM this) args
                   let (statements,exprs,types) = concatFirst $ unzip3 args'
                   let rhs =
                         J.InstanceCreation
                           (map (\y -> case y of
                                         JClass "char" -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Character", [])]
                                         JClass x -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident x, [])]
                                         -- CFInt -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Integer", [])]
                                         -- CFInteger -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Integer", [])]
                                         -- CFChar -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Character", [])]
                                         -- CFCharacter -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Character", [])]
                                         ListType _ -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident (namespace ++ "FunctionalList"),[])]
                                         TVar _  -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident "java.lang.Object", [])]
                                         _ -> sorry "BaseTransCFJava.trans.JNew: no idea how to do")
                                types)
                           (J.ClassType [(J.Ident c,[])])
                           (map unwrap exprs)
                           Nothing
                   -- let typ = JClass c
                   newVarName <- getNewVarName this
                   assignExpr <- assignVar this realType newVarName rhs
                   return (statements ++
                           [assignExpr]
                          ,var newVarName
                          ,realType)

              JProxyCall (JMethod c m args r) realType ->
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
                                     _ -> sorry "BaseTransCFJava.trans.JMethod: no idea how to do")
                             types
                   realJavaType <- javaType this realType
                   (classStatement,rhs) <- case c of
                                             Right ce ->
                                               do (classS,classE,_) <- translateM this ce
                                                  if realJavaType == objClassTy
                                                    then return (classS ,J.MethodInv $ J.PrimaryMethodCall (unwrap classE) refTypes (J.Ident m) exprs')
                                                    else return (classS ,cast realJavaType (J.MethodInv $ J.PrimaryMethodCall (unwrap classE) refTypes (J.Ident m) exprs'))
                                             Left cn ->
                                               if realJavaType == objClassTy
                                                 then return ([] ,J.MethodInv $ J.TypeMethodCall (J.Name [J.Ident cn]) refTypes (J.Ident m) exprs')
                                                 else return ([] ,cast realJavaType (J.MethodInv $ J.TypeMethodCall (J.Name [J.Ident cn]) refTypes (J.Ident m) exprs'))
                   -- let typ = JClass r
                   if r /= "java.lang.Void"
                      then do newVarName <- getNewVarName this
                              assignExpr <- assignVar this realType newVarName rhs
                              return (statements ++ classStatement ++ [assignExpr] ,var newVarName ,realType)
                      else return (statements ++ classStatement ++ [J.BlockStmt $ J.ExpStmt rhs], Right J.Null, realType)
              JProxyCall _ _ -> sorry "JProxyCall: Not supported"
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
                   let typ = JClass r
                   aType <- javaType this typ
                   let rhs = J.Cast aType $ J.FieldAccess $ J.PrimaryFieldAccess (unwrap classExpr) (J.Ident fName)
                   assignExpr <- assignVar this typ newVarName rhs
                   return (classStatement ++ [assignExpr],var newVarName,typ)
              SeqExprs es ->
                do es' <- mapM (translateM this) es
                   let (_,lastExp,lastType) = last es'
                   let statements = concatMap (\(x,_,_) -> x) es'
                   return (statements,lastExp,lastType)
              FVar _ -> sorry "BaseTransCFJava.trans.FVar: no idea how to do"
              Constr (Constructor ctrName' ctrParams) es ->
                do let Datatype nam _ _ = last ctrParams
                       ctrName = nam ++ ctrName'
                   es' <- mapM (translateM this) es
                   newVarName <- getNewVarName this
                   let (stmts, oexpr, _) =  concatFirst $ unzip3 es'
                   let inst = case es' of
                                [] -> localVar (classTy nam) (varDecl newVarName $ methodCallExp [map toLower nam,ctrName] [])
                                _  -> localVar (classTy nam) (varDecl newVarName $ methodCallExp [map toLower nam,ctrName] (map unwrap oexpr))
                   return (stmts ++ [inst], var newVarName, last ctrParams)
              Data nam params ctrs e ->
                do  let tag = memberDecl $ finalFieldDecl intTy (varDeclNoInit datatypetag)
                        classctr = memberDecl $ constructorDecl nam [paramDecl intTy datatypetag] Nothing [initField datatypetag]
                    ctrs' <-  zipWithM translateCtr ctrs [1..]
                    let classdef = localClass nam (classBody ([tag, classctr] ++ concat ctrs'))
                        proxy = localVar (classTy nam) (varDecl (map toLower nam) (instCreat (classTyp nam) [integerExp 0]) )
                    (s',e',t') <- translateM this e
                    return([classdef,proxy] ++ s',e', t')
                where translateCtr (Constructor ctrname' []) tagnum = do
                          let ctrname = nam ++ ctrname'
                              fielddecl = memberDecl $ fieldDecl (classTy nam) (varDeclNoInit ctrname )
                              singleton = bStmt $ ifthen (eq (varExp ctrname) nullExp)
                                                          (assignE (name [ctrname]) (instCreat (classTyp nam) [integerExp tagnum]))
                              methoddecl = memberDecl $ methodDecl [] (Just $ classTy nam) ctrname []
                                                        (Just $ block [singleton, returnExpS $ varExp ctrname])
                          return [fielddecl, methoddecl]
                      translateCtr (Constructor ctrname' types) tagnum = do
                          javaty <- mapM (javaType this) types
                          let ctrname = nam ++ ctrname'
                              fields = map ((fieldtag ++ ) . show) [1..length types]
                              fieldsdec = map memberDecl $ zipWith  fieldDecl javaty (map varDeclNoInit fields)
                              ctrdecl = memberDecl $ constructorDecl ctrname (zipWith paramDecl javaty fields)
                                                     (Just $ J.SuperInvoke [] [integerExp tagnum])
                                                     (map initField fields)
                              ctrclass = memberDecl $ memberClassDecl ctrname nam (classBody ( fieldsdec ++ [ctrdecl] ))
                              methoddecl = memberDecl $ methodDecl [] (Just $ classTy nam) ctrname (zipWith paramDecl javaty fields)
                                                        (returnExp (cast (classTy nam) (instCreat (classTyp ctrname) (map varExp fields))))
                          return [ctrclass, methoddecl]
              Case e alts ->
                do (s',e', _) <- translateM this e
                   let tagaccess = fieldAccess (unwrap e') datatypetag
                   newVarName <- getNewVarName this
                   blocks <- mapM (mapAlt (unwrap e') (name [newVarName])) alts
                   let (blocks', type') = unzip blocks
                       defaultBlock = switchBlock Nothing [bStmt $ throwRuntimeException "pattern match fail"]
                   jtype <- javaType this $ head type'
                   let newvardecl = localVar jtype $ varDeclNoInit newVarName
                       switchstmt = bStmt $ switchStmt tagaccess (blocks' ++ [defaultBlock])
                   return (s' ++ [newvardecl, switchstmt], var newVarName, head type')
                where mapAlt e' resultName (ConstrAlt (Constructor ctrname types) _ f) = do
                        let (Datatype nam tvars ctrnames) = last types
                            Just label = elemIndex ctrname ctrnames
                            len = length types - 1
                            objtype = case len of
                                         0  -> classTy nam
                                         _  -> classTy (nam ++ "." ++ nam ++ ctrname)
                        (n :: Int) <- get
                        put (n+len+1)
                        let varname = localvarstr ++ show n
                        jtypes <- mapM (javaType this) (init types)
                        tvarjtypes <- mapM (javaType this) tvars
                        let objdecl = case len of
                                         0 -> localVar objtype $ varDecl varname e'
                                         _ -> localVar objtype $ varDecl varname (cast objtype e')
                            vardecls = zipWith (\i ty -> let fieldaccess = fieldAccess (varExp varname) (fieldtag ++ show i)
                                                         in  if (ty == objClassTy || not (elem ty tvarjtypes))
                                                              then localVar ty $ varDecl (localvarstr ++ show (n+i)) fieldaccess
                                                              else localVar ty $ varDecl (localvarstr ++ show (n+i)) (cast ty fieldaccess) )
                                                [1..len]
                                                jtypes
                        (altstmt, alte, altt) <- translateM this $ f (zip [(n+1) ..] (init types))
                        let result = assign resultName (unwrap alte)
                        return (switchBlock (Just (integerExp $ fromIntegral label + 1)) $
                                  [objdecl] ++ vardecls ++ altstmt ++ [result],
                                altt)
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
                   -- put (n + 1) -- needed?
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
       ,translateScopeTyp =
          \x1 f initVars _ otherStmts closureClass ->
            do b <- genClone this
               (ostmts,oexpr,t1) <- otherStmts
               let fc = f
               return ([localClassDecl ("Fun" ++ show fc)
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
       ,genTest = return False -- do not generate test method
       ,withApply = return True
       ,getBox = \_ -> return ""
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
               let returnStmt = [bStmt $ J.Return $ Just (unwrap e)]
               isTest <- genTest this
               let mainDecl = wrapperClass nam (bs ++ returnStmt) returnType mainBody [] Nothing isTest
               return (createCUB this [mainDecl],t)}
