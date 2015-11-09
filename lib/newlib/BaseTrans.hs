{-# LANGUAGE FlexibleContexts, TypeOperators, MultiParamTypeClasses, ScopedTypeVariables, ViewPatterns #-}

module BaseTrans where

import           Control.Monad.Reader
import           Data.List (zip4, unzip4)
import qualified Language.Java.Syntax as J
import           Lens.Micro
import           Unbound.Generics.LocallyNameless

import           Inheritance
import           JavaLang
import           Panic
import qualified Src as S
import           StringPrefixes
import           Syntax
import           TransEnvironment

-- import Debug.Trace
-- import PrettyPrint

instance (:<) (Translate m) (Translate m) where
   up = id

type TransJavaExp = Either J.Name J.Literal

type TransType = ([J.BlockStmt], TransJavaExp, Expr)

data Translate m =
       T
         { translateM :: Expr -> m TransType
         , translateScopeM :: Bind Tele Expr -> Maybe TmName -> m ([J.BlockStmt], TransJavaExp, Bind Tele Expr)
         , translateIf :: m TransType -> m TransType -> m TransType -> m TransType
         , translateApply :: Expr -> Expr -> m TransType
         , translateScopeTy :: Tele -> TransType -> Maybe TmName -> m ([J.BlockStmt], TransJavaExp)
         , createWrap :: String -> Expr -> m J.CompilationUnit
         , transDefs :: Definition -> m [J.BlockStmt]
         }

createWrap' this str expr = do
  (bs, e, t) <- translateM this expr
  let returnStmt = [bStmt $ J.Return $ Just (unwrap e)]
  let returnType = applyRetType t
  let (javaCode, package) = case expr of (Module p _) -> (wrapperClass True str bs returnType moduleMainBody, p)
                                         _ -> (wrapperClass False str (bs ++ returnStmt) returnType mainBody, Nothing)
  return $ maybe (createCUB Nothing [javaCode])
                 (\pname -> createCUB  (Just (J.PackageDecl (name [pname]))) [javaCode]) package

createCUB :: Maybe J.PackageDecl -> [J.TypeDecl] -> J.CompilationUnit
createCUB package compDef = cu
  where
    cu = J.CompilationUnit package [] compDef

applyRetType t =
  case t of
    JClass "java.lang.Integer" -> Just $ J.PrimType J.IntT
    JClass "java.lang.Boolean" -> Just $ J.PrimType J.BooleanT
    _                          -> Just objClassTy

trans :: (Fresh m, MonadReader Context m, selfType :< Translate m) => Base selfType (Translate m)
trans self =
  let this = up self
  in T
    { translateM  = translateM' this
    , translateIf = translateIf' this
    , translateScopeM = translateScopeM' this
    , translateScopeTy = translateScopeTy' this
    , translateApply = translateApply' this
    , createWrap = createWrap' this
    , transDefs = transDefs' this
    }


translateM' this e =
  case e of
    Var n -> do
      t <- lookupTy n
      return ([], var (show n), t)
    Lit lit ->
      case lit of
        (S.Int i)    -> return ([], Right $ J.Int i, JClass "java.lang.Integer")
        (S.UnitLit)  -> return ([], Right J.Null, Unit)
        (S.String s) -> return ([], Right $ J.String s, JClass "java.lang.String")
        (S.Bool b)   -> return ([], Right $ J.Boolean b, JClass "java.lang.Boolean")
        (S.Char c)   -> return ([], Right $ J.Char c, JClass "java.lang.Character")
    PrimOp op e1 e2 -> do
      (s1, j1, _) <- translateM this e1
      (s2, j2, _) <- translateM this e2
      let j1' = unwrap j1
      let j2' = unwrap j2
      let (je, t) =
            case op of
              (S.Arith realOp)   -> (J.BinOp j1' realOp j2', JClass "java.lang.Integer")
              (S.Compare realOp) -> (J.BinOp j1' realOp j2', JClass "java.lang.Boolean")
              (S.Logic realOp)   -> (J.BinOp j1' realOp j2', JClass "java.lang.Boolean")
      newName <- fresh (string2Name "prim" :: TmName)
      let assignExpr = localFinalVar (javaType t) (varDecl (show newName) je)
      return (s1 ++ s2 ++ [assignExpr], var (show newName), t)

    Lam bnd -> do
      (s, je, t) <- translateScopeM this bnd Nothing
      return (s, je, Pi t)

    App e1 e2 -> translateApply this e1 e2

    -- TODO: generalize?
    Mu bnd -> do
      ((x, Embed t), e) <- unbind bnd
      case e of
        Lam bnd -> do
          (s, je, _) <- extendCtx (mkTele [(x, t)]) (translateScopeM this bnd (Just x))
          return (s, je, t)

        _ -> panic "Expected a lambda abstraction after mu"

    Let bnd -> do
      ((x, Embed e), b) <- unbind bnd
      let x' = show x
      (s1, j1, t1) <- translateM this e
      -- types do need to generate code
      if (aeq t1 estar)
        then translateM this (subst x e b)
        else do
          let jt1 = javaType t1
          let xDecl = localVar jt1 (varDecl x' $ unwrap j1)
          (s2, j2, t2) <- extendCtx (mkTele [(x, t1)]) (translateM this b)
          return (s1 ++ [xDecl] ++ s2, j2, t2)

    LetRec bnd -> do
      (unrec -> bnds', body) <- unbind bnd
      let bnds = unzip3 . map (\(n, Embed t, Embed def) -> (n, t, def)) $ bnds'
      let names = bnds ^. _1
      let types = bnds ^. _2
      let defs = bnds ^. _3
      let env = mkTele (zip names types)

      (bindStmts, bindExprs, bindTypes) <- unzip3 <$> mapM (\d -> extendCtx env (translateM this d)) defs
      (bodyStmts, bodyExpr, bodyType) <- extendCtx env (translateM this body)

      -- initialize declarations
      let bindJTypes = map javaType bindTypes
      let mDecls = map (\(n, btyp) -> memberDecl (fieldDecl btyp (varDeclNoInit (show n))))
                     (zip names bindJTypes)

      -- assign new created closures bindings to variables
      letClassName <- show <$> fresh (s2n letTransName)
      letVarName <- show <$> fresh (s2n "let")
      let assm = map (\(i, jz) -> bsAssign (name [show i]) jz) (names `zip` map unwrap bindExprs)
      let stasm = concatMap (\(a, b) -> a ++ [b]) (bindStmts `zip` assm)
      let letClass = localClass letClassName
                       (classBody (mDecls ++ [J.InitDecl False (J.Block stasm)]))

      -- after let class, some variable initialization
      let letVarInit = localVar (classTy letClassName)
                         (varDecl letVarName (instCreat (classTyp letClassName) []))
      let mVarsInit = map
                        (\(x, btyp) ->
                           (localVar btyp (varDecl x (fieldAccess (varExp letVarName) x))))
                        (zip (map show names) bindJTypes)


      return (letClass : letVarInit : mVarsInit ++ bodyStmts, bodyExpr, bodyType)


    If g e1 e2 -> translateIf this (translateM this g) (translateM this e1) (translateM this e2)

    F t e -> do
      (s, v, _) <- translateM this e
      return (s, v, t)

    U e -> do
      (s, v, t1) <- translateM this e
      let t2 = oneStep t1
      return (s, v, t2)

    Star -> do
      (js, v) <- createTypeHouse "star"
      return (js, v, Star)

    Pi _ -> do
      (js, v) <- createTypeHouse "pi"
      return (js, v, Star)

    JClass className -> do
      (stmts, jvar) <- createTypeHouse "jclass"
      return (stmts, jvar, Star)

    Unit -> do
      (stmts, jvar) <- createTypeHouse "unit"
      return (stmts, jvar, Star)

    Tuple tuple -> do
      case tuple of
        [t] -> do
          (s1, j1, t1) <- translateM this t
          return (s1, j1, Product [t1])
        _ -> do
          tuple' <- mapM (translateM this) tuple
          let (statements, exprs, types) = unzip3 tuple' & _1 %~ concat
          newVarName <- fresh (string2Name "tuple" :: TmName)
          let c = getTupleClassName tuple
          let rhs = instCreat (classTyp c) (map unwrap exprs)
          let assignExpr = localFinalVar (javaType $ JClass c) (varDecl (show newVarName) rhs)
          return (statements ++ [assignExpr], var (show newVarName), Product types)

    Proj index expr -> do
      ret@(statement, javaExpr, exprType) <- translateM this expr
      case exprType of
        Product [_] -> return ret
        Product types -> do
          newVarName <- fresh (string2Name "proj" :: TmName)
          let typ = types !! (index - 1)
          let aType = javaType typ
          let rhs = cast aType (fieldAccess (unwrap javaExpr) ("_" ++ show index))
          let assignExpr = localFinalVar (javaType typ) (varDecl (show newVarName) rhs)
          return (statement ++ [assignExpr], var $ show newVarName, typ)
        _ -> panic "BaseTransCFJava.trans: expected tuple type"

    JNew classname args -> do
      args' <- mapM (translateM this) args
      let (statements, exprs, types) = unzip3 args' & _1 %~ concat
      let rhs = J.InstanceCreation
                  (map
                     (\y -> case y of
                        JClass x -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident x, [])]
                        _        -> sorry "BaseTransCFJava.trans.JNew: no idea how to do")
                     types)
                  (J.ClassType [(J.Ident classname, [])])
                  (map unwrap exprs)
                  Nothing
      let typ = JClass classname
      newVarName <- fresh (string2Name "jnew" :: TmName)
      let assignExpr = localFinalVar (javaType typ) (varDecl (show newVarName) rhs)
      return (statements ++ [assignExpr], var (show newVarName), typ)

    JMethod receiver mname args retClassname -> do
      args' <- mapM (translateM this) args
      let (statements, exprs, types) = unzip3 args' & _1 %~ concat
      let exprs' = map unwrap exprs
      let refTypes =
            map
              (\y -> case y of
                 JClass x -> J.ClassRefType $ J.ClassType [(J.Ident x, [])]
                 _        -> sorry "BaseTransCFJava.trans.JMethod: no idea how to do")
              types
      (classStatement, rhs) <- case receiver of
                                 Right ce ->
                                   do
                                     (classS, classE, _) <- translateM this ce
                                     return
                                       (classS, J.MethodInv $ J.PrimaryMethodCall
                                                                (unwrap classE)
                                                                refTypes
                                                                (J.Ident mname)
                                                                exprs')
                                 Left cn ->
                                   return
                                     ([], J.MethodInv $ J.TypeMethodCall
                                                          (J.Name [J.Ident cn])
                                                          refTypes
                                                          (J.Ident mname)
                                                          exprs')
      if retClassname /= "java.lang.Void"
        then do
          let typ = JClass retClassname
          newVarName <- fresh (string2Name "jmcall" :: TmName)
          let assignExpr = localFinalVar (javaType typ) (varDecl (show newVarName) rhs)
          return (statements ++ classStatement ++ [assignExpr], var (show newVarName), typ)
        else return
               (statements ++ classStatement ++ [J.BlockStmt $ J.ExpStmt rhs], Right J.Null, Unit)

    JField receiver fname retClass -> do
      (classStatement, classExpr, _) <- case receiver of
                                          Right ce ->
                                            translateM this ce
                                          Left cn ->
                                            return ([], Left $ J.Name [J.Ident cn], undefined)
      newVarName <- fresh (string2Name "jfcall" :: TmName)
      let aType = javaType retClass
      let rhs = J.Cast aType $ J.FieldAccess $ J.PrimaryFieldAccess (unwrap classExpr)
                                                 (J.Ident fname)
          assignExpr = localFinalVar aType $ varDecl (show newVarName) rhs
      return (classStatement ++ [assignExpr], var (show newVarName), retClass)

    Seq es -> do
      es' <- mapM (translateM this) es
      let (_, lastExp, lastType) = last es'
      let statements = concatMap (\(x, _, _) -> x) es'
      return (statements, lastExp, lastType)

    Module _ defs -> do
      defStmts <- transDefs this defs
      return (defStmts, var tempvarstr, Unit)

translateIf' this m1 m2 m3 = do
  (s1, j1, _) <- m1
  (s2, j2, t2) <- m2
  (s3, j3, t3) <- m3
  ifname <- fresh (string2Name ifresultstr :: TmName)
  let ifvarname = show ifname
  let jtype = javaType t2

  let ifresdecl = localVar jtype (varDeclNoInit ifvarname)
  let thenPart  = J.StmtBlock $ block (s2 ++ [bsAssign (name [ifvarname]) (unwrap j2)])
  let elsePart  = J.StmtBlock $ block (s3 ++ [bsAssign (name [ifvarname]) (unwrap j3)])
  let ifstmt    = bStmt $ J.IfThenElse (unwrap j1) thenPart elsePart

  return (s1 ++ [ifresdecl,ifstmt],var ifvarname ,t2)


translateScopeM' this bnd n = do
  (delta, m) <- unbind bnd
  (bodyBS, bodyV, bodyT) <- extendCtx delta (translateM this m)

  (closureBS, cx1) <- translateScopeTy this delta (bodyBS, bodyV, bodyT) n

  return (closureBS, cx1, bind delta bodyT)

{-

\(x1 : t1)(..) . e

==>

class Fcx1 extends Closure {

  Closure cx1 = this;

  public void apply() {
    final |t1| x1 = cx1.arg;
    <...>
  }
}
Closure cx1 = new Fcx1();

-}
translateScopeTy' this b (ostmts, oexpr, t1) n = transScopeTy' b
  where
    transScopeTy' (Cons bnd) = do
      let ((x1, Embed t1), b) = unrebind bnd

      let cx1 = maybe ("c" ++ show x1) (show . id) n -- FIXME: use prefix

      let accessField = fieldAccess (left $ var cx1) closureInput
      let xf = initClass (javaType t1) (show x1) accessField

      (body', xe) <- case b of
                       Empty -> return (ostmts, oexpr)
                       Cons b' ->
                         transScopeTy' b

      let fstmt = localVar closureType (varDecl cx1 (funInstCreate cx1))

      return
        ([ localClassDecl (closureTransName ++ cx1) closureClass
             (closureBodyGen [memberDecl $ fieldDecl (classTy closureClass) (varDecl cx1 J.This)]
                ([xf] ++ body' ++ [bsAssign (name [closureOutput]) (unwrap xe)]))
         , fstmt
         ], var cx1)



{-

(\(x1:t1)..) e

==>

class Fx1 extends Closure {
<..>
}
Closure cx1 = new Fx1();

cx1.arg = j2;
cx1.apply();
|retTy| rx1 = |retTy| cx1.res;


-}
translateApply' this e1 e2 = do
  (s1, j1, Pi bnd) <- translateM this e1
  (d@(Cons de), b) <- unbind bnd
  let ((x1, _), de') = unrebind de
  (s2, j2, _) <- translateM this e2
  let (d', b') = multiSubst d e2 b
  let retTy =
        case d' of
          Empty -> b'
          _     -> Pi (bind d' b')

  let fexp = unwrap j1
  let fs = assignField (fieldAccExp fexp closureInput) (unwrap j2)
  let fapply = bStmt . applyMethodCall $ fexp
  let fout = fieldAccess fexp closureOutput
  let fret = "r" ++ show x1
  let fres = [initClass (javaType retTy) fret fout]

  return (s1 ++ s2 ++ (fs : fapply : fres), var fret, retTy)

createTypeHouse :: (Fresh m) => String -> m ([J.BlockStmt], TransJavaExp)
createTypeHouse str = do
  typeVar <- show <$> fresh (s2n str)
  let fstmt = [ localVar typeOfType (varDecl typeVar typeHouseCreate)
              -- , assignField (fieldAccExp (left . var $ typeVar) typeField) (J.Lit (J.String str))
              ]
  return (fstmt, var typeVar)


initClass :: J.Type -> String -> J.Exp -> J.BlockStmt
initClass ty tempName expr =
  localFinalVar ty
    (varDecl tempName
       (if ty == objClassTy
          then expr
          else cast ty expr))

getTupleClassName :: [a] -> String
getTupleClassName tuple =
  if lengthOfTuple > 50
    then panic "The length of tuple is too long (>50)!"
    else namespace ++ "tuples.Tuple" ++ show lengthOfTuple
  where
    lengthOfTuple = length tuple

javaType ty =
  case ty of
    Pi _ -> classTy closureClass
    (JClass c) -> classTy c
    (Product tuple) ->
      case tuple of
        [t] -> javaType t
        _   -> classTy $ getTupleClassName tuple
    _ -> objClassTy


transDefs' this defs =
  case defs of
    Def bnd -> do
      ((fname', Embed srcTyp, Embed expr), otherDef) <- unbind bnd
      (bs, e, t) <- translateM this expr
      otherDefStmts <- (extendCtx (mkTele [(fname', t)])) $ transDefs this otherDef
      let typ = javaType t
      -- HACK
      let fname = show fname'
      let anno = normalAnno fname fname (show srcTyp)
      let xDecl = localVarWithAnno anno typ (varDecl fname $ unwrap e)
      return (bs ++ [xDecl] ++ otherDefStmts)
    DefRec bnd ->
      do
        (defList, body) <- unbind bnd
        let (names, stypes', types', exprs') = unzip4 $ unrec defList
        let stypes = map unembed stypes'
        let types = map unembed types'
        let exprs = map unembed exprs'
        let newBindings = (mkTele $ zip names types)
        (bindStmts, bindExprs, tBinds) <- unzip3 <$> (mapM (extendCtx newBindings . translateM this)
                                                        exprs)
        bodyStmts <- (extendCtx newBindings) $ transDefs this body
        let bindtyps = map javaType tBinds
        -- Note: `name2string` return original name
        let annos = map (\(fname, srcTyp) -> normalAnno (name2String fname) (show fname) srcTyp)
                      (zip names (map show stypes))
        let assm = map (\(anno, fname, typ, e) -> localVarWithAnno anno typ (varDecl (show fname) $ unwrap e))
                     (zip4 annos names bindtyps bindExprs)
        return (concatMap (\(a, b) -> a ++ [b]) (bindStmts `zip` assm) ++ bodyStmts)
    DefNull -> return []
