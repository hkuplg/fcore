{-# LANGUAGE FlexibleContexts, TypeOperators, MultiParamTypeClasses, ScopedTypeVariables #-}

module BaseTrans where

import           Control.Monad.Reader
import qualified Language.Java.Syntax as J
import           Lens.Micro
import           Unbound.Generics.LocallyNameless

import           Inheritance
import           JavaLang
import           Panic
import           StringPrefixes
import           Syntax
import           TransEnvironment

type TransJavaExp = Either J.Name J.Literal

type TransType = ([J.BlockStmt], TransJavaExp, Expr)

data Translate m =
       T
         { translateM :: Expr -> m TransType
         , translateScopeM :: Bind Tele Expr -> Maybe TmName -> m ([J.BlockStmt], TransJavaExp, Bind Tele Expr)
         , translateApply :: Expr -> Expr -> m TransType
         , translateScopeTy :: Tele -> TransType -> Maybe TmName -> m ([J.BlockStmt], TransJavaExp)
         , createWrap :: String -> Expr -> m J.CompilationUnit
         }

createWrap' this str e = do
  (bs, e, t) <- translateM this e
  let returnStmt = [bStmt $ J.Return $ Just (unwrap e)]
  let returnType = applyRetType t
  let javaCode = wrapperClass False str (bs ++ returnStmt) returnType mainBody
  return (createCUB Nothing [javaCode])

createCUB :: Maybe J.PackageDecl -> [J.TypeDecl] -> J.CompilationUnit
createCUB package compDef = cu
  where
    cu = J.CompilationUnit package [] compDef

applyRetType Nat = Just $ J.PrimType J.IntT
applyRetType _ = Just objClassTy

trans :: (Fresh m, MonadReader Context m, selfType :< Translate m) => Base selfType (Translate m)
trans self =
  let this = up self
  in T
    { translateM  = translateM' this
    , translateScopeM = translateScopeM' this
    , translateScopeTy = translateScopeTy' this
    , translateApply = translateApply' this
    , createWrap = createWrap' this
    }


translateM' this e =
  case e of
    Var n -> do
      t <- lookupTy n
      return ([], var (show n), t)
    Lit lit -> return ([], Right $ J.Int lit, Nat)
    PrimOp op e1 e2 -> do
      (s1, j1, _) <- translateM this e1
      (s2, j2, _) <- translateM this e2
      let j1' = unwrap j1
      let j2' = unwrap j2
      let (je, t) =
            case op of
              Add  -> (J.BinOp j1' J.Add j2', Nat)
              Mult -> (J.BinOp j1' J.Mult j2', Nat)
              Sub  -> (J.BinOp j1' J.Sub j2', Nat)
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
          (s, je, _) <- extendCtx (mkTele [(name2String x, t)]) (translateScopeM this bnd (Just x))
          return (s, je, t)

        _ -> panic "Expected a lambda abstraction after mu"

    Let bnd -> do
      ((x, Embed e), b) <- unbind bnd
      let x' = name2String x
      (s1, j1, t1) <- translateM this e
      -- types do need to generate code
      if (aeq t1 estar || aeq t1 ebox)
        then translateM this (subst x e b)
        else do
          let jt1 = javaType t1
          let xDecl = localVar jt1 (varDecl x' $ unwrap j1)
          (s2, j2, t2) <- extendCtx (mkTele [(x', t1)]) (translateM this b)
          return (s1 ++ [xDecl] ++ s2, j2, t2)

    F t e -> do
      (s, v, _) <- translateM this e
      return (s, v, t)

    U e -> do
      (s, v, t1) <- translateM this e
      let t2 = oneStep t1
      return (s, v, t2)

    Nat -> do
      (js, v) <- createTypeHouse "nat"
      return (js, v, Kind Star)

    Kind Star -> do
      (js, v) <- createTypeHouse "star"
      return (js, v, Kind Box)

    Pi _ -> do
      (js, v) <- createTypeHouse "pi"
      return (js, v, Kind Star)

    _ -> sorry "Not implemented yet"


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
          _     -> Pi (bind d' b)

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
              , assignField (fieldAccExp (left . var $ typeVar) typeField) (J.Lit (J.String str))
              ]
  return (fstmt, var typeVar)


initClass :: J.Type -> String -> J.Exp -> J.BlockStmt
initClass ty tempName expr =
  localFinalVar ty
    (varDecl tempName
       (if ty == objClassTy
          then expr
          else cast ty expr))

javaType ty =
  case ty of
    Pi _ -> classTy closureClass
    Nat  -> classTy "java.lang.Integer"
    _    -> objClassTy
