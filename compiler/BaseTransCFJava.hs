{-# OPTIONS -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XScopedTypeVariables #-}

module BaseTransCFJava where
-- translation that does not pre-initialize Closures that are ininitalised in apply() methods of other Closures

import qualified ESF.Syntax           as E
import qualified Language.Java.Syntax as J
import qualified ClosureF             as C
import Inheritance
import StringPrefixes
import MonadLib
import Data.Char
import Debug.Trace

instance (:<) (Translate m) (Translate m) where
   up = id

type InitVars = [J.BlockStmt]

-- Closure F to Java

var x = J.ExpName (J.Name [J.Ident x])

jbody = Just (J.Block [])

init = [J.InitDecl False (J.Block [])]

closureClass = "hk.hku.cs.f2j.Closure"

closureType     = J.RefType (J.ClassRefType (J.ClassType [(J.Ident closureClass,[])]))
javaClassType c = J.RefType (J.ClassRefType (J.ClassType [(J.Ident c, [])]))
objType         = javaClassType "Object"
objArrayType    = J.RefType (J.ArrayType (J.RefType (J.ClassRefType (J.ClassType [(J.Ident "Object",[])]))))

ifBody :: ([J.BlockStmt], [J.BlockStmt]) -> (J.Exp, J.Exp, J.Exp) -> Int -> (J.BlockStmt, J.Exp)
ifBody (s2, s3) (j1, j2, j3) n = (J.BlockStmt $ J.IfThenElse (j1) (J.StmtBlock $ J.Block (s2 ++ j2Stmt)) (J.StmtBlock $ J.Block (s3 ++ j3Stmt)), newvar)
    where
        j2Stmt = [(J.LocalVars [] (J.RefType (refType "")) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Just (J.InitExp j2))]))]
        j3Stmt = [(J.LocalVars [] (J.RefType (refType "")) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Just (J.InitExp j3))]))]
        ifvarname = (ifresultstr ++ show n)
        refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
        newvar = var ifvarname

field name = J.MemberDecl (J.FieldDecl [] (objType) [
             J.VarDecl (J.VarId (J.Ident name)) Nothing])

app mod b rt en args = J.MemberDecl (J.MethodDecl mod [] (rt) (J.Ident en) args [] (J.MethodBody b))

applyCall = J.BlockStmt (J.ExpStmt (J.MethodInv (J.MethodCall (J.Name [J.Ident "apply"]) [])))

refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])

mainArgType = [J.FormalParam [] (J.RefType $ J.ArrayType (J.RefType (refType "String"))) False (J.VarId (J.Ident "args"))]
mainbody = Just (J.Block [J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall
    (J.ExpName (J.Name [J.Ident "System.out"])) [] (J.Ident "println") [J.ExpName $ J.Name [J.Ident ("apply" ++ "()")]])))])

createCUB this compDef = cu where
   cu = J.CompilationUnit Nothing [] compDef

getClassDecl className bs ass returnType mainbodyDef = J.ClassTypeDecl (J.ClassDecl [J.Public] (J.Ident className) [] (Nothing) []
    (J.ClassBody [app [J.Static] body returnType "apply" [], app [J.Public, J.Static] mainbodyDef Nothing "main" mainArgType]))
    where
        body = Just (J.Block (bs ++ ass))

reduceTTuples :: [([a], J.Exp, C.Type t)] -> ([a], J.Exp, C.Type t)
reduceTTuples all = (merged, arrayAssignment, tupleType)
    where
        merged = concat $ map (\x -> case x of (a,b,c) -> a) all
        arrayAssignment = J.ArrayCreateInit (objType) 1 (J.ArrayInit (map (\x -> case x of (a,b,c) -> J.InitExp b) all))
        tupleType = C.TupleType (map (\x -> case x of (a,b,c) -> c) all)

initStuff tempvarstr n j t = J.LocalVars [J.Final] (t) ([J.VarDecl (J.VarId $ J.Ident (tempvarstr ++ show n)) (Just exp)])
    where
        exp | t == objType = J.InitExp j
            | otherwise = J.InitExp $ J.Cast t j


initClassCast c tempvarstr n j = initStuff tempvarstr n j (javaClassType c)

initObj tempvarstr n j = initStuff tempvarstr n j objType

initClosure tempvarstr n j = initStuff tempvarstr n j closureType

initObjArray tempvarstr n j = initStuff tempvarstr n j objArrayType

type Var = Int -- Either Int Int left -> standard variable; right -> recursive variable

instCreat i = J.InstanceCreation [] (J.ClassType [(J.Ident ("Fun" ++ show i),[])]) [] Nothing

jexp init body idCF generateClone =
       J.ClassBody (init ++  [J.MemberDecl (J.MethodDecl [J.Public] [] Nothing (J.Ident "apply") [] [] (J.MethodBody body))] ++
          (if generateClone then [J.MemberDecl (J.MethodDecl [J.Public] [] (Just closureType) (J.Ident "clone") [] [] (J.MethodBody cloneBody))] else [])
       )
        where
            cloneBody = Just (J.Block [J.LocalVars [] (closureType) [J.VarDecl (J.VarId (J.Ident "c"))
                (Just $ J.InitExp $ instCreat idCF)],J.BlockStmt
                (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "c",J.Ident localvarstr])) J.EqualA
                (J.ExpName (J.Name [J.Ident "this",J.Ident localvarstr])))),J.BlockStmt (J.ExpStmt (J.MethodInv
                (J.PrimaryMethodCall (J.ExpName (J.Name [J.Ident "c"])) [] (J.Ident "apply") []))),J.BlockStmt (J.Return (Just
                (J.Cast (closureType) (J.ExpName (J.Name [J.Ident "c"])))))])

currentInitialDeclaration idCurrentName = J.MemberDecl $ J.FieldDecl [] closureType [J.VarDecl (J.VarId idCurrentName) (Just (J.InitExp J.This))]
outputAssignment javaExpression = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA  javaExpression))

{-
translateScopeTyp javaExpression statementsBeforeOA currentId nextId initVars generateClone =
     [(J.LocalClass (J.ClassDecl [] (J.Ident ("Fun" ++ show nextId)) []
        (Just $ J.ClassRefType (J.ClassType [(J.Ident "Closure",[])])) [] (jexp [currentInitialDeclaration
        (J.Ident (localvarstr ++ show currentId))] (Just (J.Block (initVars ++ statementsBeforeOA ++ [outputAssignment javaExpression]))) nextId generateClone))),
        J.LocalVars [] (closureType) ([J.VarDecl (J.VarId $ J.Ident (localvarstr ++ show nextId)) (Just (J.InitExp (instCreat nextId)))])]
-}

data Translate m = T {
  translateM ::
     C.Expr Int (Var, C.Type Int) ->
     m ([J.BlockStmt], J.Exp, C.Type Int),
  translateScopeM ::
    C.Scope (C.Expr Int (Var, C.Type Int)) Int (Var, C.Type Int) ->
    Maybe (Int,C.Type Int) ->
    m ([J.BlockStmt], J.Exp, C.TScope Int),
  translateApply ::  m ([J.BlockStmt], J.Exp, C.Type Int) ->  m ([J.BlockStmt], J.Exp, C.Type Int) ->  m ([J.BlockStmt], J.Exp, C.Type Int),
  translateIf ::  m ([J.BlockStmt], J.Exp, C.Type Int) -> m ([J.BlockStmt], J.Exp, C.Type Int) ->  m ([J.BlockStmt], J.Exp, C.Type Int) ->  m ([J.BlockStmt], J.Exp, C.Type Int),
  translateScopeTyp :: Int -> Int -> [J.BlockStmt] -> C.Scope (C.Expr Int (Var, C.Type Int)) Int (Var, C.Type Int) -> m ([J.BlockStmt], J.Exp, C.TScope Int) -> m ([J.BlockStmt], C.TScope Int),
  genApply :: J.Ident -> C.TScope Int -> J.Exp -> J.Type -> m [J.BlockStmt],
  genRes :: C.TScope Int -> [J.BlockStmt] -> m [J.BlockStmt],
  genClone :: m Bool,
  getCvarAss :: C.TScope Int -> J.Ident -> J.Exp -> J.Exp -> m [J.BlockStmt],
  -- getS3 :: TScope Int -> J.Exp -> (J.Exp -> J.Type -> [J.BlockStmt]) -> ([J.BlockStmt] -> [J.BlockStmt]) -> [J.BlockStmt] -> m ([J.BlockStmt], J.Exp)
  createWrap :: String -> C.Expr Int (Var, C.Type Int) -> m (J.CompilationUnit, C.Type Int)
  }

chooseCastBox (C.JClass c)       = (initClassCast c, javaClassType c)
chooseCastBox (C.Forall _)       = (initClosure,closureType)
chooseCastBox (C.TupleType [t])  = chooseCastBox t -- optimization for tuples of size 1
chooseCastBox (C.TupleType _)    = (initObjArray,objArrayType)
chooseCastBox _                 = (initObj,objType)

javaType (C.JClass c)      = javaClassType c
javaType (C.Forall _)      = closureType
javaType (C.TupleType [t]) = javaType t -- optimization for tuples of size 1
javaType (C.TupleType _)   = objArrayType
javaType _                = objType

getS3 this f t j3 cvarass =
  do (n :: Int) <- get
     put (n+1)
     let (cast,typ) = chooseCastBox (C.scope2ctyp t)
     apply <- genApply this f t (var (tempvarstr ++ show n)) typ
     rest <- genRes this t [cast tempvarstr n j3]
     let r = cvarass ++ apply ++ rest
     return (r, var (tempvarstr ++ show n))

genIfBody this m2 m3 j1 s1 n = do
            (s2,j2,t2) <- m2 {-translateM this e2-}
            (s3,j3,t3) <- m3 {-translateM this e3-}
            let ifvarname = (ifresultstr ++ show n)
            let refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
            let ifresdecl = J.LocalVars [] (javaType t2) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Nothing)])
            let (ifstmt, ifexp) = ifBody (s2, s3) (j1, j2, j3) n  -- uses a fresh variable
            return (s1 ++ [ifresdecl,ifstmt], ifexp, t2)  -- need to check t2 == t3

--(J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "c",J.Ident localvarstr])) J.EqualA

assignVar varId e t = J.LocalVars [] (javaType t) [J.VarDecl (J.VarId $ J.Ident varId) (Just (J.InitExp e))]

fieldAccess varId fieldId = J.FieldAccess $ J.PrimaryFieldAccess (J.ExpName (J.Name [J.Ident $ varId])) (J.Ident fieldId)

inputFieldAccess varId = fieldAccess varId localvarstr

pairUp :: [Var] -> [(J.Exp, C.Type Int)] -> ([(Var,J.Exp)],[(Var, C.Type Int)])
pairUp bindings vars = (paired, exchanged)
    where
      z = bindings `zip` vars
      paired = map (\x -> case x of (a,(b,c)) -> (a,b)) z
      exchanged = map (\x -> case x of (a,(b,c)) -> (a,c)) z

trans :: (MonadState Int m, selfType :< Translate m) => Base selfType (Translate m)
trans self = let this = up self in T {
  translateM = \e -> case e of
     C.Var (i, t) ->
       do return ([],var (localvarstr ++ show i), t)

     C.Lit lit -> case lit of (E.Integer i) -> return ([], J.Lit $ J.Int i, C.JClass "java.lang.Integer")
                              (E.String s)  -> return ([], J.Lit $ J.String s, C.JClass "java.lang.String")
                              (E.Boolean b) -> return ([], J.Lit $ J.Boolean b, C.JClass "java.lang.Boolean")
                              (E.Char c)    -> return ([], J.Lit $ J.Char c, C.JClass "java.lang.Character")

     C.PrimOp e1 op e2 ->
       do  (n :: Int) <- get
           put (n+1)
           (s1,j1,t1) <- translateM this e1
           (s2,j2,t2) <- translateM this e2
           let (je, typ) = case op of (E.Arith realOp)   -> (J.BinOp j1 realOp j2, C.JClass "java.lang.Integer")
                                      (E.Compare realOp) -> (J.BinOp j1 realOp j2, C.JClass "java.lang.Boolean")
                                      (E.Logic realOp)   -> (J.BinOp j1 realOp j2, C.JClass "java.lang.Boolean")
           return (s1 ++ s2 ++ [assignVar (localvarstr ++ show n) je typ],
                   var (localvarstr ++ show n), typ)

     C.If e1 e2 e3 -> translateIf this (translateM this e1) (translateM this e2) (translateM this e3)

     -- A simple optimization for tuples of size 1 (DOES NOT NEED TO BE FORMALIZED)
     C.Tuple [e] ->
       do  (s1,j1,t1) <- translateM this e
           return (s1,j1,C.TupleType [t1])

     -- otherwise: (not optimized)
     C.Tuple tuple ->
       liftM reduceTTuples $ mapM (translateM this) tuple

     C.Proj i e ->
       do (s1,j1,t) <- translateM this e
          case t of
             -- A simple optimization for tuples of size 1 (DOES NOT NEED TO BE FORMALIZED)
             C.TupleType [t] -> return (s1,j1,t)
             -- otherwise: (not optimized)
             C.TupleType ts  ->
               let fj = J.ArrayAccess (J.ArrayIndex j1 (J.Lit (J.Int $ toInteger (i-1))))
               in return (s1, J.Cast (javaType (ts!!(i-1))) fj, ts!!(i-1))
             otherwise -> error "expected tuple type"

     C.TApp e t ->
       do  n <- get
           (s,je, C.Forall (C.Kind f)) <- translateM this e
           return (s,je, C.scope2ctyp (C.substScope n t (f n)))
    -- TODO: CLam and CFix generation of top-level Fun closures is a bit ad-hoc transformation from the old generated code + duplicate code
     C.Lam se ->
       do  (n :: Int) <- get
           (s,je, t) <- translateScopeM this se Nothing
           return (s,je, C.Forall t)

     C.Fix t s   ->
       do  (n :: Int) <- get
           put (n+1)
           (s, je, t') <- translateScopeM this (s (n,t)) (Just (n,t)) -- weird!
           return (s,je, C.Forall t')

     C.LetRec t xs body ->
       do  (n :: Int) <- get
           let needed = length $ (xs (zip [n..] (repeat (t!!0))))
           put (n+2+needed)
           mfuns <- return (\defs -> forM (xs defs) (translateM this))

           let vars = (liftM (map (\x -> case x of (a,b,c) -> (b,c)))) (mfuns (zip [n..] (repeat (t!!0))))
           let (bindings :: [Var]) = [n+2..n+1+needed]
           (x2xs, newvars) <- ((liftM (pairUp bindings)) vars)
           let mDecls = map (\x -> J.MemberDecl (J.FieldDecl [J.Public] (closureType) [J.VarDecl (J.VarId (J.Ident (localvarstr ++ show x))) Nothing])) bindings
           let finalFuns = mfuns newvars
           let appliedBody = body newvars
           (s, je, t') <- translateM this appliedBody--join ((liftM (translateM this)) appliedBody)
           (stmts, _, _) <- ((liftM (unzip3)) (finalFuns))
           let assm = map (\(i, jz) -> J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident (localvarstr ++ show i)])) J.EqualA jz))) x2xs
           let stasm = concatMap (\(a,b) -> a ++ [b]) (stmts `zip` assm)
           let letClass = [J.LocalClass (J.ClassDecl [] (J.Ident ("Let" ++ show n)) [] Nothing [] (J.ClassBody
                           (J.MemberDecl (J.FieldDecl [J.Public] (objType) [J.VarDecl (J.VarId (J.Ident "out")) Nothing]) : mDecls ++ [J.InitDecl False (J.Block stasm)]))),
                           J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident (localvarstr ++ show n)])) J.EqualA (J.InstanceCreation [] (J.ClassType [(J.Ident ("Let" ++ show n),[])]) [] Nothing))),
                           J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident (localvarstr ++ show (n+1))])) J.EqualA (fieldAccess (localvarstr ++ show n) "out")))]
           return (letClass, var (localvarstr ++ show (n+1)), t')
           --((liftM (!!1)) (finalFuns))--(s, je, t')

     C.App e1 e2 -> translateApply this (translateM this e1) (translateM this e2)

     -- InstanceCreation [TypeArgument] ClassType [Argument] (Maybe ClassBody)
     C.JNewObj c args -> do args' <- mapM (translateM this) args
                            let argsStatements = concat $ map (\(x, _, _) -> x) args'
                            let argsExprs = map (\(_, x, _) -> x) args'
                            let argTypes = map (\(_, _, x) -> x) args'
                            (n :: Int) <- get
                            put (n + 1)
                            let rhs = J.InstanceCreation (map (\(C.JClass x) -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident x, [])]) argTypes)
                                                         (J.ClassType [(J.Ident c, [])]) argsExprs Nothing
                            let typ = C.JClass c
                            return (argsStatements ++ [assignVar (localvarstr ++ show n) rhs typ], var (localvarstr ++ show n), typ)

     C.JMethod (Left c) m args r ->
       do args' <- mapM (translateM this) args
          let argsStatements = concat $ map (\(x, _, _) -> x) args'
          let argsExprs = map (\(_, x, _) -> x) args'
          let argTypes = map (\(_, _, x) -> x) args'
          (classStatement, classExpr, _) <- translateM this c
          let refTypeArgs = (map (\(C.JClass x) -> J.ClassRefType $ J.ClassType [(J.Ident x, [])]) argTypes)
          let rhs = J.MethodInv $ J.PrimaryMethodCall classExpr refTypeArgs (J.Ident m) argsExprs
          let typ = C.JClass r
          if r /= "java.lang.Void"
            then do (n :: Int) <- get
                    put (n + 1)
                    return ( argsStatements ++ classStatement ++ [assignVar (localvarstr ++ show n) rhs typ]
                           , var (localvarstr ++ show n)
                           , typ )
            else return ( argsStatements ++ classStatement ++ [(J.BlockStmt $ J.ExpStmt rhs)], rhs, typ )

     C.JMethod (Right c) m args r ->
       do args' <- mapM (translateM this) args
          let argsStatements = concat $ map (\(x, _, _) -> x) args'
          let argsExprs = map (\(_, x, _) -> x) args'
          let argTypes = map (\(_, _, x) -> x) args'
          let refTypeArgs = (map (\(C.JClass x) -> J.ClassRefType $ J.ClassType [(J.Ident x, [])]) argTypes)
          let rhs = J.MethodInv $ J.TypeMethodCall (J.Name [J.Ident c]) refTypeArgs (J.Ident m) argsExprs
          let typ = C.JClass r
          if r /= "java.lang.Void"
            then do (n :: Int) <- get
                    put (n + 1)
                    return ( argsStatements ++ [assignVar (localvarstr ++ show n) rhs typ]
                           , var (localvarstr ++ show n)
                           , typ )
            else return ( argsStatements ++ [(J.BlockStmt $ J.ExpStmt rhs)], rhs, typ )

     C.JField c fName r ->
       case c of
         (Left ce) -> do (classStatement, classExpr, _) <- translateM this ce
                         let rhs = J.FieldAccess $ J.PrimaryFieldAccess classExpr (J.Ident fName)
                         let typ = C.JClass r
                         (n :: Int) <- get
                         put (n + 1)
                         let newName = localvarstr ++ show n
                         return (classStatement ++ [assignVar newName rhs typ], var newName, typ)
         (Right cn) -> do let classExpr = J.ExpName $ J.Name [J.Ident cn]
                          let rhs = J.FieldAccess $ J.PrimaryFieldAccess classExpr (J.Ident fName)
                          let typ = C.JClass r
                          (n :: Int) <- get
                          put (n + 1)
                          let newName = localvarstr ++ show n
                          return ([assignVar newName rhs typ], var newName, typ)

     C.SeqExprs es -> do es' <- mapM (translateM this) es
                         let (_, lastExp, lastType) = last es'
                         let statements = concat $ map (\(x, _, _) -> x) es'
                         return (statements, lastExp, lastType)

     ,

  translateScopeM = \e m -> case e of
      C.Body t ->
        do  (s,je, t1) <- translateM this t
            return (s,je, C.Body t1)

      C.Kind f ->
        do  n <- get
            put (n+1) -- needed?
            (s,je,t1) <- translateScopeM this (f n) m
            return (s,je, C.Kind (\a -> C.substScope n (C.TVar a) t1))

      C.Type t g ->
        do  n <- get
            let f       = J.Ident (localvarstr ++ show n) -- use a fresh variable
            let (v,n')  = maybe (n+1,n+2) (\(i,_) -> (i,n+1)) m -- decide whether we have found the fixpoint closure or not
            put (n' + 1)
            let nextInClosure = g (n',t)
            let js = (initStuff localvarstr n' (inputFieldAccess (localvarstr ++ show v)) (javaType t))
            (cvar,t1) <- translateScopeTyp this v n [js] nextInClosure (translateScopeM this nextInClosure Nothing)
            return (cvar,J.ExpName (J.Name [f]), C.Type t (\_ -> t1) ),

  translateApply = \m1 m2 ->
       do  (n :: Int) <- get
           put (n+1)
           (s1,j1, C.Forall (C.Type t1 g)) <- m1
           (s2,j2,t2) <- m2
           let t    = g ()
           let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
           cvarass <- getCvarAss (up this) t f j1 j2
           let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
           (s3, nje3) <- getS3 (up this) f t j3 cvarass
           return (s1 ++ s2 ++ s3, nje3, C.scope2ctyp t),

  translateIf = \m1 m2 m3 ->
        do  n <- get
            put (n+1)
            (s1,j1,t1) <- m1 {- translateM this e1 -}
            -- let j1' = J.BinOp j1 J.Equal (J.Lit (J.Int 0))
            -- genIfBody this m2 m3 j1' s1 n,
            genIfBody this m2 m3 j1 s1 n,

  translateScopeTyp = \currentId nextId initVars nextInClosure m ->
     do b <- genClone this
        (statementsBeforeOA,javaExpression,t1) <- m
        return ([(J.LocalClass (J.ClassDecl [] (J.Ident ("Fun" ++ show nextId)) []
                 (Just $ J.ClassRefType (J.ClassType [(J.Ident closureClass,[])])) [] (jexp [currentInitialDeclaration
                 (J.Ident (localvarstr ++ show currentId))] (Just (J.Block (initVars ++ statementsBeforeOA ++ [outputAssignment javaExpression]))) nextId b))),
                 J.LocalVars [] (closureType) ([J.VarDecl (J.VarId $ J.Ident (localvarstr ++ show nextId)) (Just (J.InitExp (instCreat nextId)))])],t1),


  genApply = \f t x y -> return [J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall (J.ExpName (J.Name [f])) [] (J.Ident "apply") [])))],

  genRes = \t -> return,

  getCvarAss = \t f j1 j2 ->
     return [ J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp j1))]),
              J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident localvarstr))) J.EqualA j2) )],

  genClone = return False, -- do not generate clone method

  createWrap = \name exp ->
        do (bs,e,t) <- translateM this exp
           let returnType = case t of C.JClass "java.lang.Integer" -> Just $ J.PrimType $ J.IntT
                                      _ -> Just $ objType
           let classDecl = getClassDecl name bs ([J.BlockStmt (J.Return $ Just e)]) returnType mainbody
           return (createCUB this [classDecl], t)

    }
