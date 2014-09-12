{-# OPTIONS -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XScopedTypeVariables #-}

module BaseTransCFJava where
-- translation that does not pre-initialize Closures that are ininitalised in apply() methods of other Closures

import           ClosureF
import           Inheritance
import qualified Language.Java.Syntax as J
import           MonadLib
import           Panic
import           Prelude hiding (init)
import qualified Src
import           StringPrefixes

instance (:<) (Translate m) (Translate m) where
   up = id

type InitVars = [J.BlockStmt]

-- Closure F to Java

names :: [String] -> J.Name
names xs = J.Name $ map J.Ident xs

var :: String -> J.Exp
var x = J.ExpName $ names [x]

jbody :: Maybe J.Block
jbody = Just (J.Block [])

init :: [J.Decl]
init = [J.InitDecl False (J.Block [])]

closureClass :: String
closureClass = "hk.hku.cs.f2j.Closure"

javaClassType :: String -> J.Type
javaClassType t = J.RefType $ J.ClassRefType (J.ClassType [(J.Ident t,[])])

closureType :: J.Type
closureType     = javaClassType closureClass

objType :: J.Type
objType         = javaClassType "Object"

arrayType :: J.Type -> J.Type
arrayType ty  = J.RefType (J.ArrayType ty)

objArrayType :: J.Type
objArrayType = arrayType objType

varDecl :: String -> Maybe J.VarInit -> J.VarDecl
varDecl name e = J.VarDecl (J.VarId $ J.Ident name) e

ifBody :: ([J.BlockStmt], [J.BlockStmt]) -> (J.Exp, J.Exp, J.Exp) -> Int -> (J.BlockStmt, J.Exp)
ifBody (s2, s3) (j1, j2, j3) n = (J.BlockStmt $ J.IfThenElse j1
                                                             (J.StmtBlock $ J.Block (s2 ++ j2Stmt))
                                                             (J.StmtBlock $ J.Block (s3 ++ j3Stmt))
                                 , newvar)
    where j2Stmt = [localVarDecl j2]
          j3Stmt = [localVarDecl j3]
          localVarDecl e = J.LocalVars []
                                       (javaClassType "")
                                       [varDecl ifvarname (Just (J.InitExp e))]
          ifvarname = (ifresultstr ++ show n)
          newvar = var ifvarname

field :: String -> J.Decl
field name = J.MemberDecl $ J.FieldDecl []
                                        objType
                                        [varDecl name Nothing]

app :: [J.Modifier] -> Maybe J.Block -> Maybe J.Type -> String -> [J.FormalParam] -> J.Decl
app modi b rt en args = J.MemberDecl (J.MethodDecl modi [] rt (J.Ident en) args [] (J.MethodBody b))

applyCall :: J.BlockStmt
applyCall = J.BlockStmt (J.ExpStmt (J.MethodInv (J.MethodCall (J.Name [J.Ident "apply"]) [])))

mainArgType :: [J.FormalParam]
mainArgType = [J.FormalParam []
                             (arrayType $ javaClassType "String")
                             False
                             (J.VarId (J.Ident "args"))]

mainbody :: Maybe J.Block
mainbody = Just (J.Block [J.BlockStmt $ J.ExpStmt $
                                          J.MethodInv $
                                             J.PrimaryMethodCall
                                                (var "System.out")
                                                []
                                                (J.Ident "println")
                                                [var "apply()"]])

createCUB :: t -> [J.TypeDecl] -> J.CompilationUnit
createCUB _ compDef = cu
  where cu = J.CompilationUnit Nothing [] compDef

getClassDecl :: String -> [J.BlockStmt] -> [J.BlockStmt] -> Maybe J.Type -> Maybe J.Block -> J.TypeDecl
getClassDecl className bs ass returnType mainbodyDef =
  J.ClassTypeDecl (J.ClassDecl [J.Public] (J.Ident className) [] Nothing []
                               (J.ClassBody [app [J.Static] body returnType "apply" []
                                            ,app [J.Public, J.Static] mainbodyDef Nothing "main" mainArgType]))
    where body = Just (J.Block (bs ++ ass))


reduceTTuples :: [([a], J.Exp, Type t)] -> ([a], J.Exp, Type t)
reduceTTuples all' = (merged, arrayAssignment, tupleType)
    where
        merged = concatMap (\x -> case x of (a, _, _) -> a) all'
        arrayAssignment = J.ArrayCreateInit objType
                                            1
                                            (J.ArrayInit (map (\x -> case x of (_, b, _) -> J.InitExp b) all'))
        tupleType = TupleType (map (\x -> case x of (_, _, c) -> c) all')

initStuff :: Show a => String -> a -> J.Exp -> J.Type -> J.BlockStmt
initStuff tempVarStr n j t = J.LocalVars [J.Final] t [varDecl (tempVarStr ++ show n) (Just e)]
    where
        e | t == objType = J.InitExp j
          | otherwise = J.InitExp $ J.Cast t j


initClassCast :: Show a => String -> String -> a -> J.Exp -> J.BlockStmt
initClassCast c tempVarStr n j = initStuff tempVarStr n j (javaClassType c)

initObj :: Show a => String -> a -> J.Exp -> J.BlockStmt
initObj tempVarStr n j = initStuff tempVarStr n j objType

initClosure :: Show a => String -> a -> J.Exp -> J.BlockStmt
initClosure tempVarStr n j = initStuff tempVarStr n j closureType

initObjArray :: Show a => String -> a -> J.Exp -> J.BlockStmt
initObjArray tempVarStr n j = initStuff tempVarStr n j objArrayType

type Var = Int -- Either Int Int left -> standard variable; right -> recursive variable

instCreat :: Show a => a -> J.Exp
instCreat i = J.InstanceCreation [] (J.ClassType [(J.Ident ("Fun" ++ show i),[])]) [] Nothing

jexp :: Show a => [J.Decl] -> Maybe J.Block -> a -> Bool -> J.ClassBody
jexp init' body idCF generateClone =
       J.ClassBody $ init'
                  ++ [J.MemberDecl $ methodDecl "apply" Nothing body]
                  ++ if generateClone
                       then [J.MemberDecl $ methodDecl "clone" (Just closureType) cloneBody] 
                       else []
        where
            methodDecl name typ by = J.MethodDecl [J.Public] [] typ (J.Ident name) [] [] (J.MethodBody by)
            cloneBody = Just (J.Block [J.LocalVars [] closureType [varDecl "c"
                                                                           (Just $ J.InitExp $ instCreat idCF)]
                                    , J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs $ names ["c", localvarstr]) 
                                                                       J.EqualA
                                                                       (J.ExpName $ names ["this", localvarstr])))
                                    , J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall (var "c") [] (J.Ident "apply") [])))
                                    , J.BlockStmt (J.Return (Just (J.Cast closureType (var "c"))))])

currentInitialDeclaration :: J.Ident -> J.Decl
currentInitialDeclaration idCurrentName = J.MemberDecl $ J.FieldDecl []
                                                                     closureType 
                                                                     [J.VarDecl (J.VarId idCurrentName) (Just (J.InitExp J.This))]

outputAssignment :: J.Exp -> J.BlockStmt
outputAssignment javaExpression = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (names ["out"])) 
                                                                   J.EqualA
                                                                   javaExpression))

{-
translateScopeTyp javaExpression statementsBeforeOA currentId nextId initVars generateClone =
     [(J.LocalClass (J.ClassDecl [] (J.Ident ("Fun" ++ show nextId)) []
        (Just $ J.ClassRefType (J.ClassType [(J.Ident "Closure",[])])) [] (jexp [currentInitialDeclaration
        (J.Ident (localvarstr ++ show currentId))] (Just (J.Block (initVars ++ statementsBeforeOA ++ [outputAssignment javaExpression]))) nextId generateClone))),
        J.LocalVars [] (closureType) ([J.VarDecl (J.VarId $ J.Ident (localvarstr ++ show nextId)) (Just (J.InitExp (instCreat nextId)))])]
-}

type TransType = ([J.BlockStmt], J.Exp, Type Int)


data Translate m = T {
  translateM ::
     Expr Int (Var, Type Int) ->
     m ([J.BlockStmt], J.Exp, Type Int),
  translateScopeM ::
    Scope (Expr Int (Var, Type Int)) Int (Var, Type Int) ->
    Maybe (Int,Type Int) ->
    m ([J.BlockStmt], J.Exp, TScope Int),
  translateApply ::  m TransType ->  m TransType ->  m TransType,
  translateIf ::  m TransType -> m TransType ->  m TransType ->  m TransType,
  translateScopeTyp :: Int -> Int -> [J.BlockStmt] -> Scope (Expr Int (Var, Type Int)) Int (Var, Type Int) -> m ([J.BlockStmt], J.Exp, TScope Int) -> m ([J.BlockStmt], TScope Int),
  genApply :: J.Ident -> TScope Int -> J.Exp -> J.Type -> m [J.BlockStmt],
  genRes :: TScope Int -> [J.BlockStmt] -> m [J.BlockStmt],
  genClone :: m Bool,
  getCvarAss :: TScope Int -> J.Ident -> J.Exp -> J.Exp -> m [J.BlockStmt],
  -- getS3 :: TScope Int -> J.Exp -> (J.Exp -> J.Type -> [J.BlockStmt]) -> ([J.BlockStmt] -> [J.BlockStmt]) -> [J.BlockStmt] -> m ([J.BlockStmt], J.Exp)
  createWrap :: String -> Expr Int (Var, Type Int) -> m (J.CompilationUnit, Type Int)
  }

chooseCastBox :: Show a => Type t -> (String -> a -> J.Exp -> J.BlockStmt, J.Type)
chooseCastBox (JClass c)       = (initClassCast c, javaClassType c)
chooseCastBox (Forall _)       = (initClosure,closureType)
chooseCastBox (TupleType [t])  = chooseCastBox t -- optimization for tuples of size 1
chooseCastBox (TupleType _)    = (initObjArray,objArrayType)
chooseCastBox _                 = (initObj,objType)

javaType :: Type t -> J.Type
javaType (JClass c)      = javaClassType c
javaType (Forall _)      = closureType
javaType (TupleType [t]) = javaType t -- optimization for tuples of size 1
javaType (TupleType _)   = objArrayType
javaType _                = objType

getS3 :: MonadState Int m => Translate m -> J.Ident -> TScope Int -> J.Exp -> [J.BlockStmt] -> m ([J.BlockStmt], J.Exp)
getS3 this f t j3 cvarass =
  do (n :: Int) <- get
     put (n+1)
     let (cast,typ) = chooseCastBox (scope2ctyp t)
     apply <- genApply this f t (var (tempvarstr ++ show n)) typ
     rest <- genRes this t [cast tempvarstr n j3]
     let r = cvarass ++ apply ++ rest
     return (r, var (tempvarstr ++ show n))

genIfBody this m2 m3 j1 s1 n = do
            (s2,j2,t2) <- m2 {-translateM this e2-}
            (s3,j3,t3) <- m3 {-translateM this e3-}
            let ifvarname = (ifresultstr ++ show n)
            let refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
            let ifresdecl = J.LocalVars [] (javaType t2) [J.VarDecl (J.VarId $ J.Ident ifvarname) (Nothing)]
            let (ifstmt, ifexp) = ifBody (s2, s3) (j1, j2, j3) n  -- uses a fresh variable
            return (s1 ++ [ifresdecl,ifstmt], ifexp, t2)  -- need to check t2 == t3

--(J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "c",J.Ident localvarstr])) J.EqualA

assignVar varId e t = J.LocalVars [] (javaType t) [J.VarDecl (J.VarId $ J.Ident varId) (Just (J.InitExp e))]

fieldAccess varId fieldId = J.FieldAccess $ J.PrimaryFieldAccess (J.ExpName (J.Name [J.Ident $ varId])) (J.Ident fieldId)

inputFieldAccess varId = fieldAccess varId localvarstr

pairUp :: [Var] -> [(J.Exp, Type Int)] -> [(Var, Type Int)]
pairUp bindings vars = exchanged
    where
      z = bindings `zip` vars
      exchanged = map (\x -> case x of (a,(b,c)) -> (a,c)) z

trans :: (MonadState Int m, selfType :< Translate m) => Base selfType (Translate m)
trans self = let this = up self in T {
  translateM = \e -> case e of
     Var (i, t) -> return ([],var (localvarstr ++ show i), t)

     Lit lit -> case lit of
                  (Src.Integer i) -> return ([], J.Lit $ J.Int i, JClass "java.lang.Integer")
                  (Src.String s)  -> return ([], J.Lit $ J.String s, JClass "java.lang.String")
                  (Src.Boolean b) -> return ([], J.Lit $ J.Boolean b, JClass "java.lang.Boolean")
                  (Src.Char c)    -> return ([], J.Lit $ J.Char c, JClass "java.lang.Character")

     PrimOp e1 op e2 ->
       do  (n :: Int) <- get
           put (n+1)
           (s1,j1,t1) <- translateM this e1
           (s2,j2,t2) <- translateM this e2
           let (je, typ) = case op of
                             (Src.Arith realOp)   -> (J.BinOp j1 realOp j2, JClass "java.lang.Integer")
                             (Src.Compare realOp) -> (J.BinOp j1 realOp j2, JClass "java.lang.Boolean")
                             (Src.Logic realOp)   -> (J.BinOp j1 realOp j2, JClass "java.lang.Boolean")
           return (s1 ++ s2 ++ [assignVar (localvarstr ++ show n) je typ],
                   var (localvarstr ++ show n), typ)

     If e1 e2 e3 -> translateIf this (translateM this e1) (translateM this e2) (translateM this e3)

     -- A simple optimization for tuples of size 1 (DOES NOT NEED TO BE FORMALIZED)
     Tuple [e] ->
       do  (s1,j1,t1) <- translateM this e
           return (s1,j1,TupleType [t1])

     -- otherwise: (not optimized)
     Tuple tuple ->
       liftM reduceTTuples $ mapM (translateM this) tuple

     Proj i e ->
       do (s1,j1,t) <- translateM this e
          case t of
             -- A simple optimization for tuples of size 1 (DOES NOT NEED TO BE FORMALIZED)
             TupleType [t] -> return (s1,j1,t)
             -- otherwise: (not optimized)
             TupleType ts  ->
               let fj = J.ArrayAccess (J.ArrayIndex j1 (J.Lit (J.Int $ toInteger (i-1))))
               in return (s1, J.Cast (javaType (ts!!(i-1))) fj, ts!!(i-1))
             otherwise -> panic "BaseTransCFJava.trans: expected tuple type"

     TApp e t ->
       do  n <- get
           (s,je, Forall (Kind f)) <- translateM this e
           return (s,je, scope2ctyp (substScope n t (f n)))
    -- TODO: CLam and CFix generation of top-level Fun closures is a bit ad-hoc transformation from the old generated code + duplicate code
     Lam se ->
       do  (n :: Int) <- get
           (s,je, t) <- translateScopeM this se Nothing
           return (s,je, Forall t)

     Fix t s   ->
       do  (n :: Int) <- get
           put (n+1)
           (s, je, t') <- translateScopeM this (s (n,t)) (Just (n,t)) -- weird!
           return (s,je, Forall t')

     LetRec t xs body ->
       do  (n :: Int) <- get
           let needed = length $ (xs (zip [n..] (repeat (t!!0))))
           put (n+2+needed)
           mfuns <- return (\defs -> forM (xs defs) (translateM this))

           let vars = (liftM (map (\x -> case x of (a,b,c) -> (b,c)))) (mfuns (zip [n..] (repeat (t!!0))))
           let (bindings :: [Var]) = [n+2..n+1+needed]
           newvars <- ((liftM (pairUp bindings)) vars)
           let mDecls = map (\x -> J.MemberDecl (J.FieldDecl [J.Public] (closureType) [J.VarDecl (J.VarId (J.Ident (localvarstr ++ show x))) Nothing])) bindings
           let finalFuns = mfuns newvars
           let appliedBody = body newvars
           let tnv = map fst newvars
           (s, je, t') <- translateM this appliedBody--join ((liftM (translateM this)) appliedBody)
           (stmts, jex, _) <- ((liftM (unzip3)) (finalFuns))
           let typ = javaType t'
           let assm = map (\(i, jz) -> J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident (localvarstr ++ show i)])) J.EqualA jz))) (tnv `zip` jex)
           let stasm = (concatMap (\(a,b) -> a ++ [b]) (stmts `zip` assm)) ++ s ++ [J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "out"])) J.EqualA je))]
           let letClass = [J.LocalClass (J.ClassDecl [] (J.Ident ("Let" ++ show n)) [] Nothing [] (J.ClassBody
                           (J.MemberDecl (J.FieldDecl [J.Public] (objType) [J.VarDecl (J.VarId (J.Ident "out")) Nothing]) : mDecls ++ [J.InitDecl False (J.Block stasm)]))),
                           J.LocalVars [] (J.RefType (J.ClassRefType (J.ClassType [(J.Ident ("Let" ++ show n),[])]))) [J.VarDecl (J.VarId (J.Ident (localvarstr ++ show n))) (Just (J.InitExp (J.InstanceCreation [] (J.ClassType [(J.Ident ("Let" ++ show n),[])]) [] Nothing)))],
                           J.LocalVars [] typ [J.VarDecl (J.VarId (J.Ident (localvarstr ++ show (n+1)))) (Just (J.InitExp (J.Cast (typ) (J.ExpName (J.Name [J.Ident (localvarstr ++ show n),J.Ident "out"])))))]]
           return (letClass, var (localvarstr ++ show (n+1)), t')

     App e1 e2 -> translateApply this (translateM this e1) (translateM this e2)

     -- InstanceCreation [TypeArgument] ClassType [Argument] (Maybe ClassBody)
     JNewObj c args -> do args' <- mapM (translateM this) args
                          let argsStatements = concat $ map (\(x, _, _) -> x) args'
                          let argsExprs = map (\(_, x, _) -> x) args'
                          let argTypes = map (\(_, _, x) -> x) args'
                          (n :: Int) <- get
                          put (n + 1)
                          let rhs = J.InstanceCreation (map (\(JClass x) -> J.ActualType $ J.ClassRefType $ J.ClassType [(J.Ident x, [])]) argTypes)
                                                       (J.ClassType [(J.Ident c, [])]) argsExprs Nothing
                          let typ = JClass c
                          return (argsStatements ++ [assignVar (localvarstr ++ show n) rhs typ], var (localvarstr ++ show n), typ)

     JMethod c m args r ->
       do args' <- mapM (translateM this) args
          let argsStatements = concatMap (\(x, _, _) -> x) args'
          let argsExprs = map (\(_, x, _) -> x) args'
          let argTypes = map (\(_, _, x) -> x) args'
          let refTypeArgs = (map (\(JClass x) -> J.ClassRefType $ J.ClassType [(J.Ident x, [])]) argTypes)
          (classStatement, rhs) <- case c of
                                     (Right ce) -> do (classS, classE, _) <- translateM this ce
                                                      return (classS, J.MethodInv $ J.PrimaryMethodCall classE refTypeArgs (J.Ident m) argsExprs)
                                     (Left cn)  -> return ([], J.MethodInv $ J.TypeMethodCall (J.Name [J.Ident cn]) refTypeArgs (J.Ident m) argsExprs)
          let typ = JClass r
          if r /= "java.lang.Void"
            then do (n :: Int) <- get
                    put (n + 1)
                    return ( argsStatements ++ classStatement ++ [assignVar (localvarstr ++ show n) rhs typ]
                           , var (localvarstr ++ show n)
                           , typ )
            else return ( argsStatements ++ classStatement ++ [(J.BlockStmt $ J.ExpStmt rhs)], rhs, typ )


     JField c fName r ->
       do (classStatement, classExpr, _) <- case c of (Right ce) -> translateM this ce
                                                      (Left cn)  -> return ([], J.ExpName $ J.Name [J.Ident cn], undefined)
          (n :: Int) <- get
          put (n + 1)
          let newName = localvarstr ++ show n
          let typ = JClass r
          let rhs = J.FieldAccess $ J.PrimaryFieldAccess classExpr (J.Ident fName)
          return (classStatement ++ [assignVar newName rhs typ], var newName, typ)

     SeqExprs es -> do es' <- mapM (translateM this) es
                       let (_, lastExp, lastType) = last es'
                       let statements = concat $ map (\(x, _, _) -> x) es'
                       return (statements, lastExp, lastType)

     ,

  translateScopeM = \e m -> case e of
      Body t ->
        do  (s,je, t1) <- translateM this t
            return (s,je, Body t1)

      Kind f ->
        do  n <- get
            put (n+1) -- needed?
            (s,je,t1) <- translateScopeM this (f n) m
            return (s,je, Kind (\a -> substScope n (TVar a) t1))

      Type t g ->
        do  n <- get
            let f       = J.Ident (localvarstr ++ show n) -- use a fresh variable
            let (v,n')  = maybe (n+1,n+2) (\(i,_) -> (i,n+1)) m -- decide whether we have found the fixpoint closure or not
            put (n' + 1)
            let nextInClosure = g (n',t)
            let js = (initStuff localvarstr n' (inputFieldAccess (localvarstr ++ show v)) (javaType t))
            (cvar,t1) <- translateScopeTyp this v n [js] nextInClosure (translateScopeM this nextInClosure Nothing)
            return (cvar,J.ExpName (J.Name [f]), Type t (\_ -> t1) ),

  translateApply = \m1 m2 ->
       do  (n :: Int) <- get
           put (n+1)
           (s1,j1, Forall (Type t1 g)) <- m1
           (s2,j2,t2) <- m2
           let t    = g ()
           let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
           cvarass <- getCvarAss (up this) t f j1 j2
           let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
           (s3, nje3) <- getS3 (up this) f t j3 cvarass
           return (s1 ++ s2 ++ s3, nje3, scope2ctyp t),

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
        return ([J.LocalClass (J.ClassDecl [] (J.Ident ("Fun" ++ show nextId)) []
                 (Just $ J.ClassRefType (J.ClassType [(J.Ident closureClass,[])])) [] (jexp [currentInitialDeclaration
                 (J.Ident (localvarstr ++ show currentId))] (Just (J.Block (initVars ++ statementsBeforeOA ++ [outputAssignment javaExpression]))) nextId b)),
                 J.LocalVars [] (closureType) ([J.VarDecl (J.VarId $ J.Ident (localvarstr ++ show nextId)) (Just (J.InitExp (instCreat nextId)))])],t1),


  genApply = \f t x y -> return [J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall (J.ExpName (J.Name [f])) [] (J.Ident "apply") [])))],

  genRes = \t -> return,

  getCvarAss = \t f j1 j2 ->
     return [ J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp j1))]),
              J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident localvarstr))) J.EqualA j2) )],

  genClone = return False, -- do not generate clone method

  createWrap = \name exp ->
        do (bs,e,t) <- translateM this exp
           let returnType = case t of JClass "java.lang.Integer" -> Just $ J.PrimType $ J.IntT
                                      _ -> Just $ objType
           let classDecl = getClassDecl name bs ([J.BlockStmt (J.Return $ Just e)]) returnType mainbody
           return (createCUB this [classDecl], t)

    }
