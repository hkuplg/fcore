{-# OPTIONS -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XScopedTypeVariables #-}

module BaseTransCFJava where
-- translation that does not pre-initialize Closures that are ininitalised in apply() methods of other Closures

import           ClosureF
import           Inheritance
import           JavaEDSL
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

-- jbody :: Maybe J.Block
-- jbody = Just (J.Block [])

-- init :: [J.Decl]
-- init = [J.InitDecl False (J.Block [])]

closureType :: J.Type
closureType = classTy closureClass

objClassTy :: J.Type
objClassTy = classTy "Object"

-- objArrayType :: J.Type
-- objArrayType = arrayTy objType

-- field :: String -> J.Decl
-- field name = memberDecl $ fieldDecl [] objType [varDecl name Nothing]

mainArgType :: [J.FormalParam]
mainArgType =
  [paramDecl (arrayTy $
              classTy "String")
             "args"]

mainBody :: Maybe J.Block
mainBody =
  Just (block [bStmt $
               classMethodCall (var "System.out")
                               "println"
                               [var "apply()"]])

createCUB :: t -> [J.TypeDecl] -> J.CompilationUnit
createCUB _ compDef = cu
  where cu = J.CompilationUnit Nothing [] compDef

--consPrimList :: [([a], J.Exp, PCTyp t)] -> ([a], J.Exp, PCTyp t)
--consPrimList l = case l of

initStuff :: String -> Int -> J.Exp -> J.Type -> J.BlockStmt
initStuff tempVar n expr ty =
  localVars [J.Final]
            ty
            [varDecl tempName (Just initValue)]
  where tempName = tempVar ++ show n
        initValue
          | ty == objClassTy = J.InitExp expr
          | otherwise =
            J.InitExp (cast ty expr)


initClass :: String -> String -> Int -> J.Exp -> J.BlockStmt
initClass className tempVarStr n expr =
  initStuff tempVarStr n expr (classTy className)

-- initObj :: String -> Int -> J.Exp -> J.BlockStmt
-- initObj tempVarStr n expr =
--   initStuff tempVarStr n expr objClassTy

-- initClosure :: String -> Int -> J.Exp -> J.BlockStmt
-- initClosure tempVarStr n expr =
--   initStuff tempVarStr n expr closureType

-- initObjArray :: String -> Int -> J.Exp -> J.BlockStmt
-- initObjArray tempVarStr n expr =
--   initStuff tempVarStr n expr objArrayType

--initPrimList tempvarstr n j = initClassCast primListClass tempvarstr n j

type Var = Int -- Either Int Int left -> standard variable; right -> recursive variable

-- instCreat :: Int -> J.Exp
-- instCreat i = J.InstanceCreation [] (J.ClassType [(J.Ident ("Fun" ++ show i),[])]) [] Nothing

funInstCreate :: Int -> J.Exp
funInstCreate i = instCreat fun []
  where fun = (J.ClassType [(J.Ident ("Fun" ++ show i),[])])

jexp :: [J.Decl] -> Maybe J.Block -> Int -> Bool -> J.ClassBody
jexp initDecls body idCF generateClone =
  classBody $ initDecls ++
  [applyMethod] ++
  if generateClone
     then [cloneMethod]
     else []
  where applyMethod =
          J.MemberDecl $
          methodDecl [J.Public]
                     Nothing
                     "apply"
                     []
                     body
        cloneMethod =
          J.MemberDecl $
          methodDecl [J.Public]
                     (Just closureType)
                     "clone"
                     []
                     cloneBody
        cloneBody =
          Just (block [localVars []
                                 closureType
                                 [varDecl "c"
                                          (Just $ J.InitExp $ funInstCreate idCF)]
                      ,bStmt (J.ExpStmt (assign (names ["c",localvarstr])
                                                J.EqualA
                                                (J.ExpName $
                                                 names ["this",localvarstr])))
                      ,bStmt (classMethodCall (var "c")
                                              "apply"
                                              [])
                      ,bStmt (J.Return (Just (cast closureType (var "c"))))])

currentInitialDeclaration :: J.Ident -> J.Decl
currentInitialDeclaration idCurrentName =
  J.MemberDecl $
  J.FieldDecl
    []
    closureType
    [J.VarDecl (J.VarId idCurrentName)
               (Just (J.InitExp J.This))]

outputAssignment :: J.Exp -> J.BlockStmt
outputAssignment javaExpression =
  J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (names ["out"])) J.EqualA javaExpression))

{-
translateScopeTyp javaExpression statementsBeforeOA currentId nextId initVars generateClone =
     [(J.LocalClass (J.ClassDecl [] (J.Ident ("Fun" ++ show nextId)) []
        (Just $ J.ClassRefType (J.ClassType [(J.Ident "Closure",[])])) [] (jexp [currentInitialDeclaration
        (J.Ident (localvarstr ++ show currentId))] (Just (J.Block (initVars ++ statementsBeforeOA ++ [outputAssignment javaExpression]))) nextId generateClone))),
        J.LocalVars [] (closureType) ([J.VarDecl (J.VarId $ J.Ident (localvarstr ++ show nextId)) (Just (J.InitExp (instCreat nextId)))])]
-}

type TransType = ([J.BlockStmt], J.Exp, Type Int)

data Translate m =
  T {translateM :: Expr Int (Var,Type Int) -> m ([J.BlockStmt],J.Exp,Type Int)
    ,translateScopeM :: Scope (Expr Int (Var,Type Int)) Int (Var,Type Int) -> Maybe (Int,Type Int) -> m ([J.BlockStmt],J.Exp,TScope Int)
    ,translateApply :: m TransType -> m TransType -> m TransType
    ,translateIf :: m TransType -> m TransType -> m TransType -> m TransType
    ,translateScopeTyp :: Int -> Int -> [J.BlockStmt] -> Scope (Expr Int (Var,Type Int)) Int (Var,Type Int) -> m ([J.BlockStmt],J.Exp,TScope Int) -> m ([J.BlockStmt],TScope Int)
    ,genApply :: J.Ident -> TScope Int -> J.Exp -> J.Type -> m [J.BlockStmt]
    ,genRes :: TScope Int -> [J.BlockStmt] -> m [J.BlockStmt]
    ,genClone :: m Bool
    ,getCvarAss :: TScope Int -> J.Ident -> J.Exp -> J.Exp -> m [J.BlockStmt]
     -- getS3 :: TScope Int -> J.Exp -> (J.Exp -> J.Type -> [J.BlockStmt]) -> ([J.BlockStmt] -> [J.BlockStmt]) -> [J.BlockStmt] -> m ([J.BlockStmt], J.Exp)
    ,createWrap :: String -> Expr Int (Var,Type Int) -> m (J.CompilationUnit,Type Int)}

getTupleClassName :: [a] -> String
getTupleClassName tuple =
  if lengthOfTuple > 50
    then panic "The length of tuple is too long (>50)!"
    else "hk.hku.cs.f2j.tuples.Tuple" ++ show lengthOfTuple
  where
    lengthOfTuple = length tuple

chooseCastBox :: Type t -> (String -> Int -> J.Exp -> J.BlockStmt, J.Type)
chooseCastBox (JClass c) =
  (initClass c,classTy c)
chooseCastBox (CFInt) =
  (initClass "java.lang.Integer",classTy "java.lang.Integer")
chooseCastBox (CFInteger) =
  (initClass "java.lang.Integer",classTy "java.lang.Integer")
chooseCastBox (Forall _) =
  (initClass closureClass,closureType)
chooseCastBox (TupleType tuple) =
  case tuple of
    [t] -> chooseCastBox t
    _ ->
      (initClass tupleClassName,classTy tupleClassName)
      where tupleClassName = getTupleClassName tuple
chooseCastBox _ = (initClass "Object",objClassTy)

javaType (JClass c)        = classTy c
javaType (Forall _)        = closureType
javaType (TupleType tuple) = case tuple of [t] -> javaType t
                                           _   -> classTy $ getTupleClassName tuple
javaType CFInt             = classTy "java.lang.Integer"
javaType CFInteger         = classTy "java.lang.Integer"
javaType _                 = objClassTy

getS3 :: MonadState Int m => Translate m -> J.Ident -> TScope Int -> J.Exp -> [J.BlockStmt] -> m ([J.BlockStmt], J.Exp)
getS3 this func t j3 cvarass =
  do (n :: Int) <- get
     put (n+1)
     let (cast,typ) = chooseCastBox (scope2ctyp t)
     apply <- genApply this func t (var (tempvarstr ++ show n)) typ
     rest <- genRes this t [cast tempvarstr n j3]
     let r = cvarass ++ apply ++ rest
     return (r, var (tempvarstr ++ show n))

ifBody :: ([J.BlockStmt],[J.BlockStmt])
       -> (J.Exp,J.Exp,J.Exp)
       -> Int
       -> (J.BlockStmt,J.Exp)
ifBody (s2,s3) (j1,j2,j3) n =
  (bStmt $
   J.IfThenElse
     j1
     (J.StmtBlock $
      block (s2 ++ j2Stmt))
     (J.StmtBlock $
      block (s3 ++ j3Stmt))
  ,newvar)
  where j2Stmt = [localVarDecl j2]
        j3Stmt = [localVarDecl j3]
        localVarDecl e =
          J.LocalVars []
                      (classTy "")
                      [varDecl ifVarName (Just (J.InitExp e))]
        ifVarName = (ifresultstr ++ show n)
        newvar = var ifVarName

genIfBody :: Monad m
          => t
          -> m ([J.BlockStmt],J.Exp,Type t2)
          -> m ([J.BlockStmt],J.Exp,t1)
          -> J.Exp
          -> [J.BlockStmt]
          -> Int
          -> m ([J.BlockStmt],J.Exp,Type t2)
genIfBody _ m2 m3 j1 s1 n =
  do (s2,j2,t2) <- m2 {-translateM this e2-}
     (s3,j3,_) <- m3 {-translateM this e3-}
     let ifvarname = (ifresultstr ++ show n)
     let ifresdecl =
           J.LocalVars
             []
             (javaType t2)
             [J.VarDecl (J.VarId $
                         J.Ident ifvarname)
                        (Nothing)]
     let (ifstmt,ifexp) =
           ifBody (s2,s3)
                  (j1,j2,j3)
                  n -- uses a fresh variable
     return (s1 ++
             [ifresdecl,ifstmt]
            ,ifexp
            ,t2) -- need to check t2 == t3

--(J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "c",J.Ident localvarstr])) J.EqualA

assignVar :: String -> J.Exp -> Type t -> J.BlockStmt
assignVar varId e t = J.LocalVars [] (javaType t) [varDecl varId (Just (J.InitExp e))]

fieldAccess :: String -> String -> J.Exp
fieldAccess varId fieldId = J.FieldAccess $ J.PrimaryFieldAccess
                                              (var varId)
                                              (J.Ident fieldId)

inputFieldAccess :: String -> J.Exp
inputFieldAccess varId = fieldAccess varId localvarstr

pairUp :: [Var] -> [(J.Exp, Type Int)] -> [(Var, Type Int)]
pairUp bindings vars = exchanged
    where
      z = bindings `zip` vars
      exchanged = map (\(a, (_, c)) -> (a, c)) z

toTupleOf3Lists :: [(a, b, c)] -> ([a], [b], [c])
toTupleOf3Lists list = (first, second, third)
  where
    first  = map (\(x, _, _) -> x) list
    second = map (\(_, y, _) -> y) list
    third  = map (\(_, _, z) -> z) list

concatFirst :: ([[a]], [b], [c]) -> ([a], [b], [c])
concatFirst (xs, y, z) = (concat xs, y, z)

getNewVarName this = do (n :: Int) <- get
                        put (n + 1)
                        return $ localvarstr ++ show n

mainClass :: String
          -> [J.BlockStmt]
          -> Maybe J.Type
          -> Maybe J.Block
          -> J.TypeDecl
mainClass className stmts returnType mainbodyDef =
  J.ClassTypeDecl
    (classDecl [J.Public]
               className
               (classBody [memberDecl $
                           methodDecl [J.Static]
                                      returnType
                                      "apply"
                                      []
                                      body
                          ,memberDecl $
                           methodDecl [J.Public,J.Static]
                                      Nothing
                                      "main"
                                      mainArgType
                                      mainbodyDef]))
  where body = Just (block stmts)


trans :: (MonadState Int m, selfType :< Translate m) => Base selfType (Translate m)
trans self =
  let this = up self
  in T {translateM =
          \e ->
            case e of
              Var (i,t) ->
                return ([],var (localvarstr ++ show i),t)
              Lit lit ->
                case lit of
                  (Src.Integer i) ->
                    return ([]
                           ,J.Lit $
                            J.Int i
                           ,JClass "java.lang.Integer")
                  (Src.String s) ->
                    return ([]
                           ,J.Lit $
                            J.String s
                           ,JClass "java.lang.String")
                  (Src.Boolean b) ->
                    return ([]
                           ,J.Lit $
                            J.Boolean b
                           ,JClass "java.lang.Boolean")
                  (Src.Char c) ->
                    return ([]
                           ,J.Lit $
                            J.Char c
                           ,JClass "java.lang.Character")
              PrimOp e1 op e2 ->
                do (s1,j1,_) <- translateM this e1
                   (s2,j2,_) <- translateM this e2
                   let (je,typ) =
                         case op of
                           (Src.Arith realOp) ->
                             (J.BinOp j1 realOp j2,JClass "java.lang.Integer")
                           (Src.Compare realOp) ->
                             (J.BinOp j1 realOp j2,JClass "java.lang.Boolean")
                           (Src.Logic realOp) ->
                             (J.BinOp j1 realOp j2,JClass "java.lang.Boolean")
                   newVarName <- getNewVarName this
                   return (s1 ++ s2 ++
                           [assignVar newVarName je typ]
                          ,var newVarName
                          ,typ)
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
                       let (statements,exprs,types) = concatFirst $
                                                      toTupleOf3Lists tuple'
                       newVarName <- getNewVarName this
                       let c = getTupleClassName tuple
                       let rhs =
                             J.InstanceCreation []
                                                (J.ClassType [(J.Ident c,[])])
                                                exprs
                                                Nothing
                       return (statements ++
                               [assignVar newVarName rhs (JClass c)]
                              ,var newVarName
                              ,TupleType types)
              Proj index expr ->
                do ret@(statement,javaExpr,exprType) <- translateM this expr
                   case exprType of
                     TupleType [_] -> return ret
                     TupleType types ->
                       do newVarName <- getNewVarName this
                          let typ =
                                (types !!
                                 (index - 1))
                          let rhs =
                                J.Cast (javaType typ) $
                                J.FieldAccess $
                                J.PrimaryFieldAccess javaExpr
                                                     (J.Ident ("_" ++ show index))
                          return (statement ++
                                  [assignVar newVarName rhs typ]
                                 ,var newVarName
                                 ,typ)
                     _ ->
                       panic "BaseTransCFJava.trans: expected tuple type"
              TApp e t ->
                do n <- get
                   (s,je,Forall (Kind f)) <- translateM this e
                   return (s
                          ,je
                          ,scope2ctyp
                             (substScope n
                                         t
                                         (f n)))
                   -- TODO: CLam and CFix generation of top-level Fun closures is a bit ad-hoc transformation from the old generated code + duplicate code
              Lam se ->
                do (s,je,t) <- translateScopeM this se Nothing
                   return (s,je,Forall t)
              Fix t s ->
                do (n :: Int) <- get
                   put (n + 1)
                   (s,je,t') <- translateScopeM this
                                                (s (n,t))
                                                (Just (n,t)) -- weird!
                   return (s,je,Forall t')
              LetRec t xs body ->
                do (n :: Int) <- get
                   let needed =
                         length $
                         (xs (zip [n ..]
                                  (repeat (t !! 0))))
                   put (n + 2 + needed)
                   mfuns <- return (\defs ->
                                      forM (xs defs)
                                           (translateM this))
                   let vars =
                         (liftM (map (\(_,b,c) ->
                                        (b,c))))
                           (mfuns (zip [n ..]
                                       (repeat (t !! 0))))
                   let (bindings :: [Var]) =
                         [n + 2 .. n + 1 + needed]
                   newvars <- ((liftM (pairUp bindings)) vars)
                   let mDecls =
                         map (\x ->
                                J.MemberDecl
                                  (J.FieldDecl
                                     [J.Public]
                                     (closureType)
                                     [J.VarDecl (J.VarId (J.Ident (localvarstr ++
                                                                   show x)))
                                                Nothing]))
                             bindings
                   let finalFuns = mfuns newvars
                   let appliedBody = body newvars
                   let tnv = map fst newvars
                   (s,je,t') <- translateM this appliedBody
                   (stmts,jex,_) <- ((liftM (unzip3)) (finalFuns))
                   let typ = javaType t'
                   let assm =
                         map (\(i,jz) ->
                                J.BlockStmt
                                  (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident (localvarstr ++
                                                                                    show i)]))
                                                       J.EqualA
                                                       jz)))
                             (tnv `zip` jex)
                   let stasm =
                         (concatMap (\(a,b) ->
                                       a ++
                                       [b])
                                    (stmts `zip` assm)) ++
                         s ++
                         [J.BlockStmt
                            (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "out"])) J.EqualA je))]
                   let letClass =
                         [J.LocalClass
                            (J.ClassDecl
                               []
                               (J.Ident ("Let" ++ show n))
                               []
                               Nothing
                               []
                               (J.ClassBody
                                  (J.MemberDecl
                                     (J.FieldDecl [J.Public]
                                                  objClassTy
                                                  [J.VarDecl (J.VarId (J.Ident "out")) Nothing]) :
                                   mDecls ++
                                   [J.InitDecl False
                                               (J.Block stasm)])))
                         ,J.LocalVars
                            []
                            (J.RefType (J.ClassRefType
                                          (J.ClassType
                                             [(J.Ident ("Let" ++ show n),[])])))
                            [J.VarDecl (J.VarId (J.Ident (localvarstr ++ show n)))
                                       (Just (J.InitExp (J.InstanceCreation
                                                           []
                                                           (J.ClassType
                                                              [(J.Ident ("Let" ++
                                                                         show n)
                                                               ,[])])
                                                           []
                                                           Nothing)))]
                         ,J.LocalVars
                            []
                            typ
                            [J.VarDecl (J.VarId (J.Ident (localvarstr ++
                                                          show (n + 1))))
                                       (Just (J.InitExp (J.Cast (typ)
                                                                (J.ExpName (J.Name [J.Ident (localvarstr ++
                                                                                             show n)
                                                                                   ,J.Ident "out"])))))]]
                   return (letClass
                          ,var (localvarstr ++
                                show (n + 1))
                          ,t')
              App e1 e2 ->
                translateApply this
                               (translateM this e1)
                               (translateM this e2)
              -- InstanceCreation [TypeArgument] ClassType [Argument] (Maybe ClassBody)
              JNewObj c args ->
                do args' <- mapM (translateM this) args
                   let (statements,exprs,types) = concatFirst $
                                                  toTupleOf3Lists args'
                   let rhs =
                         J.InstanceCreation
                           (map (\(JClass x) ->
                                   J.ActualType $ J.ClassRefType $
                                   J.ClassType [(J.Ident x,[])])
                                types)
                           (J.ClassType [(J.Ident c,[])])
                           exprs
                           Nothing
                   let typ = JClass c
                   newVarName <- getNewVarName this
                   return (statements ++
                           [assignVar newVarName rhs typ]
                          ,var newVarName
                          ,typ)
              JMethod c m args r ->
                do args' <- mapM (translateM this) args
                   let (statements,exprs,types) = concatFirst $
                                                  toTupleOf3Lists args'
                   let refTypes =
                         (map (\y ->
                           case y of JClass x ->
                                       J.ClassRefType $
                                       J.ClassType [(J.Ident x,[])]
                                     CFInt ->
                                       J.ClassRefType $
                                       J.ClassType [(J.Ident "java.lang.Integer",[])]
                                     CFInteger ->
                                       J.ClassRefType $
                                       J.ClassType [(J.Ident "java.lang.Integer",[])]
                                 )
                              types)
                   (classStatement,rhs) <- case c of
                                             (Right ce) ->
                                               do (classS,classE,_) <- translateM this ce
                                                  return (classS
                                                         ,J.MethodInv $
                                                          J.PrimaryMethodCall classE
                                                                              refTypes
                                                                              (J.Ident m)
                                                                              exprs)
                                             (Left cn) ->
                                               return ([]
                                                      ,J.MethodInv $
                                                       J.TypeMethodCall (J.Name [J.Ident cn])
                                                                        refTypes
                                                                        (J.Ident m)
                                                                        exprs)
                   let typ = JClass r
                   if r /= "java.lang.Void"
                      then do newVarName <- getNewVarName this
                              return (statements ++ classStatement ++
                                      [assignVar newVarName rhs typ]
                                     ,var newVarName
                                     ,typ)
                      else return (statements ++ classStatement ++
                                   [(J.BlockStmt $
                                     J.ExpStmt rhs)]
                                  ,rhs
                                  ,typ)
              JField c fName r ->
                do (classStatement,classExpr,_) <- case c of
                                                     (Right ce) ->
                                                       translateM this ce
                                                     (Left cn) ->
                                                       return ([]
                                                              ,J.ExpName $
                                                               J.Name [J.Ident cn]
                                                              ,undefined)
                   newVarName <- getNewVarName this
                   let typ = JClass r
                   let rhs =
                         J.Cast (javaType typ) $
                         J.FieldAccess $
                         J.PrimaryFieldAccess classExpr
                                              (J.Ident fName)
                   return (classStatement ++
                           [assignVar newVarName rhs typ]
                          ,var newVarName
                          ,typ)
              SeqExprs es ->
                do es' <- mapM (translateM this) es
                   let (_,lastExp,lastType) = last es'
                   let statements =
                         concat $
                         map (\(x,_,_) -> x) es'
                   return (statements,lastExp,lastType)
       ,translateScopeM =
          \e m ->
            case e of
              Body t ->
                do (s,je,t1) <- translateM this t
                   return (s,je,Body t1)
              Kind f ->
                do n <- get
                   put (n + 1) -- needed?
                   (s,je,t1) <- translateScopeM this
                                                (f n)
                                                m
                   return (s
                          ,je
                          ,Kind (\a ->
                                   substScope n
                                              (TVar a)
                                              t1))
              Type t g ->
                do n <- get
                   let f =
                         J.Ident (localvarstr ++ show n) -- use a fresh variable
                   let (v,n') =
                         maybe (n + 1,n + 2)
                               (\(i,_) ->
                                  (i,n + 1))
                               m -- decide whether we have found the fixpoint closure or not
                   put (n' + 1)
                   let nextInClosure = g (n',t)
                   let js =
                         (initStuff localvarstr
                                    n'
                                    (inputFieldAccess (localvarstr ++ show v))
                                    (javaType t))
                   (cvar,t1) <- translateScopeTyp this
                                                  v
                                                  n
                                                  [js]
                                                  nextInClosure
                                                  (translateScopeM this nextInClosure Nothing)
                   return (cvar,J.ExpName (J.Name [f]),Type t (\_ -> t1))
       ,translateApply =
          \m1 m2 ->
            do (n :: Int) <- get
               put (n + 1)
               (s1,j1,Forall (Type t1 g)) <- m1
               (s2,j2,t2) <- m2
               let t = g ()
               let f =
                     J.Ident (localvarstr ++ show n) -- use a fresh variable
               cvarass <- getCvarAss (up this)
                                     t
                                     f
                                     j1
                                     j2
               let j3 =
                     (J.FieldAccess
                        (J.PrimaryFieldAccess (J.ExpName (J.Name [f]))
                                              (J.Ident "out")))
               (s3,nje3) <- getS3 (up this) f t j3 cvarass
               return (s1 ++ s2 ++ s3,nje3,scope2ctyp t)
       ,translateIf =
          \m1 m2 m3 ->
            do n <- get
               put (n + 1)
               (s1,j1,t1) <- m1 {- translateM this e1 -}
               -- let j1' = J.BinOp j1 J.Equal (J.Lit (J.Int 0))
               -- genIfBody this m2 m3 j1' s1 n,
               genIfBody this m2 m3 j1 s1 n
       ,translateScopeTyp =
          \currentId nextId initVars nextInClosure m ->
            do b <- genClone this
               (statementsBeforeOA,javaExpression,t1) <- m
               return ([J.LocalClass
                          (J.ClassDecl
                             []
                             (J.Ident ("Fun" ++ show nextId))
                             []
                             (Just $
                              J.ClassRefType
                                (J.ClassType [(J.Ident closureClass,[])]))
                             []
                             (jexp [currentInitialDeclaration
                                      (J.Ident (localvarstr ++ show currentId))]
                                   (Just (J.Block (initVars ++
                                                   statementsBeforeOA ++
                                                   [outputAssignment javaExpression])))
                                   nextId
                                   b))
                       ,J.LocalVars
                          []
                          (closureType)
                          ([J.VarDecl (J.VarId $
                                       J.Ident (localvarstr ++ show nextId))
                                      (Just (J.InitExp (funInstCreate nextId)))])]
                      ,t1)
       ,genApply =
          \f t x y ->
            return [J.BlockStmt
                      (J.ExpStmt (J.MethodInv
                                    (J.PrimaryMethodCall (J.ExpName (J.Name [f]))
                                                         []
                                                         (J.Ident "apply")
                                                         [])))]
       ,genRes = \t -> return
       ,getCvarAss =
          \t f j1 j2 ->
            return [J.LocalVars
                      []
                      closureType
                      ([J.VarDecl (J.VarId f)
                                  (Just (J.InitExp j1))])
                   ,J.BlockStmt
                      (J.ExpStmt (J.Assign (J.FieldLhs
                                              (J.PrimaryFieldAccess (J.ExpName (J.Name [f]))
                                                                    (J.Ident localvarstr)))
                                           J.EqualA
                                           j2))]
       ,genClone = return False -- do not generate clone method
       ,createWrap =
          \name exp ->
            do (bs,e,t) <- translateM this exp
               let returnType =
                     case t of
                       JClass "java.lang.Integer" -> Just $ J.PrimType $ J.IntT
                       _ -> Just objClassTy
               let returnStmt =
                     [bStmt $ J.Return $
                      Just e]
               let mainDecl =
                     mainClass name (bs ++ returnStmt) returnType mainBody
               return (createCUB this [mainDecl],t)}
