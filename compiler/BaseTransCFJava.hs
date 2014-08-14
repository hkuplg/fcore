{-# OPTIONS -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XScopedTypeVariables #-}

module BaseTransCFJava where
-- translation that does not pre-initialize Closures that are ininitalised in apply() methods of other Closures
import Prelude hiding (init, last)

import ESF.Syntax
import qualified Language.Java.Syntax as J
import ClosureF
import Inheritance
import StringPrefixes
import MonadLib

instance (:<) (Translate m) (Translate m) where
   up = id

type InitVars = [J.BlockStmt]                 

-- Closure F to Java

var x = J.ExpName (J.Name [J.Ident x])

jbody = Just (J.Block [])

init = [J.InitDecl False (J.Block [])]

closureType = J.RefType (J.ClassRefType (J.ClassType [(J.Ident "Closure",[])]))
objType = J.RefType (J.ClassRefType (J.ClassType [(J.Ident "Object",[])]))
boxedIntType = J.RefType (J.ClassRefType (J.ClassType [(J.Ident "Integer",[])]))
objArrayType = J.RefType (J.ArrayType (J.RefType (J.ClassRefType (J.ClassType [(J.Ident "Object",[])]))))

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

{-
closureClass = J.ClassTypeDecl (J.ClassDecl [J.Abstract] (J.Ident "Closure") [] Nothing [] (
               J.ClassBody [field localvarstr,field "out",app [J.Abstract] Nothing Nothing "apply" [] {-,app [J.Public,J.Abstract] Nothing (Just closureType) "clone" []-}]))
-}

applyCall = J.BlockStmt (J.ExpStmt (J.MethodInv (J.MethodCall (J.Name [J.Ident "apply"]) [])))

refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])

mainArgType = [J.FormalParam [] (J.RefType $ J.ArrayType (J.RefType (refType "String"))) False (J.VarId (J.Ident "args"))]
mainbody = Just (J.Block [J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall
    (J.ExpName (J.Name [J.Ident "System.out"])) [] (J.Ident "println") [J.ExpName $ J.Name [J.Ident ("apply" ++ "()")]])))])

createCUB this compDef = cu where
   cu = J.CompilationUnit Nothing [] ([closureClass this] ++ compDef)

getClassDecl className bs ass returnType mainbodyDef = J.ClassTypeDecl (J.ClassDecl [J.Public] (J.Ident className) [] (Nothing) []
    (J.ClassBody [app [J.Static] body returnType "apply" [], app [J.Public, J.Static] mainbodyDef Nothing "main" mainArgType]))
    where
        body = Just (J.Block (bs ++ ass))

reduceTTuples :: [([a], J.Exp, PCTyp t)] -> ([a], J.Exp, PCTyp t)
reduceTTuples all = (merged, arrayAssignment, tupleType)
    where
        merged = concat $ map (\x -> case x of (a,b,c) -> a) all
        arrayAssignment = J.ArrayCreateInit (objType) 1 (J.ArrayInit (map (\x -> case x of (a,b,c) -> J.InitExp b) all))
        tupleType = CTupleType (map (\x -> case x of (a,b,c) -> c) all)

initStuff tempvarstr n j t = J.LocalVars [J.Final] (t) ([J.VarDecl (J.VarId $ J.Ident (tempvarstr ++ show n)) (Just exp)])
    where
        exp | t == objType = J.InitExp j
            | otherwise = J.InitExp $ J.Cast t j

initIntCast tempvarstr n j = initStuff tempvarstr n j boxedIntType

initObj tempvarstr n j = initStuff tempvarstr n j objType

initClosure tempvarstr n j = initStuff tempvarstr n j closureType

initObjArray tempvarstr n j = initStuff tempvarstr n j objArrayType

type Var = Int -- Either Int Int left -> standard variable; right -> recursive variable

last (Typ _ _) = False
last (Kind f)  = last (f 0)
last (Body _)  = True

instCreat i = J.InstanceCreation [] (J.ClassType [(J.Ident ("Fun" ++ show i),[])]) [] Nothing

jexp init body idCF generateClone =
       J.ClassBody (init ++  [J.MemberDecl (J.MethodDecl [] [] Nothing (J.Ident "apply") [] [] (J.MethodBody body))] ++
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
     PCExp Int (Var, PCTyp Int) ->
     m ([J.BlockStmt], J.Exp, PCTyp Int),
  translateScopeM ::
    Scope (PCExp Int (Var, PCTyp Int)) Int (Var, PCTyp Int) ->
    Maybe (Int,PCTyp Int) ->
    m ([J.BlockStmt], J.Exp, TScope Int),
  translateApply ::  m ([J.BlockStmt], J.Exp, PCTyp Int) ->  m ([J.BlockStmt], J.Exp, PCTyp Int) ->  m ([J.BlockStmt], J.Exp, PCTyp Int),
  translateIf ::  m ([J.BlockStmt], J.Exp, PCTyp Int) -> m ([J.BlockStmt], J.Exp, PCTyp Int) ->  m ([J.BlockStmt], J.Exp, PCTyp Int) ->  m ([J.BlockStmt], J.Exp, PCTyp Int),
  translateScopeTyp :: Int -> Int -> [J.BlockStmt] -> Scope (PCExp Int (Var, PCTyp Int)) Int (Var, PCTyp Int) -> m ([J.BlockStmt], J.Exp, TScope Int) -> m ([J.BlockStmt], TScope Int), 
  genApply :: J.Ident -> TScope Int -> J.Exp -> J.Type -> m [J.BlockStmt],
  genRes :: TScope Int -> [J.BlockStmt] -> m [J.BlockStmt],
  genClone :: m Bool,
  getCvarAss :: TScope Int -> J.Ident -> J.Exp -> J.Exp -> m [J.BlockStmt],
  -- getS3 :: TScope Int -> J.Exp -> (J.Exp -> J.Type -> [J.BlockStmt]) -> ([J.BlockStmt] -> [J.BlockStmt]) -> [J.BlockStmt] -> m ([J.BlockStmt], J.Exp)
  createWrap :: String -> PCExp Int (Var, PCTyp Int) -> m (J.CompilationUnit, PCTyp Int),
  closureClass :: J.TypeDecl
  }

chooseCastBox (CJClass "java.lang.Integer") = (initIntCast,boxedIntType)
chooseCastBox (CForall _)       = (initClosure,closureType)
chooseCastBox (CTupleType [t])  = chooseCastBox t -- optimization for tuples of size 1
chooseCastBox (CTupleType _)    = (initObjArray,objArrayType)
chooseCastBox _                 = (initObj,objType)

javaType (CJClass "java.lang.Integer") = boxedIntType
javaType (CForall _)     = closureType
javaType (CTupleType [t]) = javaType t -- optimization for tuples of size 1
javaType (CTupleType _)  = objArrayType
javaType _               = objType

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
            let ifresdecl = J.LocalVars [] (javaType t2) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Nothing)])
            let (ifstmt, ifexp) = ifBody (s2, s3) (j1, j2, j3) n  -- uses a fresh variable
            return (s1 ++ [ifresdecl,ifstmt], ifexp, t2)  -- need to check t2 == t3   

--(J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "c",J.Ident localvarstr])) J.EqualA

assignVar varId e t = J.LocalVars [] (javaType t) [J.VarDecl (J.VarId $ J.Ident varId) (Just (J.InitExp e))]

fieldAccess varId fieldId = J.FieldAccess $ J.PrimaryFieldAccess (J.ExpName (J.Name [J.Ident $ varId])) (J.Ident fieldId)

inputFieldAccess varId = fieldAccess varId localvarstr

trans :: (MonadState Int m, selfType :< Translate m) => Base selfType (Translate m)
trans self = let this = up self in T {
  translateM = \e -> case e of
     CVar (i, t) ->
       do return ([],var (localvarstr ++ show i), t)

     CFLit lit -> case lit of (Integer i) -> return ([], J.Lit $ J.Int i, CJClass "java.lang.Integer")
                              (String s)  -> return ([], J.Lit $ J.String s, CJClass "java.lang.String")
                              (Boolean b) -> return ([], J.Lit $ J.Boolean b, CJClass "java.lang.String")

     CFPrimOp e1 op e2 -> -- Int -> Int -> Int only for now!
       do  (n :: Int) <- get
           put (n+1)
           (s1,j1,t1) <- translateM this e1
           (s2,j2,t2) <- translateM this e2
           let (je, typ) = case op of (Arith realOp)   -> (J.BinOp j1 realOp j2, CJClass "java.lang.Integer")
                                      (Compare realOp) -> (J.BinOp j1 realOp j2, CJClass "java.lang.Boolean")
                                      (Logic realOp)   -> (J.BinOp j1 realOp j2, CJClass "java.lang.Boolean")
           return (s1 ++ s2 ++ [assignVar (localvarstr ++ show n) je typ], 
                   var (localvarstr ++ show n), typ)  -- type being returned will be wrong for operators like "<"

     CFIf0 e1 e2 e3 -> translateIf this (translateM this e1) (translateM this e2) (translateM this e3) 
     
     -- A simple optimization for tuples of size 1 (DOES NOT NEED TO BE FORMALIZED)     
     CFTuple [e] -> 
       do  (s1,j1,t1) <- translateM this e
           return (s1,j1,CTupleType [t1])

     -- otherwise: (not optimized)
     CFTuple tuple ->
       liftM reduceTTuples $ mapM (translateM this) tuple

     CFProj i e ->
       do (s1,j1,t) <- translateM this e
          case t of 
             -- A simple optimization for tuples of size 1 (DOES NOT NEED TO BE FORMALIZED) 
             CTupleType [t] -> return (s1,j1,t)
             -- otherwise: (not optimized)
             CTupleType ts  -> 
               let fj = J.ArrayAccess (J.ArrayIndex j1 (J.Lit (J.Int $ toInteger (i-1)))) 
               in return (s1, J.Cast (javaType (ts!!(i-1))) fj, ts!!(i-1))
             otherwise -> error "expected tuple type"

     CTApp e t ->
       do  n <- get
           (s,je, CForall (Kind f)) <- translateM this e
           return (s,je, scope2ctyp (substScope n t (f n)))
    -- TODO: CLam and CFix generation of top-level Fun closures is a bit ad-hoc transformation from the old generated code + duplicate code
     CLam se ->
       do  (n :: Int) <- get
           (s,je, t) <- translateScopeM this se Nothing
           return (s,je, CForall t)

     CFix t s   ->
       do  (n :: Int) <- get
           put (n+1)
           (s, je, t') <- translateScopeM this (s (n,t)) (Just (n,t)) -- weird!
           return (s,je, CForall t')

     CApp e1 e2 -> translateApply this (translateM this e1) (translateM this e2),

  translateScopeM = \e m -> case e of
      Body t ->
        do  (s,je, t1) <- translateM this t
            return (s,je, Body t1)

      Kind f ->
        do  n <- get
            put (n+1) -- needed?
            (s,je,t1) <- translateScopeM this (f n) m
            return (s,je, Kind (\a -> substScope n (CTVar a) t1))

      Typ t g ->
        do  n <- get
            let f       = J.Ident (localvarstr ++ show n) -- use a fresh variable
            let (v,n')  = maybe (n+1,n+2) (\(i,_) -> (i,n+1)) m -- decide whether we have found the fixpoint closure or not
            put (n' + 1)
            let nextInClosure = g (n',t)
            let js = (initStuff localvarstr n' (inputFieldAccess (localvarstr ++ show v)) (javaType t)) 
            (cvar,t1) <- translateScopeTyp this v n [js] nextInClosure (translateScopeM this nextInClosure Nothing) 
            return (cvar,J.ExpName (J.Name [f]), Typ t (\_ -> t1) ),

  translateApply = \m1 m2 -> 
       do  (n :: Int) <- get
           put (n+1)
           (s1,j1, CForall (Typ t1 g)) <- m1
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
            let j1' = J.BinOp j1 J.Equal (J.Lit (J.Int 0))
            genIfBody this m2 m3 j1' s1 n,

  translateScopeTyp = \currentId nextId initVars nextInClosure m ->
     do b <- genClone this
        (statementsBeforeOA,javaExpression,t1) <- m
        return ([(J.LocalClass (J.ClassDecl [] (J.Ident ("Fun" ++ show nextId)) []
                 (Just $ J.ClassRefType (J.ClassType [(J.Ident "Closure",[])])) [] (jexp [currentInitialDeclaration
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
           let returnType = case t of CJClass "java.lang.Integer" -> Just $ J.PrimType $ J.IntT
                                      _ -> Just $ objType
           let classDecl = getClassDecl name bs ([J.BlockStmt (J.Return $ Just e)]) returnType mainbody
           return (createCUB this [classDecl], t),
   
  closureClass = J.ClassTypeDecl (J.ClassDecl [J.Abstract] (J.Ident "Closure") [] Nothing [] (
                 J.ClassBody [field localvarstr,field "out",app [J.Abstract] Nothing Nothing "apply" [] {-,app [J.Public,J.Abstract] Nothing (Just closureType) "clone" []-}]))

    }
