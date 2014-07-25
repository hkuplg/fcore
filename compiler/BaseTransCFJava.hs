{-# OPTIONS -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XScopedTypeVariables #-}

module BaseTransCFJava where
-- translation that does not pre-initialize Closures that are ininitalised in apply() methods of other Closures
import Prelude hiding (init, last)

-- import Control.Monad.State
-- import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Language.Java.Syntax as J
import ClosureF
-- import Mixins
import Inheritance
import StringPrefixes
import MonadLib


{-
class (:<) (f :: (* -> *) -> *) g  where
   to :: f m -> g m
   override :: f m -> (g m -> g m) -> f m -- needed to do proper overriding of methods, when we only know we inherit from a subtype. When we know the exact type of the supertype, then this method is not needed.
-}

instance (:<) (Translate m) (Translate m) where
   up = id
--   override fm f = f fm

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

whileApplyLoop :: String -> J.Ident -> J.Type -> [J.BlockStmt]
whileApplyLoop ctemp tempOut outType = [J.LocalVars [] closureType [J.VarDecl (J.VarId $ J.Ident $ ctemp) (Nothing)],
        J.LocalVars [] outType [J.VarDecl (J.VarId tempOut) (Just (J.InitExp (J.Lit J.Null)))],
        -- this is a hack, because language-java 0.2.x removed J.Paren
        --J.Paren $ J.Assign (J.NameLhs (J.Name [J.Ident ctemp]))
        --        J.EqualA (J.ExpName (J.Name [J.Ident "Next",J.Ident "next"]))
        J.BlockStmt (J.While (J.BinOp (J.ExpName $ J.Name [J.Ident ("(" ++ ctemp ++ " = " ++ "Next.next" ++ ")")])
        J.NotEq (J.Lit J.Null)) (J.StmtBlock (J.Block
        [J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "Next",J.Ident "next"]))
        J.EqualA (J.Lit J.Null))),
        J.BlockStmt (J.ExpStmt (J.MethodInv (J.MethodCall (J.Name [J.Ident $ ctemp,J.Ident "apply"]) []))),
        J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [tempOut]))
        J.EqualA (J.Cast outType
        (J.ExpName (J.Name [J.Ident $ ctemp,J.Ident "out"])))))])))]

containsNext :: [J.BlockStmt] -> Bool
containsNext l = foldr (||) False $ map (\x -> case x of (J.BlockStmt (J.ExpStmt (J.Assign (
                                                                J.NameLhs (J.Name [J.Ident "Next",J.Ident "next"])) J.EqualA _))) -> True
                                                         _ -> False) l

-- ad-hoc fix for final-returned expressions in Stack translation
empyClosure outExp = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "Next",J.Ident "next"])) J.EqualA 
        (J.InstanceCreation [] (J.ClassType [(J.Ident "Closure",[])]) [] (Just (J.ClassBody [J.MemberDecl (J.MethodDecl 
        [J.Annotation (J.MarkerAnnotation {J.annName = J.Name [J.Ident "Override"]}),J.Public] [] (Just (J.RefType (J.ClassRefType (J.ClassType [(J.Ident "Closure",[])])))) 
        (J.Ident "clone") [] [] (J.MethodBody (Just (J.Block [J.BlockStmt (J.Return (Just (J.Lit J.Null)))])))),
        J.MemberDecl (J.MethodDecl [J.Annotation (J.MarkerAnnotation {J.annName = J.Name [J.Ident "Override"]})] [] Nothing (J.Ident "apply") [] [] (J.MethodBody (Just (J.Block 
        [J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "out"])) J.EqualA outExp))]))))])))))

field name = J.MemberDecl (J.FieldDecl [] (objType) [
             J.VarDecl (J.VarId (J.Ident name)) Nothing])

app mod b rt en args = J.MemberDecl (J.MethodDecl mod [] (rt) (J.Ident en) args [] (J.MethodBody b))

closureClass = J.ClassTypeDecl (J.ClassDecl [J.Abstract] (J.Ident "Closure") [] Nothing [] (
               J.ClassBody [field localvarstr,field "out",app [J.Abstract] Nothing Nothing "apply" [],app [J.Public,J.Abstract] Nothing (Just closureType) "clone" []]))

nextClass = J.ClassTypeDecl (J.ClassDecl [] (J.Ident "Next") [] Nothing [] (J.ClassBody [J.MemberDecl (J.FieldDecl
        [J.Static] (closureType) [J.VarDecl (J.VarId (J.Ident "next")) (Just (J.InitExp (J.Lit J.Null)))])]))

applyCall = J.BlockStmt (J.ExpStmt (J.MethodInv (J.MethodCall (J.Name [J.Ident "apply"]) [])))

refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])

stackbody t = 
        applyCall : whileApplyLoop "c" (J.Ident "result") (case t of CInt -> boxedIntType
                                                                     _ -> objType) ++ [
               J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall
    (J.ExpName (J.Name [J.Ident "System.out"])) [] (J.Ident "println") [J.ExpName $ J.Name [J.Ident ("result")]])))]

mainArgType = [J.FormalParam [] (J.RefType $ J.ArrayType (J.RefType (refType "String"))) False (J.VarId (J.Ident "args"))]
mainbody = Just (J.Block [J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall
    (J.ExpName (J.Name [J.Ident "System.out"])) [] (J.Ident "println") [J.ExpName $ J.Name [J.Ident ("apply" ++ "()")]])))])

createCUB compDef = cu where
   cu = J.CompilationUnit Nothing [] ([closureClass] ++ compDef)

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

initStuff tempvarstr n j t = J.LocalVars [] (t) ([J.VarDecl (J.VarId $ J.Ident (tempvarstr ++ show n)) (Just (exp))])
    where
        exp | t == objType = J.InitExp j
            | otherwise = J.InitExp $ J.Cast t j

initIntCast tempvarstr n j = initStuff tempvarstr n j boxedIntType

initObj tempvarstr n j = initStuff tempvarstr n j objType

initClosure tempvarstr n j = initStuff tempvarstr n j closureType

initObjArray tempvarstr n j = initStuff tempvarstr n j objArrayType

type Var = Either Int Int -- left -> standard variable; right -> recursive variable

last (Typ _ _) = False
last (Kind f)  = last (f 0)
last (Body _)  = True

instCreat i = J.InstanceCreation [] (J.ClassType [(J.Ident ("Fun" ++ show i),[])]) [] Nothing

jexp init body idCF =
       J.ClassBody (init ++  [
          J.MemberDecl (J.MethodDecl [] [] Nothing (J.Ident "apply") [] [] (J.MethodBody body)),
          J.MemberDecl (J.MethodDecl [J.Public] [] (Just closureType) (J.Ident "clone") [] [] (J.MethodBody cloneBody))
       ])
        where
            cloneBody = Just (J.Block [J.LocalVars [] (closureType) [J.VarDecl (J.VarId (J.Ident "c"))
                (Just $ J.InitExp $ instCreat idCF)],J.BlockStmt
                (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "c",J.Ident localvarstr])) J.EqualA
                (J.ExpName (J.Name [J.Ident "this",J.Ident localvarstr])))),J.BlockStmt (J.ExpStmt (J.MethodInv
                (J.PrimaryMethodCall (J.ExpName (J.Name [J.Ident "c"])) [] (J.Ident "apply") []))),J.BlockStmt (J.Return (Just
                (J.Cast (closureType) (J.ExpName (J.Name [J.Ident "c"])))))])

currentInitialDeclaration idCurrentName = J.MemberDecl $ J.FieldDecl [] closureType [J.VarDecl (J.VarId idCurrentName) (Just (J.InitExp J.This))]
outputAssignment javaExpression = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA  javaExpression))
standardTranslation javaExpression statementsBeforeOA currentId nextId = [(J.LocalClass (J.ClassDecl [] (J.Ident ("Fun" ++ show nextId)) []
                                        (Just $ J.ClassRefType (J.ClassType [(J.Ident "Closure",[])])) [] (jexp [currentInitialDeclaration
                                        (J.Ident (localvarstr ++ show currentId))] (Just (J.Block (statementsBeforeOA ++ [outputAssignment javaExpression]))) nextId))),
                                        J.LocalVars [] (closureType) ([J.VarDecl (J.VarId $ J.Ident (localvarstr ++ show nextId)) (Just (J.InitExp (instCreat nextId)))])]

data Translate m = T {
  translateM ::
     PCExp Int (Var, PCTyp Int) ->
     m ([J.BlockStmt], J.Exp, PCTyp Int),
  translateScopeM ::
    Scope (PCExp Int (Var, PCTyp Int)) Int (Var, PCTyp Int) ->
    Maybe (Int,PCTyp Int) ->
    m ([J.BlockStmt], J.Exp, TScope Int),
  createWrap :: String -> PCExp Int (Var, PCTyp Int) -> m (J.CompilationUnit, PCTyp Int)
  }

instance Monoid Bool where
    mempty = False
    mappend a b = a

genSubst :: (MonadState Int m, MonadState (Map.Map J.Exp Int) m) => J.Exp -> ([Char] -> Int -> J.Exp -> J.BlockStmt) -> m ([J.BlockStmt], J.Exp)
genSubst j1 initFun = x
         where
             x = do (env1 :: Map.Map J.Exp Int) <- get
                    case j1 of J.Lit e -> return ([], j1)
                                   --FieldAccess (PrimaryFieldAccess...  or J.ExpName
                               _ -> case (Map.lookup j1 env1) of Just e -> return ([], var (tempvarstr ++ show e))
                                                                 Nothing -> do (n :: Int) <- get
                                                                               put (n+1)
                                                                               let temp1 = var (tempvarstr ++ show n)
                                                                               put (Map.insert j1 n env1)
                                                                               let defV1 = initFun tempvarstr n j1
                                                                               return ([defV1], temp1)

getS3 t j3 genApply genRes cvarass  = case (scope2ctyp t) of
                                        CInt -> do 
                                                (result, rj) <- genSubst j3 initIntCast
                                                let apply = genApply rj boxedIntType
                                                let rest = genRes result                                                
                                                let r = cvarass ++ apply ++ rest
                                                return r
                                        CForall (_) -> do 
                                                (result, rj) <- genSubst j3 initClosure
                                                let apply = genApply rj closureType
                                                let rest = genRes result                                                           
                                                let r = cvarass ++ apply ++ rest
                                                return r
                                        CTupleType _ -> do 
                                                (result, rj) <- genSubst j3 initObjArray
                                                let apply = genApply rj objArrayType
                                                let rest = genRes result        
                                                let r = cvarass ++ apply ++ rest
                                                return r
                                        _ -> do
                                                (result, rj) <- genSubst j3 initObj
                                                let apply = genApply rj objType
                                                let rest = genRes result                                                           
                                                let r = cvarass ++ apply ++ rest
                                                return r                                                      

getCvarAss t f n j1 j2 = do
                   (env :: Map.Map J.Exp Int) <- get
                   let nje1 = case (Map.lookup j1 env) of Nothing -> J.Cast closureType j1
                                                          Just no -> var (tempvarstr ++ show no)
                   (usedCl :: Set.Set J.Exp) <- get                                       
                   maybeCloned <- case t of
                                               Body _ ->
                                                   return nje1
                                               _ ->
                                                   if (Set.member nje1 usedCl) then 
                                                        return $ J.MethodInv (J.PrimaryMethodCall (nje1) [] (J.Ident "clone") [])
                                                   else do
                                                        put (Set.insert nje1 usedCl)
                                                        return nje1
                   let nje2 = case (Map.lookup j2 env) of Nothing -> j2
                                                          Just no -> var (tempvarstr ++ show no)
        
                   let cvar = J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp (maybeCloned)))])
                   let ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident localvarstr))) J.EqualA nje2) )
                   return [cvar, ass]                                                     

genIfBody this e2 e3 j1 s1 n = do 
            (s2,j2,t2) <- translateM this e2
            (s3,j3,t3) <- translateM this e3
            let ifvarname = (ifresultstr ++ show n)
            let refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
            let ifresdecl = J.LocalVars [] (objType) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Nothing)])
            let  (ifstmt, ifexp) = ifBody (s2, s3) (j1, j2, j3) n  -- uses a fresh variable
            return (s1 ++ [ifresdecl,ifstmt], ifexp, t2)  -- need to check t2 == t3   

trans :: (MonadState Int m, MonadState (Map.Map J.Exp Int) m, MonadState (Set.Set J.Exp) m, selfType :< Translate m) => Base selfType (Translate m)
trans self = let this = up self in T {
  translateM = \e -> case e of
     CVar (Left i,t) ->
        do return ([],var (localvarstr ++ show i ++ "." ++ localvarstr), t)

     CVar (Right i, t) ->
       do return ([],var (localvarstr ++ show i), t)

     CFLit e    ->
       return ([],J.Lit $ J.Int e, CInt)

     CFPrimOp e1 op e2 ->
       do  (s1,j1,t1) <- translateM this e1
           (s3, jf1) <- genSubst j1 initIntCast
           (s2,j2,t2) <- translateM this e2
           (s4, jf2) <- genSubst j2 initIntCast
           return (s1 ++ s2 ++ s3 ++ s4, J.BinOp jf1 op jf2, t1)

     CFIf0 e1 e2 e3 ->
        do  n <- get
            put (n+1)
            (s1,j1,t1) <- translateM this (CFPrimOp e1 J.Equal (CFLit 0))
            genIfBody this e2 e3 j1 s1 n

     CFTuple tuple ->
       liftM reduceTTuples $ mapM (translateM this) tuple

     CFProj i e ->
       do (s1,j1,t) <- translateM this e
          (s2, j2) <- genSubst j1 initObjArray
          let fj = J.ArrayAccess (J.ArrayIndex j2 (J.Lit (J.Int $ toInteger i)))
          let ft = case t of CTupleType ts -> ts!!i
                             _ -> error "expected tuple type"
          return (s1 ++ s2, fj, ft)

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
           (s, je, t') <- translateScopeM this (s (Right n,t)) (Just (n,t)) -- weird!
           return (s,je, CForall t')

     CApp e1 e2 ->
       do  (n :: Int) <- get
           put (n+1)
           (s1,j1, CForall (Typ t1 g)) <- translateM this e1
           (s2,j2,t2) <- translateM this e2
           let t    = g ()
           let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
           cvarass <- getCvarAss t f n j1 j2
           let genApply = \x y -> [J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall (J.ExpName (J.Name [f])) [] (J.Ident "apply") [])))]
           let genRes = \x -> x
           let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
           s3 <- getS3 t j3 genApply genRes cvarass

           return (s1 ++ s2 ++ s3, j3, scope2ctyp t), -- need to check t1 == t2


  translateScopeM = \e m -> case e of

      Body t ->
        do  (s,je, t1) <- translateM this t
            return (s,je, Body t1)

      Kind f ->
        do  n <- get
            --put (n+1) -- needed?
            (s,je,t1) <- translateScopeM this (f n) m
            return (s,je, Kind (\a -> substScope n (CTVar a) t1))

      Typ t g ->
        do  n <- get
            let f       = J.Ident (localvarstr ++ show n) -- use a fresh variable
            let (v,n')  = maybe (n+1,n+2) (\(i,_) -> (i,n+1)) m -- decide whether we have found the fixpoint closure or not
            put n'
            (s,je,t1) <- translateScopeM this (g (Left v,t)) Nothing
            (env :: Map.Map J.Exp Int) <- get
            let nje = case (Map.lookup je env) of Nothing -> je
                                                  Just no -> var (tempvarstr ++ show no)
            let cvar = standardTranslation nje s v n
            return (cvar,J.ExpName (J.Name [f]), Typ t (\_ -> t1) ),

  createWrap = \name exp ->
        do (bs,e,t) <- translateM this exp
           let returnType = case t of CInt -> Just $ J.PrimType $ J.IntT
                                      _ -> Just $ objType
           let maybeCastedReturnExp = case t of CInt -> J.Cast boxedIntType e
                                                _ -> J.Cast objType e
           let classDecl = getClassDecl name bs ([J.BlockStmt (J.Return $ Just maybeCastedReturnExp)]) returnType mainbody
           return (createCUB [classDecl], t)

    }
