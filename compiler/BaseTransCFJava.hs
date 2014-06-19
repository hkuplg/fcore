{-# OPTIONS -XFlexibleContexts -XTypeOperators -XMultiParamTypeClasses -XKindSignatures -XScopedTypeVariables #-}

module BaseTransCFJava where
-- translation that does not pre-initialize Closures that are ininitalised in apply() methods of other Closures 
import Prelude hiding (init, last)
import Debug.Trace
import Data.List hiding (init, last)

-- import Control.Monad.State
-- import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Language.Java.Syntax as J
import Language.Java.Pretty
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

ifBody :: ([J.BlockStmt], [J.BlockStmt]) -> (J.Exp, J.Exp, J.Exp) -> Int -> (J.BlockStmt, J.Exp)
ifBody (s2, s3) (j1, j2, j3) n = (J.BlockStmt $ J.IfThenElse (j1) (J.StmtBlock $ J.Block (s2 ++ j2Stmt)) (J.StmtBlock $ J.Block (s3 ++ j3Stmt)), newvar)
    where
        j2Stmt = [(J.LocalVars [] (J.RefType (refType "")) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Just (J.InitExp j2))]))]
        j3Stmt = [(J.LocalVars [] (J.RefType (refType "")) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Just (J.InitExp j3))]))]
        ifvarname = (ifresultstr ++ show n)
        refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
        newvar = var ifvarname
        
createCU :: (J.Block, J.Exp, PCTyp Int) -> Maybe String -> (J.CompilationUnit, PCTyp Int)
createCU (J.Block bs,e,t) Nothing = createCU (J.Block bs,e,t) (Just "apply")
createCU (J.Block bs,e,t) (Just expName) = (cu,t) where
   cu = J.CompilationUnit Nothing [] [closureClass,classDecl]
   field name = J.MemberDecl (J.FieldDecl [] (objType) [
              J.VarDecl (J.VarId (J.Ident name)) Nothing])
   app mod b rt en args = J.MemberDecl (J.MethodDecl mod [] (rt) (J.Ident en) args [] (J.MethodBody b))
   closureClass = J.ClassTypeDecl (J.ClassDecl [J.Abstract] (J.Ident "Closure") [] Nothing [] (
                  J.ClassBody [field localvarstr,field "out",app [J.Abstract] Nothing Nothing "apply" [],app [J.Public,J.Abstract] Nothing (Just closureType) "clone" []]))
   body = Just (J.Block (bs ++ [ass]))
   mainArgType = [J.FormalParam [] (J.RefType $ J.ArrayType (J.RefType (refType "String"))) False (J.VarId (J.Ident "args"))]
   --TODO: maybe get a state monad to createCU to createCU somehow?
   maybeCastedReturnExp = case t of CInt -> case (listlast bs) of J.LocalVars [] _ ([J.VarDecl (J.VarId id) (_)]) -> J.ExpName $ J.Name [id]
                                                                  _ -> J.Cast boxedIntType e
                                    _ -> e
   returnType = case t of CInt -> Just $ J.PrimType $ J.IntT
                          _ -> Just $ closureType

   mainbody = Just (J.Block [J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall 
    (J.ExpName (J.Name [J.Ident "System.out"])) [] (J.Ident "println") [J.ExpName $ J.Name [J.Ident (expName ++ "()")]])))])
                          
   ass  = J.BlockStmt (J.Return $ Just maybeCastedReturnExp)
   refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
   classDecl = J.ClassTypeDecl (J.ClassDecl [J.Public] (J.Ident "Main") [] (Nothing) [] 
    (J.ClassBody [app [J.Static] body returnType expName [], app [J.Public, J.Static] mainbody Nothing "main" mainArgType]))

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

type Var = Either Int Int -- left -> standard variable; right -> recursive variable

last (Typ _ _) = False
last (Kind f)  = last (f 0)
last (Body _)  = True

instCreat i = J.InstanceCreation [] (J.ClassType [(J.Ident ("Fun" ++ show i),[])]) [] Nothing

jexp init body (enCF,idCF) ids = J.InstanceCreation [] (J.ClassType [(J.Ident "Closure",[])]) [] 
       (Just (J.ClassBody (init ++  [
          J.MemberDecl (J.MethodDecl [] [] Nothing (J.Ident "apply") [] [] (J.MethodBody body)),
          J.MemberDecl (J.MethodDecl [J.Public] [] (Just closureType) (J.Ident "clone") [] [] (J.MethodBody cloneBody))
       ])))
        where
            enclosingId = J.Ident (localvarstr ++ show (enCF-2))
            newClone i =  Just $ J.Block $ [J.BlockStmt $ J.Return $ Just (instCreat i)]            
            cloneBody | ids = newClone idCF
                      | otherwise = Just (J.Block [J.LocalVars [] (closureType) [J.VarDecl (J.VarId (J.Ident "c")) 
                (Just (J.InitExp (J.MethodInv (J.MethodCall (J.Name [enclosingId,J.Ident "clone"]) []))))],J.BlockStmt 
                (J.ExpStmt (J.Assign (J.NameLhs (J.Name [J.Ident "c",J.Ident localvarstr])) J.EqualA 
                (J.ExpName (J.Name [enclosingId,J.Ident localvarstr])))),J.BlockStmt (J.ExpStmt (J.MethodInv 
                (J.MethodCall (J.Name [J.Ident "c",J.Ident "apply"]) []))),J.BlockStmt (J.Return (Just 
                (J.Cast (closureType) (J.ExpName (J.Name [J.Ident "c",J.Ident "out"])))))])

currentInitialDeclaration idCurrentName = J.MemberDecl $ J.FieldDecl [] closureType [J.VarDecl (J.VarId idCurrentName) (Just (J.InitExp J.This))]
outputAssignment javaExpression = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA  javaExpression))
standardTranslation javaExpression statementsBeforeOA currentId nextId ids = J.LocalVars [] closureType [J.VarDecl (J.VarId $ J.Ident (localvarstr ++ show nextId)) 
                                                (Just (J.InitExp 
                                                    (jexp [currentInitialDeclaration (J.Ident (localvarstr ++ show currentId))] (Just (J.Block (statementsBeforeOA ++ [outputAssignment javaExpression]))) (currentId,nextId) ids)
                                                    )
                                                )]
  
data Translate m = T {
  translateM :: 
     PCExp Int (Var, PCTyp Int) -> 
     m ([J.BlockStmt], J.Exp, PCTyp Int),
  translateScopeM :: 
    Scope (PCExp Int (Var, PCTyp Int)) Int (Var, PCTyp Int) -> 
    Maybe (Int,PCTyp Int) ->
    m ([J.BlockStmt], J.Exp, TScope Int)
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
    
trans :: (MonadState Int m, MonadReader (Set.Set Int) m, MonadState (Map.Map J.Exp Int) m, selfType :< Translate m) => Base selfType (Translate m)
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
           
     CFif0 e1 e2 e3 ->
        do  n <- get
            put (n+1)
            (s1,j1,t1) <- translateM this (CFPrimOp e1 J.Equal (CFLit 0))
            (s2,j2,t2) <- translateM this e2
            (s3,j3,t3) <- translateM this e3
            let ifvarname = (ifresultstr ++ show n)
            let refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
            let ifresdecl = J.LocalVars [] (objType) ([J.VarDecl (J.VarId $ J.Ident ifvarname) (Nothing)])
            let  (ifstmt, ifexp) = ifBody (s2, s3) (j1, j2, j3) n  -- uses a fresh variable        
            return (s1 ++ [ifresdecl,ifstmt], ifexp, t2)                    -- need to check t2 == t3
           
     CFTuple tuple ->
       liftM reduceTTuples $ mapM (translateM this) tuple
       
     CFProj i (CFTuple tuple) ->
       translateM this (tuple!!i)
     
     CTApp e t -> 
       do  n <- get
           (s,je, CForall (Kind f)) <- translateM this e
           return (s,je, scope2ctyp (substScope n t (f n)))
    -- TODO: CLam and CFix generation of top-level Fun closures is a bit ad-hoc transformation from the old generated code + duplicate code 
     CLam se ->
       do  (n :: Int) <- get
           (s,je, t) <- local (Set.insert n) (translateScopeM this se Nothing)
           let ns = case (head s) of (J.LocalVars [] (a) ([J.VarDecl (varId) 
                                        (Just (J.InitExp (J.InstanceCreation _ _ _ (Just initexp))))])) -> 
                                        [(J.LocalClass (J.ClassDecl [] (J.Ident ("Fun" ++ show n)) [] 
                                        (Just $ J.ClassRefType (J.ClassType [(J.Ident "Closure",[])])) [] (initexp))),
                                        J.LocalVars [] (a) ([J.VarDecl (varId) (Just (J.InitExp (instCreat n)))])]
           return (ns ++ (tail s),je, CForall t)
     
     CFix t s   -> 
       do  (n :: Int) <- get
           put (n+1)
           (s, je, t') <- local (Set.insert (n+1)) (translateScopeM this (s (Right n,t)) (Just (n,t))) -- weird!
           let ns = case (head s) of (J.LocalVars [] (a) ([J.VarDecl (varId) 
                                        (Just (J.InitExp (J.InstanceCreation _ _ _ (Just initexp))))])) -> 
                                        [(J.LocalClass (J.ClassDecl [] (J.Ident ("Fun" ++ show (n+1))) [] 
                                        (Just $ J.ClassRefType (J.ClassType [(J.Ident "Closure",[])])) [] (initexp))),
                                        J.LocalVars [] (a) ([J.VarDecl (varId) (Just (J.InitExp (instCreat (n+1))))])]           
           return (ns ++ (tail s),je, CForall t')
           
     CApp e1 e2 ->
       do  (n :: Int) <- get
           put (n+1)
           (s1,j1, CForall (Typ t1 g)) <- translateM this e1
           -- DEBUG
           -- (s1,j1, debug) <- translateM this e1
           -- (CForall (Typ t1 g)) <- trace ("C:" ++ show debug) (return debug)
           -- END DEBUG
           (s2,j2,t2) <- translateM this e2
           (env :: Map.Map J.Exp Int) <- get
           let t    = g ()
           let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable
           let nje1 = case (Map.lookup j1 env) of Nothing -> J.Cast closureType j1
                                                  Just no -> var (tempvarstr ++ show no)
           let maybeCloned = case t of
                                       Body _ ->
                                           nje1
                                       _ ->
                                           J.MethodInv (J.PrimaryMethodCall (nje1) [] (J.Ident "clone") [])
                                           
           let cvar = J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp (maybeCloned)))])
           let nje2 = case (Map.lookup j2 env) of Nothing -> j2
                                                  Just no -> var (tempvarstr ++ show no)    
                                                  
           let ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident localvarstr))) J.EqualA nje2) ) 
           let apply = J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall (J.ExpName (J.Name [f])) [] (J.Ident "apply") [])))
           let j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
           s3 <- case (scope2ctyp t) of CInt ->
                                            do (result, _) <- genSubst j3 initIntCast
                                               let r = [cvar,ass,apply] ++ result
                                               return r
                                        CForall (_) ->
                                            do (result, _) <- genSubst j3 initClosure
                                               let r = [cvar,ass,apply] ++ result
                                               return r
                                        _ ->  
                                            do (result, _) <- genSubst j3 initObj
                                               let r = [cvar,ass,apply] ++ result
                                               return r

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
            envs :: Set.Set Int <- ask
            let f    = J.Ident (localvarstr ++ show n) -- use a fresh variable              
            case m of -- Consider refactoring later?
              Just (i,t') | last (g (Right i,t')) ->
                do  put (n+1)
                    --let self = J.Ident (localvarstr ++ show i)
                    (s,je,t1) <- translateScopeM this (g (Left i,t)) m
                    (env :: Map.Map J.Exp Int) <- get
                    let nje = case (Map.lookup je env) of Nothing -> je
                                                          Just no -> var (tempvarstr ++ show no)
                    let cvar = standardTranslation nje s i n (Set.member n envs)
                    return ([cvar],J.ExpName (J.Name [f]), Typ t (\_ -> t1) )
              otherwise -> 
                do  put (n+2)
                    --let self = J.Ident (localvarstr ++ show (n+1)) -- use another fresh variable              
                    (s,je,t1) <- translateScopeM this (g (Left (n+1),t)) m
                    (env :: Map.Map J.Exp Int) <- get
                    let nje = case (Map.lookup je env) of Nothing -> je
                                                          Just no -> var (tempvarstr ++ show no)                    
                    let cvar = standardTranslation nje s (n+1) n (Set.member n envs)
                    return ([cvar],J.ExpName (J.Name [f]), Typ t (\_ -> t1) )

    }
