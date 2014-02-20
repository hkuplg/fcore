{-# OPTIONS -XRankNTypes -XFlexibleInstances #-}

module SystemFJava where

import Prelude hiding (init)
import Debug.Trace
import Unsafe.Coerce

import qualified Language.Java.Syntax as J
import Language.Java.Pretty

-- System F syntax

data PFTyp t = FTVar t | FForall (t -> PFTyp t) | FFun (PFTyp t) (PFTyp t) 

data PFExp t e = 
     FVar e 
   | FBLam (t -> PFExp t e) 
   | FLam (PFTyp t) (e -> PFExp t e) 
   | FApp (PFExp t e) (PFExp t e)
   | FTApp (PFExp t e) (PFTyp t)

-- Closure F syntax

data Scope b t e = Body b | Kind (t -> Scope b t e) | Typ (PCTyp t e) (e -> Scope b t e) 

data Void = Void (forall a . a)

type TScope t e = Scope (PCTyp t e) t e

type EScope t e = Scope (PCExp t e) t e

data PCTyp t e = CTFVar Int | CTVar t | CForall (TScope t e)

data PCExp t e = 
     CVar e 
   | CFVar Int
   | CLam (EScope t e)  
   | CApp (PCExp t e) (PCExp t e)
   | CTApp (PCExp t e) (PCTyp t e)

-- Pretty printing

data Iso a b = Iso {from :: a -> b, to :: b -> a}

showPCTyp2 = gshowPCTyp

-- showPCTyp3 = gshowPCTyp (Iso IT unIT)

gshowPCTyp :: PCTyp Typ Typ -> Int -> String
gshowPCTyp (CTFVar i) n = "a" ++ show i
gshowPCTyp (CTVar t) n = gshowPCTyp (unT t) n
gshowPCTyp (CForall s) n = "(forall " ++ showScope2 gshowPCTyp  s n ++ ")"

showScope2 showF (Body t) n = ". " ++ showF t n
showScope2 showF (Kind f) n = "a" ++ show n ++ " " ++ showScope2 showF (f (T (CTFVar n))) (n+1)
showScope2 showF (Typ t f) n = 
   "(x" ++ show n ++ " : " ++ gshowPCTyp  t n ++ ") " ++ showScope2 showF (f (T (CTFVar n))) (n+1)

gshowPCTyp3 :: PCTyp ITyp (Int,ITyp) -> Int -> String
gshowPCTyp3 (CTFVar i) n = "a" ++ show i
gshowPCTyp3 (CTVar t) n = gshowPCTyp3 (unIT t) n
gshowPCTyp3 (CForall s) n = "(forall " ++ showScope3 gshowPCTyp3  s n ++ ")"

showScope3 showF (Body t) n = ". " ++ showF t n
showScope3 showF (Kind f) n = "a" ++ show n ++ " " ++ showScope3 showF (f (IT (CTFVar n))) (n+1)
showScope3 showF (Typ t f) n = 
   "(x" ++ show n ++ " : " ++ gshowPCTyp3  t n ++ ") " ++ showScope3 showF (f (n,IT (CTFVar n))) (n+1)


showPCTyp :: PCTyp Int Int -> Int -> String
showPCTyp (CTFVar i) n = "y" ++ show i
showPCTyp (CTVar i) n = "a" ++ show i
showPCTyp (CForall s) n = "(forall " ++ showScope showPCTyp s n ++ ")"

showScope showF (Body t) n = ". " ++ showF t n
showScope showF (Kind f) n = "a" ++ show n ++ " " ++ showScope showF (f n) (n+1)
showScope showF (Typ t f) n = "(x" ++ show n ++ " : " ++ showPCTyp t n ++ ") " ++ showScope showF (f n) (n+1)

showPCExp (CVar x) n      = "x" ++ show x
showPCExp (CLam s) n      = "(\\" ++ showScope showPCExp s n ++ ")"
showPCExp (CApp e1 e2) n  = showPCExp e1 n ++ " " ++ showPCExp e2 n
showPCExp (CTApp e t) n   = showPCExp e n ++ " " ++ showPCTyp t n

instance Show (PCTyp Int Int) where
   show e = showPCTyp e 0

instance Show (TScope Int Int) where
   show e = showScope showPCTyp e 0

instance Show (EScope Int Int) where
   show e = showScope showPCExp e 0

instance Show (PCExp Int Int) where
   show e = showPCExp e 0

-- System F to Closure F

ftyp2scope :: PFTyp t -> TScope t e
ftyp2scope (FTVar x)     = Body (CTVar x)
ftyp2scope (FForall f)   = Kind (\a -> ftyp2scope (f a))
ftyp2scope (FFun t1 t2)  = Typ (ftyp2ctyp t1) (\x -> ftyp2scope t2)

ftyp2ctyp :: PFTyp t -> PCTyp t e
ftyp2ctyp (FTVar x) = CTVar x
ftyp2ctyp t         = CForall (ftyp2scope t)

fexp2cexp :: PFExp t e -> PCExp t e
fexp2cexp (FVar x)      = CVar x
fexp2cexp (FApp e1 e2)  = CApp (fexp2cexp e1) (fexp2cexp e2)
fexp2cexp (FTApp e t)   = CTApp (fexp2cexp e) (ftyp2ctyp t)
fexp2cexp e             = CLam (groupLambda e)

groupLambda :: PFExp t e -> EScope t e
groupLambda (FBLam f)  = Kind (\a -> groupLambda (f a))
groupLambda (FLam t f) = Typ (ftyp2ctyp t) (\x -> groupLambda (f x))
groupLambda e          = Body (fexp2cexp e)

-- type inference for Closure F

newtype Typ = T {unT :: PCTyp Typ Typ}

newtype ITyp = IT {unIT :: PCTyp ITyp (Int,ITyp)}

infer :: PCExp Typ Typ -> PCTyp Typ Typ
infer (CVar t) = unT t
infer (CTApp e t) = 
  case infer e of 
     (CForall (Kind f)) -> scope2ctyp (f (T t))
infer (CApp e1 e2) = 
  case (infer e1, infer e2) of 
     (CForall (Typ t1 f),t3) -> scope2ctyp (f (T t1))
infer (CLam s) = CForall (inferScope s)

inferScope :: EScope Typ Typ -> TScope Typ Typ
inferScope (Body e)   = Body (infer e)
inferScope (Kind f)   = Kind (\a -> inferScope (f a))
inferScope (Typ t f)  = Typ t (\_ -> inferScope (f (T t)))

scope2ctyp :: TScope t e -> PCTyp t e
scope2ctyp (Body t)  = t
scope2ctyp s         = CForall s

inferPretty ::  PFExp Typ Typ -> IO ()
inferPretty e = putStrLn (showPCTyp2 (infer (fexp2cexp e)) 0) 

-- Some test terms

idF = FBLam (\a -> FLam (FTVar a) (\x -> FVar x))

-- /\a . (\(f : a -> a) . \(x : a) . f x) (idF a)

idF2 = FBLam (\a -> FApp (FLam (FFun (FTVar a) (FTVar a)) (\f -> FLam (FTVar a) (\x -> FApp (FVar f) (FVar x)))) (FTApp idF (FTVar a)))

-- /\a . \(x:a) . (idF a) x

idF3 = FBLam (\a -> FLam (FTVar a) (\x -> FApp (FTApp idF (FTVar a)) (FVar x) ))

-- Closure F to Java

var x = J.ExpName (J.Name [J.Ident x])

jbody = Just (J.Block [])

init = [J.InitDecl False (J.Block [])]

jexp init body = J.InstanceCreation [] (J.ClassType [(J.Ident "Closure",[])]) [] 
       (Just (J.ClassBody (init ++  [
          J.MemberDecl (J.MethodDecl [] [] Nothing (J.Ident "apply") [] [] (J.MethodBody body))
       ])))

closureType = J.RefType (J.ClassRefType (J.ClassType [(J.Ident "Closure",[])]))

createCU :: (J.Block, J.Exp,  PCTyp ITyp (Int,ITyp)) -> (J.CompilationUnit, PCTyp ITyp (Int,ITyp))
createCU (J.Block bs,e,t) = (cu,t) where
   cu = J.CompilationUnit Nothing [] [closureClass, classDecl]
   field name = J.MemberDecl (J.FieldDecl [] (J.RefType (refType "Object")) [
              J.VarDecl (J.VarId (J.Ident name)) Nothing])
   app mod b = J.MemberDecl (J.MethodDecl mod [] Nothing (J.Ident "apply") [] [] (J.MethodBody b))
   closureClass = J.ClassTypeDecl (J.ClassDecl [J.Abstract] (J.Ident "Closure") [] Nothing [] (
                  J.ClassBody [field "x",field "out",app [J.Abstract] Nothing]))
   body = Just (J.Block (bs ++ [ass]))
   ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA e))
   refType t = J.ClassRefType (J.ClassType [(J.Ident t,[])])
   classDecl = J.ClassTypeDecl (J.ClassDecl [] (J.Ident "MyClosure") [] (Just (refType "Closure")) [] (J.ClassBody [app [] body]))


translate :: PCExp ITyp (Int,ITyp) -> Int -> ([J.BlockStmt], J.Exp,  PCTyp ITyp (Int,ITyp))
translate (CVar (i,t)) n = ([],var ("x" ++ show i ++ ".x"),unIT t) -- small hack! 
translate (CTApp e t) n = 
   case translate e n of
      (s,je,CForall (Kind f)) -> (s,je,scope2ctyp (f (IT t)))
translate (CLam s) n = 
   case translateScope s Nothing n of 
      (s,je, t) -> (s,je, CForall t)
translate (CApp e1 e2) n = 
   case (translate e1 (n+1), translate e2 (n+1)) of 
      ((s1,j1,CForall (Typ t1 g)),(s2,j2,t2)) -> (s1 ++ s2 ++ s3, j3, scope2ctyp (g (n,IT t1))) -- need to check t1 == t2
        where
           f    = J.Ident ("x" ++ show n) -- use a fresh variable
           cvar = J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp (J.Cast closureType j1)))])
           ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "x"))) J.EqualA j2) ) 
           apply = J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall (J.ExpName (J.Name [f])) [] (J.Ident "apply") [])))
           s3 = [cvar,ass,apply]
           j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))

translateScope (Body t) m n = 
   case translate t n of
      (s,je, t1) -> (s,je, Body t1)
translateScope (Kind f) m n = 
    case translateScope (f (IT (CTFVar n))) m (n+1) of
      (s,je,t1) -> (s,je, Kind (\a -> substScope n a t1))
translateScope (Typ t f) m n = 
   case translateScope (f (n+1,IT t)) (Just t) (n+2) of
       (s,je,t1) -> ([cvar],J.ExpName (J.Name [f]),Typ t (\_ -> t1))
         where
          f    = J.Ident ("x" ++ show n) -- use a fresh variable
          self = J.Ident ("x" ++ show (n+1))
          cvar = refactoredScopeTranslationBit (je) (self) (s) (f)

checkExp :: J.Exp -> Bool
checkExp (J.ExpName (J.Name [J.Ident f])) = '.' `elem` f
checkExp x = True

jexpOutside init = J.InstanceCreation [] (J.ClassType [(J.Ident "Closure",[])]) [] 
       (Just (J.ClassBody (init ++  [
          J.MemberDecl (J.MethodDecl [] [] Nothing (J.Ident "apply") [] [] (J.MethodBody (Just(J.Block $ [J.BlockStmt $ J.Empty]))))
       ])))

pullupClosure [J.LocalVars [] rf vd] = case vd of
                                [J.VarDecl variableId (Just(J.InitExp exp))] -> exp

-- seperating (hopefully) the important bit

refactoredScopeTranslationBit :: J.Exp -> J.Ident -> [J.BlockStmt] -> J.Ident -> J.BlockStmt
refactoredScopeTranslationBit javaExpression idCurrentName statementsBeforeOA idNextName = completeClosure
    where
        outputAssignment = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA  javaExpression))
        fullAssignment = J.InitDecl False (J.Block [(J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA
                                                    (pullupClosure statementsBeforeOA))))])
        currentInitialDeclaration = J.MemberDecl $ J.FieldDecl [] closureType [J.VarDecl (J.VarId idCurrentName) (Just (J.InitExp J.This))]
        completeClosure | checkExp javaExpression  = J.LocalVars [] closureType [J.VarDecl (J.VarId idNextName) 
                                                (Just (J.InitExp 
                                                    (jexp [currentInitialDeclaration] (Just (J.Block (statementsBeforeOA ++ [outputAssignment]))))
                                                    )
                                                )]
                        | otherwise = J.LocalVars [] closureType [J.VarDecl (J.VarId idNextName) 
                                                (Just (J.InitExp 
                                                    (jexpOutside [currentInitialDeclaration,fullAssignment]
                                                    )
                                                ))]
-- Free variable substitution

substScope :: Int -> 
              t -> 
              TScope t e -> 
              TScope t e
substScope n t (Body t1) = Body (substType n t t1)
substScope n t (Kind f)  = Kind (\a -> substScope n t (f a))
substScope n t (Typ t1 f) = Typ (substType n t t1) (\x -> substScope n t (f x))

substType n t (CTFVar x) 
   | x == n     = CTVar t
   | otherwise  = CTFVar x
substType n t (CTVar x) = CTVar x
substType n t (CForall s) = CForall (substScope n t s)

prettyJ :: Pretty a => a -> IO ()
prettyJ = putStrLn . prettyPrint

jthree = J.Lit (J.Int 3)

e1 = jexp init jbody

compile e = 
  case translate (fexp2cexp e) 0 of
      (ss,exp,t) -> (J.Block ss,exp, t)

compilePretty ::  PFExp ITyp (Int,ITyp) -> IO ()
compilePretty e = let (b,exp,t) = compile e in (prettyJ b >> prettyJ exp >> putStrLn (gshowPCTyp3 t 0))

compileCU ::  PFExp ITyp (Int,ITyp) -> IO ()
compileCU e = let (cu,t) = createCU $ compile e in (prettyJ cu >> putStrLn (gshowPCTyp3 t 0))
