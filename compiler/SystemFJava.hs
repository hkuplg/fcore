{-# OPTIONS -XRankNTypes #-}

module SystemFJava where

import Prelude hiding (init)

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

data Scope b fv t e = Body b | Kind (t -> Scope b fv t e) | Typ (PCTyp fv t e) (e -> Scope b fv t e) 

data Void = Void (forall a . a)

type TScope fv t e = Scope (PCTyp fv t e) fv t e

type EScope fv t e = Scope (PCExp fv t e) fv t e

data PCTyp fv t e = CTFVar fv | CTVar t | CForall (TScope fv t e)

data PCExp fv t e = 
     CVar e 
   | CLam (EScope fv t e)  
   | CApp (PCExp fv t e) (PCExp fv t e)
   | CTApp (PCExp fv t e) (PCTyp fv t e)

ftyp2scope :: PFTyp t -> TScope fv t e
ftyp2scope (FTVar x)     = Body (CTVar x)
ftyp2scope (FForall f)   = Kind (\a -> ftyp2scope (f a))
ftyp2scope (FFun t1 t2)  = Typ (ftyp2ctyp t1) (\x -> ftyp2scope t2)

ftyp2ctyp :: PFTyp t -> PCTyp fv t e
ftyp2ctyp (FTVar x) = CTVar x
ftyp2ctyp t         = CForall (ftyp2scope t)

fexp2cexp :: PFExp t e -> PCExp fv t e
fexp2cexp (FVar x)      = CVar x
fexp2cexp (FApp e1 e2)  = CApp (fexp2cexp e1) (fexp2cexp e2)
fexp2cexp (FTApp e t)   = CTApp (fexp2cexp e) (ftyp2ctyp t)
fexp2cexp e             = CLam (groupLambda e)

groupLambda :: PFExp t e -> EScope fv t e
groupLambda (FBLam f)  = Kind (\a -> groupLambda (f a))
groupLambda (FLam t f) = Typ (ftyp2ctyp t) (\x -> groupLambda (f x))
groupLambda e          = Body (fexp2cexp e)

-- type inference for Closure F

infer = joinPCTyp . infer'

infer' :: PCExp fv (PCTyp fv t e) (PCTyp fv t e) -> PCTyp fv (PCTyp fv t e) (PCTyp fv t e)
infer' (CVar t) = CTVar t
infer' (CLam s) = CForall (inferScope s)
infer' (CTApp e t) = 
   case infer' e of
      (CForall (Kind f)) -> scope2ctyp (f (joinPCTyp t))
infer' (CApp e1 e2) = 
   case (infer' e1, infer' e2) of 
      (CForall (Typ t1 f), t3) -> scope2ctyp (f (joinPCTyp t1)) -- need to check whether t1 and t3 are compatible 

scope2ctyp :: TScope fv t e -> PCTyp fv t e
scope2ctyp (Body t)  = t
scope2ctyp s         = CForall s

inferScope :: EScope fv (PCTyp fv t e) (PCTyp fv t e) -> TScope fv (PCTyp fv t e) (PCTyp fv t e)
inferScope (Body e) = Body (infer' e)
inferScope (Kind f) = Kind (\a -> inferScope (f a))
inferScope (Typ t f) = Typ t (\_ -> inferScope (f (joinPCTyp t)))

joinPCTyp :: PCTyp fv (PCTyp fv t e) (PCTyp fv t e) -> PCTyp fv t e
joinPCTyp (CTVar t) = t
joinPCTyp (CForall s) = CForall (joinScope s)

joinScope :: TScope fv (PCTyp fv t e) (PCTyp fv t e) -> TScope fv t e
joinScope (Body t) = Body (joinPCTyp t)
joinScope (Kind f) = Kind (\a -> joinScope (f (CTVar a)))
joinScope (Typ t f) = let t' = joinPCTyp t in Typ t' (\_ -> joinScope (f t'))

-- Closure F to Java

var x = J.ExpName (J.Name [J.Ident x])

translate :: PCExp Int (PCTyp Int t e) (Int, PCTyp Int t e) -> Int -> ([J.BlockStmt], J.Exp,  PCTyp Int (PCTyp Int t e) (Int,PCTyp Int t e))
translate (CVar (i,t)) n = ([],var ("x" ++ show i),CTVar t)
translate (CTApp e t) n = 
   case translate e n of
      (s,je,CForall (Kind f)) -> (s,je,scope2ctyp (f (joinPCTyp2 t)))
translate (CLam s) n = 
   case translateScope s Nothing n of 
      (s,je, t) -> (s,je, CForall t)
translate (CApp e1 e2) n = 
   case (translate e1 (n+1), translate e2 (n+1)) of 
      ((s1,j1,CForall (Typ t1 g)),(s2,j2,t2)) -> (s1 ++ s2 ++ s3, j3, scope2ctyp (g (n+1,joinPCTyp2 t1))) -- need to check t1 == t2
        where
           f    = J.Ident ("x" ++ show n) -- use a fresh variable
           cvar = J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp (J.Cast closureType j1)))])
           ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.FieldLhs (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "x"))) J.EqualA j2) ) 
           apply = J.BlockStmt (J.ExpStmt (J.MethodInv (J.PrimaryMethodCall (J.ExpName (J.Name [f])) [] (J.Ident "apply") [])))
           s3 = [cvar,ass,apply]
           j3 = (J.FieldAccess (J.PrimaryFieldAccess (J.ExpName (J.Name [f])) (J.Ident "out")))
     
closureType = J.RefType (J.ClassRefType (J.ClassType [(J.Ident "Closure",[])]))

translateScope (Body t) Nothing n = 
   case translate t n of
      (s,je, t1) -> (s,je, Body t1)
translateScope (Body t) (Just _) n = -- what to do with _? Anything 
   case translate t (n+1) of
      (s,je, t2) -> ([cvar],J.ExpName (J.Name [f]), Body t2)
        where
          f    = J.Ident ("x" ++ show n) -- use a fresh variable
          ass  = J.BlockStmt (J.ExpStmt (J.Assign (J.NameLhs (J.Name [(J.Ident "out")])) J.EqualA je))
          cvar = J.LocalVars [] closureType ([J.VarDecl (J.VarId f) (Just (J.InitExp (jexp [] (Just (J.Block (s ++ [ass]))))))])
translateScope (Kind f) m n = 
   case translateScope (f (CTFVar n)) m (n+1) of
      (s,je,t1) -> (s,je, Kind (\a -> substScope n a t1))
translateScope (Typ t f) m n = 
   case translateScope (f (n, joinPCTyp2 t)) (Just t) (n+1) of
       (s,je,t1) -> (s,je,Typ t (\_ -> t1)) -- wrong: here we actually spit some java


-- Free variable substitution

substScope :: Int -> 
              PCTyp Int t e -> 
              TScope Int (PCTyp Int t e) (Int,PCTyp Int t e) -> 
              TScope Int (PCTyp Int t e) (Int,PCTyp Int t e)
substScope n t (Body t1) = Body (substType n t t1)
substScope n t (Kind f)  = Kind (\a -> substScope n t (f a))
substScope n t (Typ t1 f) = Typ (substType n t t1) (\x -> substScope n t (f x))

substType n t (CTFVar x) 
   | x == n     = CTVar t
   | otherwise  = CTFVar x
substType n t (CTVar x) = CTVar x
substType n t (CForall s) = CForall (substScope n t s)

joinPCTyp2 :: PCTyp Int (PCTyp Int t e) (Int,PCTyp Int t e) -> PCTyp Int t e
joinPCTyp2 (CTVar t) = t
joinPCTyp2 (CForall s) = CForall (joinScope2 s)

joinScope2 :: TScope Int (PCTyp Int t e) (Int,PCTyp Int t e) -> TScope Int t e
joinScope2 (Body t) = Body (joinPCTyp2 t)
joinScope2 (Kind f) = Kind (\a -> joinScope2 (f (CTVar a)))
joinScope2 (Typ t f) = let t' = joinPCTyp2 t in Typ t' (\_ -> joinScope2 (f (0,t')))

prettyJ :: Pretty a => a -> IO ()
prettyJ = putStrLn . prettyPrint

jthree = J.Lit (J.Int 3)

jbody = Just (J.Block [])

init = [J.InitDecl False (J.Block [])]

jexp init body = J.InstanceCreation [] (J.ClassType [(J.Ident "Closure",[])]) [] 
       (Just (J.ClassBody (init ++ [
          J.MemberDecl (J.MethodDecl [] [] Nothing (J.Ident "apply") [] [] (J.MethodBody body))
       ])))

e1 = jexp init jbody

compile e = 
  case translate (fexp2cexp e) 0 of
      (ss,exp,_) -> (J.Block ss,exp)

compilePretty e = let (b,exp) = compile e in (prettyJ b >> prettyJ exp)

idF = FBLam (\a -> FLam (FTVar a) (\x -> FVar x))

idF2 = FBLam (\a -> FApp (FLam (FFun (FTVar a) (FTVar a)) (\f -> FLam (FTVar a) (\x -> FApp (FVar f) (FVar x)))) (FTApp idF (FTVar a)))


idF3 = FBLam (\a -> FLam (FTVar a) (\x -> FApp (FTApp idF (FTVar a)) (FVar x) ))