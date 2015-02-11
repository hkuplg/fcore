{-# LANGUAGE RankNTypes #-}
-- | Some utilities for optimization purpose

module OptiUtils where

import Core
import System.Directory
import Parser (reader)
import TypeCheck (typeCheck)
import Desugar (desugar)
import Simplify (simplify)
import Control.Monad (liftM)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.List (foldl')
import Panic
import qualified Src
import qualified SystemFI as FI


simplify2 :: FI.Expr t e -> Expr t e
simplify2 (FI.Var n e) = Var n e
simplify2 (FI.Lit n) = Lit n
simplify2 (FI.Lam n t f) = Lam n (transType2 t) (\e -> simplify2 (f e))
simplify2 (FI.Fix n1 n2 f t1 t2) = Fix n1 n2 (\e1 e2 -> simplify2 (f e1 e2)) (transType2 t1) (transType2 t2)
simplify2 (FI.Let n e f) = Let n (simplify2 e) (\e1 -> simplify2 (f e1))
simplify2 (FI.LetRec ns ts e1 e2) = LetRec ns (map transType2 ts) (\es1 -> map simplify2 (e1 es1)) (\es2 -> simplify2 (e2 es2))
simplify2 (FI.BLam n f) = BLam n (\t -> simplify2 (f t))
simplify2 (FI.App e1 e2) = App (simplify2 e1) (simplify2 e2)
simplify2 (FI.TApp e t) = TApp (simplify2 e) (transType2 t)
simplify2 (FI.If e1 e2 e3) = If (simplify2 e1) (simplify2 e2) (simplify2 e3)
simplify2 (FI.PrimOp e1 op e2) = PrimOp (simplify2 e1) op (simplify2 e2)
simplify2 (FI.Tuple es) = Tuple (map simplify2 es)
simplify2 (FI.Proj n e) = Proj n (simplify2 e)
simplify2 (FI.JNew n es) = JNew n (map simplify2 es)
simplify2 (FI.JMethod jc m es cn) =
  JMethod (fmap simplify2 jc) m (map simplify2 es) cn
simplify2 (FI.JField jc fn cn) = JField (fmap simplify2 jc) fn cn
simplify2 (FI.PolyList es t) = PolyList (map simplify2 es) (transType2 t)
simplify2 (FI.JProxyCall jmethod t) = JProxyCall (simplify2 jmethod) (transType2 t)
simplify2 (FI.Seq es) = Seq (map simplify2 es)
simplify2 _ = sorry "No"


transType2 :: FI.Type t -> Type t
transType2 (FI.TVar n t) = TVar n t
transType2 (FI.JClass n) = JClass n
transType2 (FI.Fun t1 t2) = Fun (transType2 t1) (transType2 t2)
transType2 (FI.Forall n f) = Forall n (\t -> transType2 (f t))
transType2 (FI.Product ts) = Product (map transType2 ts)
transType2 (FI.Unit) = Unit
transType2 (FI.ListOf t) = ListOf (transType2 t)
transType2 _ = sorry "No"



joinExpr :: Expr t (Expr t e) -> Expr t e
joinExpr (Var _ x) = x
joinExpr (Lam n t1 e1) = Lam n t1 (joinExpr . e1 . Var n )
joinExpr (App e1 e2) = App (joinExpr e1) (joinExpr e2)
joinExpr (BLam n t1) = BLam n (joinExpr . t1)
joinExpr (TApp e t) = TApp (joinExpr e) t
joinExpr (Lit s) = Lit s
joinExpr (If e1 e2 e3) = If (joinExpr e1) (joinExpr e2) (joinExpr e3)
joinExpr (PrimOp e1 o e2) = PrimOp (joinExpr e1) o (joinExpr e2)
joinExpr (Tuple es) = Tuple (map joinExpr es)
joinExpr (PolyList es t) = PolyList (map joinExpr es) t
joinExpr (JProxyCall jmethod t) = JProxyCall (joinExpr jmethod) t
joinExpr (Proj i e) = Proj i (joinExpr e)
joinExpr (Fix n1 n2 f t1 t2) = Fix n1 n2 (\e1 e2 -> joinExpr (f (Var n1 e1) (Var n2 e2))) t1 t2
joinExpr (Let n bind body) = Let n (joinExpr bind) (joinExpr . body . Var n)
joinExpr (LetRec n s b1 b2) =
  LetRec n s
         (map joinExpr . b1 . zipWith Var n)
         (joinExpr . b2 . zipWith Var n)
joinExpr (JNew name es) = JNew name (map joinExpr es)
joinExpr (JMethod jc m es cn) =
  JMethod (fmap joinExpr jc) m (map joinExpr es) cn
joinExpr (JField jc fn cn) = JField (fmap joinExpr jc) fn cn
joinExpr (Seq es) = Seq (map joinExpr es)

mapExpr :: (Expr t e -> Expr t e) -> Expr t e -> Expr t e
mapExpr f e =
    case e of
      Var _ _ -> e
      Lit _ -> e
      Lam n t g -> Lam n t (\x -> f . g $ x)
      Fix n1 n2 g t1 t -> Fix n1 n2 (\e1 e2 -> f $ g e1 e2) t1 t
      Let n bind body -> Let n (f bind) (\x -> f . body $ x)
      LetRec names sigs binds body ->
          LetRec names sigs
                 (map f . binds)
                 (f . body)
      BLam n g -> BLam n (\x -> f . g $ x)
      App e1 e2 -> App (f e1) (f e2)
      TApp e' t -> TApp (f e') t
      If e1 e2 e3 -> If (f e1) (f e2) (f e3)
      PrimOp e1 op e2 -> PrimOp (f e1) op (f e2)
      Tuple es -> Tuple $ map f es
      Proj i e' -> Proj i (f e')
      JNew cname es -> JNew cname (map f es)
      JMethod cnameOrE mname es cname -> JMethod (fmap f cnameOrE) mname (map f es) cname
      JField cnameOrE fname cname -> JField (fmap f cnameOrE) fname cname
      PolyList es t -> PolyList (map f es) t
      JProxyCall jmethod t -> JProxyCall (f jmethod) t
      Seq es -> Seq $ map f es

rewriteExpr :: (Int -> Map.Map Int e -> Expr t Int -> Expr t e) -> Int -> Map.Map Int e -> Expr t Int -> Expr t e
rewriteExpr f num env expr =
  case expr of
   Var n v -> Var n (fromJust $ Map.lookup v env)
   Lit n ->  Lit n
   Lam n t f' -> Lam n t (\e -> f (num + 1) (Map.insert num e env) (f' num))
   Fix n1 n2 f' t1 t2 -> Fix n1 n2 (\a b -> f (num + 2) (Map.insert (num + 1) b (Map.insert num a env)) (f' num (num + 1))) t1 t2
   Let n e f' -> Let n (f num env e) (\b -> f (num + 1) (Map.insert num b env) (f' num))
   LetRec n t b f' -> LetRec n t (\bs ->
                                   let len = length bs
                                       bs' = b [num .. num + len - 1]
                                   in map (f (num + len) (foldl' multInsert env (zip [num .. num + len - 1] bs))) bs')
                                 (\es ->
                                   let len = length es
                                       es' = f' [num .. num + len - 1]
                                   in f (num + len) (foldl' multInsert env (zip [num .. num + len - 1] es)) es')
   BLam n f' -> BLam n (f num env . f')
   App e1 e2 -> App (f num env e1) (f num env e2)
   TApp e t -> TApp (f num env e) t
   If e1 e2 e3 -> If (f num env e1) (f num env e2) (f num env e3)
   PrimOp e1 op e2 -> PrimOp (f num env e1) op (f num env e2)
   Tuple es -> Tuple (map (f num env) es)
   Proj n e -> Proj n (f num env e)
   JNew n es -> JNew n (map (f num env) es)
   JMethod e b es d -> JMethod (fmap (f num env) e) b (map (f num env) es) d
   JField e a b -> JField (fmap (f num env) e) a b
   PolyList es t -> PolyList (map (f num env) es) t
   JProxyCall j t -> JProxyCall (f num env j) t
   Seq es -> Seq (map (f num env) es)
   Constr c es -> Constr c (map (f num env) es)
   Case _ _ -> sorry "Rewriting case not yet done"

newtype Exp = Hide {reveal :: forall t e. Expr t e}

instance Eq Exp where
  p1 == p2 = peq 0 (reveal p1) (reveal p2)

peq :: Int -> Expr Int Int -> Expr Int Int -> Bool
peq _ (Var _ a) (Var _ b) = a == b
peq _ (Lit a) (Lit b) = a == b
peq n (Lam _ _ f1) (Lam _ _ f2) = peq (n + 1) (f1 n) (f2 n)
peq n (Fix _ _ f1 _ _) (Fix _ _ f2 _ _) = peq (n + 2) (f1 n (n + 1)) (f2 n (n + 1))
peq n (Let _ x f1) (Let _ y f2) = peq n x y && peq (n + 1) (f1 n) (f2 n)
peq n (LetRec _ _ f1 g1) (LetRec _ _ f2 g2) =
  let bind1 = f1 [n ..]
      len1 = length bind1
      bind2 = f2 [n ..]
      len2 = length bind2
      expr1 = g1 [n ..]
      expr2 = g2 [n ..]
  in len1 == len2 && peq (n + len1) expr1 expr2 && and (zipWith (peq (n + len1)) bind1 bind2)
peq n (BLam _ f1) (BLam _ f2) = peq n (f1 0) (f2 0) -- 0 for type index, OK?
peq n (App e1 e2) (App g1 g2) = peq n e1 g1 && peq n e2 g2
peq n (TApp e1 _) (TApp e2 _) = peq n e1 e2
peq n (If e1 e2 e3) (If g1 g2 g3) = peq n e1 g1 && peq n e2 g2 && peq n e3 g3
peq n (PrimOp e1 _ e2) (PrimOp g1 _ g2) = peq n e1 g1 && peq n e2 g2
peq n (Tuple es) (Tuple gs) =
  let len1 = length es
      len2 = length gs
  in len1 == len2 && and (zipWith (peq n) es gs)
peq n (Proj _ e) (Proj _ g) = peq n e g
peq n (JNew _ es) (JNew _ gs) =
  let len1 = length es
      len2 = length gs
  in len1 == len2 && and (zipWith (peq n) es gs)
peq n (JMethod e1 _ es _) (JMethod g1 _ gs _) =
  let len1 = length es
      len2 = length gs
  in len1 == len2 && checkCall n e1 g1 && and (zipWith (peq n) es gs)
peq n (JField e1 _ _) (JField g1 _ _) = checkCall n e1 g1
peq n (PolyList es _) (PolyList gs _) = and (zipWith (peq n) es gs)
peq n (JProxyCall e _) (JProxyCall g _) = peq n e g
peq n (Seq es) (Seq gs) = and (zipWith (peq n) es gs)
peq _ _ _ = False

checkCall :: Int -> Src.JCallee (Expr Int Int) -> Src.JCallee (Expr Int Int) -> Bool
checkCall _ (Src.Static s1) (Src.Static s2) = s1 == s2
checkCall n (Src.NonStatic e1) (Src.NonStatic e2) = peq n e1 e2
checkCall _ _ _ = False

src2core :: String -> IO (Expr t e)
src2core fname = liftM simplify $ src2fi fname

fCore :: (Expr t e -> b) -> String -> IO b
fCore f fname = liftM f $ src2core fname

src2fi :: String -> IO (FI.Expr t e)
src2fi fname = do
     path <- getCurrentDirectory
     string <- readFile (path ++ "/" ++ fname)
     result <- typeCheck . reader $ string
     case result of
       Left typeError -> error $ show typeError
       Right (_, tcheckedSrc) ->
             return . desugar $ tcheckedSrc

multInsert :: Map.Map Int e -> (Int, e) -> Map.Map Int e
multInsert m (key, value) = Map.insert key value m
