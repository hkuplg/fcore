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
import qualified SystemFI as FI

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
   Seq es -> Seq (map (f num env) es)
   Constr c es -> Constr c (map (f num env) es)
   Case _ _ -> sorry "Rewriting case not yet done"

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
