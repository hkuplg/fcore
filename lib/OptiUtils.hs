{-# LANGUAGE RankNTypes #-}
-- | Some utilities for optimization purpose

module OptiUtils where

import           Data.List (foldl')
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)

import           Core
import qualified Src



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
joinExpr (Error ty str)         = Error ty (joinExpr str)
joinExpr (Data recflag databinds e) = Data recflag databinds (joinExpr e)
joinExpr (ConstrOut ctr es) = ConstrOut ctr (map joinExpr es)
joinExpr (Case e alts) = Case (joinExpr e) (map joinAlt alts)
  where joinAlt (ConstrAlt ctr e1) = ConstrAlt ctr (joinExpr e1)
        joinAlt (Default e1) = Default (joinExpr e1)
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
joinExpr (Module pname defs) = Module pname (joinDefs defs)

joinDefs :: Definition t (Expr t e) -> Definition t e
joinDefs Null = Null
joinDefs (Def n t expr def) = Def n t (joinExpr expr) (joinDefs . def . Var n)
joinDefs (DefRec names types exprs def) =
  DefRec names types (map joinExpr . exprs . zipWith Var names) (joinDefs . def . zipWith Var names)

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
      Module pname defs -> Module pname (mapDefs defs)
      JNew cname es -> JNew cname (map f es)
      JMethod cnameOrE mname es cname -> JMethod (fmap f cnameOrE) mname (map f es) cname
      JField cnameOrE fname cname -> JField (fmap f cnameOrE) fname cname
      Error ty str -> Error ty (f str)
      Data recflag databinds e -> Data recflag databinds (f e)
      ConstrOut ctr es -> ConstrOut ctr (map f es)
      Case e' alts -> Case (f e') (map mapAlt alts)
         where mapAlt (ConstrAlt ctr e1) = ConstrAlt ctr (f e1)
               mapAlt (Default e1) = Default (f e1)
      Seq es -> Seq $ map f es
  where mapDefs (Def n t expr def) = Def n t (mapExpr f expr) (mapDefs . def)
        mapDefs (DefRec names types exprs def) = DefRec names types (map f . exprs) (mapDefs . def)
        mapDefs Null = Null


rewriteExpr :: (Int -> Map.Map Int e -> Expr t Int -> Expr t e) -> Int -> Map.Map Int e -> Expr t Int -> Expr t e
rewriteExpr f num env expr =
  case expr of
   Var n v -> Var n (fromJust $ Map.lookup v env)
   Lit n ->  Lit n
   Lam n t f' -> Lam n t (\e -> f (num + 1) (Map.insert num e env) (f' num))
   Fix n1 n2 f' t1 t2 -> Fix n1 n2 (\a b -> f (num + 2) (Map.insert (num + 1) b (Map.insert num a env)) (f' num (num + 1))) t1 t2
   Let n e f' -> Let n (f num env e) (\b -> f (num + 1) (Map.insert num b env) (f' num))
   LetRec n t b f' ->
     let len = length n
         range = [num .. num + len - 1]
     in LetRec n t (\bs -> let bs' = b range
                           in map (f (num + len) (foldl' multInsert env (zip range bs))) bs')
                   (\es -> let es' = f' range
                           in f (num + len) (foldl' multInsert env (zip range es)) es')
   BLam n f' -> BLam n (f num env . f')
   App e1 e2 -> App (f num env e1) (f num env e2)
   TApp e t -> TApp (f num env e) t
   If e1 e2 e3 -> If (f num env e1) (f num env e2) (f num env e3)
   PrimOp e1 op e2 -> PrimOp (f num env e1) op (f num env e2)
   Tuple es -> Tuple (map (f num env) es)
   Proj n e -> Proj n (f num env e)
   Module pname defs -> Module pname (rewriteDefs num env defs)
   JNew n es -> JNew n (map (f num env) es)
   JMethod e b es d -> JMethod (fmap (f num env) e) b (map (f num env) es) d
   JField e a b -> JField (fmap (f num env) e) a b
   Error ty str -> Error ty (f num env str)
   Seq es -> Seq (map (f num env) es)
   Data recflag databinds e -> Data recflag databinds (f num env e)
   ConstrOut c es -> ConstrOut c (map (f num env) es)
   Case e alts -> Case (f num env e) (map rewriteAlt alts)
 where
   rewriteAlt (ConstrAlt ctr e) = ConstrAlt ctr (f num env e)
   rewriteAlt (Default e)       = Default (f num env e)

   rewriteDefs _ _  Null = Null
   rewriteDefs num env (Def n t expr def) = Def n t (rewriteExpr f num env expr)
                                                    (\e -> rewriteDefs (num + 1)
                                                    (Map.insert num e env)
                                                    (def num))
   rewriteDefs num env (DefRec names types exprs def) =
     let len = length names
         range = [num .. num + len - 1]
     in DefRec names types (\bs -> let bs' = exprs range
                                   in map (f (num + len) (foldl' multInsert env (zip range bs))) bs')
                           (\es -> let es' = def range
                                   in rewriteDefs (num + len) (foldl' multInsert env (zip range es)) es')

newtype Exp = Hide {reveal :: forall t e. Expr t e}

instance Eq Exp where
  p1 == p2 = peq 0 (reveal p1) (reveal p2)

-- TODO: equality inside module?
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
peq n (Error _ str1) (Error _ str2) = peq n str1 str2
peq n (Seq es) (Seq gs) = and (zipWith (peq n) es gs)
peq _ _ _ = False

checkCall :: Int -> Src.JReceiver (Expr Int Int) -> Src.JReceiver (Expr Int Int) -> Bool
checkCall _ (Src.Static s1) (Src.Static s2) = s1 == s2
checkCall n (Src.NonStatic e1) (Src.NonStatic e2) = peq n e1 e2
checkCall _ _ _ = False

multInsert :: Map.Map Int e -> (Int, e) -> Map.Map Int e
multInsert m (key, value) = Map.insert key value m
