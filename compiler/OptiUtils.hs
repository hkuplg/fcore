-- | Some utilities for optimization purpose

module OptiUtils where

import Core

joinExpr :: Expr t (Expr t e) -> Expr t e
joinExpr (Var x) = x
joinExpr (Lam t1 e1) = Lam t1 (\ee -> joinExpr (e1 (Var ee)))
joinExpr (App e1 e2) = App (joinExpr e1) (joinExpr e2)
joinExpr (BLam t1) = BLam (\t2 -> joinExpr (t1 t2))
joinExpr (TApp e t) = TApp (joinExpr e) t
joinExpr (Lit s) = Lit s
joinExpr (If e1 e2 e3) = If (joinExpr e1) (joinExpr e2) (joinExpr e3)
joinExpr (PrimOp e1 o e2) = PrimOp (joinExpr e1) o (joinExpr e2)
joinExpr (Tuple es) = Tuple (map joinExpr es)
joinExpr (Proj i e) = Proj i (joinExpr e)
joinExpr (Fix f t1 t2) = Fix (\e1 e2 -> joinExpr (f (Var e1) (Var e2))) t1 t2
joinExpr (LetRec s b1 b2) =
  LetRec s
          (\es -> map joinExpr (b1 (map Var es)))
          (\es -> joinExpr (b2 (map Var es)))
joinExpr (JNewObj name es) = JNewObj name (map joinExpr es)
joinExpr (JMethod jc m es cn) =
  JMethod (fmap joinExpr jc) m (map joinExpr es) cn
joinExpr (JField jc fn cn) = JField (fmap joinExpr jc) fn cn
joinExpr (Seq es) = Seq (map joinExpr es)
joinExpr (Merge e1 e2) = Merge (joinExpr e1) (joinExpr e2)

mapExpr :: (Expr t e -> Expr t e) -> Expr t e -> Expr t e
mapExpr f e =
    case e of
      Var x -> e
      Lit l -> e
      Lam t g -> Lam t (\x -> f . g $ x)
      Fix g t1 t -> Fix (\e1 e2 -> f $ g e1 e2) t1 t
      LetRec sigs binds body ->
          LetRec sigs
                 (\es -> map f $ binds es)
                 (\es -> f $ body es)
      BLam g -> BLam (\x -> f . g $ x)
      App e1 e2 -> App (f e1) (f e2)
      TApp e t -> TApp (f e) t
      If e1 e2 e3 -> If (f e1) (f e2) (f e3)
      PrimOp e1 op e2 -> PrimOp (f e1) op (f e2)
      Tuple es -> Tuple $ map f es
      Proj i e -> Proj i (f e)
      JNewObj cname es -> JNewObj cname (map f es)
      JMethod cnameOrE mname es cname -> JMethod (fmap f cnameOrE) mname (map f es) cname
      JField cnameOrE fname cname -> JField (fmap f cnameOrE) fname cname
      Seq es -> Seq $ map f es
      Merge e1 e2 -> Merge (f e1) (f e2)