module PartialEvaluator where
    
import Core
import OptiUtils
import Examples
import qualified Src as S
import qualified Language.Java.Syntax as J (Op(..))
    
peval :: Expr t (Expr t e) -> Expr t e
peval (Var x) = x
peval (Lam t f) = Lam t (\x -> peval . f . Var $ x)
peval (BLam f) = BLam (\t -> peval . f $ t)
peval (App e1 e2) = -- not correct
    case e1 of
      (Lam t f) -> peval . f . peval $ e2
      fix@(Fix _ _ _) -> peval fix
      _ -> peval $ App (peval e1) (peval e2)
peval (TApp e t) = TApp (peval e) t
peval (Lit s) = Lit s
peval (If e1 e2 e3) = 
    case e1' of 
      Lit (S.Boolean True) -> peval e2 
      Lit (S.Boolean False) -> peval e3
      _ -> If e1' (peval e2) (peval e3)
    where e1' = peval e1
peval (PrimOp e1 op e2) = -- Not exhausitive
    case (e1', e2') of
      (Lit (S.Integer a), Lit (S.Integer b)) ->
          case op of
            S.Arith J.Sub -> Lit . S.Integer $ a - b
            S.Compare J.Equal -> Lit . S.Boolean $ a == b
            _ -> simplified
      (Lit (S.Boolean a), Lit (S.Boolean b)) ->
          case op of
            S.Logic J.And -> Lit . S.Boolean $ a && b
            _ -> simplified
      _ -> simplified
    where e1' = peval e1
          e2' = peval e2
          simplified = PrimOp e1' op e2'
peval (Tuple es) = Tuple $ map peval es
peval (Proj i e) = Proj i (peval e)
peval (Fix f t1 t) = Fix (\e1 e2 -> peval $ f (Var e1) (Var e2)) t1 t
peval (LetRec sigs binds body) = 
    LetRec sigs 
           (\es -> map peval . binds $ map Var es) 
           (\es -> peval . body $ map Var es)
peval (JNewObj cname es) = JNewObj cname (map peval es)
peval (JMethod cnameOrE mname es cname) = JMethod (fmap peval cnameOrE) mname (map peval es) cname
peval (JField cnameOrE fname cname) = JField (fmap peval cnameOrE) fname cname
peval (Seq es) = Seq $ map peval es
peval (Merge e1 e2) = Merge (peval e1) (peval e2)

identity :: Expr t e
identity = Lam javaInt (\x -> Var x)
id_1_1 = peval (App identity one) `eq` one

id_twice = Lam javaInt (\x -> App identity (Var x))
id_twice_1_1 = peval (App id_twice one) `eq` one

minus :: Expr t e
minus = Lam javaInt (\x -> Lam javaInt (\y -> Var x `sub` Var y))

-- test nested App
minus_1 = App minus one
zero_minus_1 = App minus_1 zero
minus_1_0_be_1 = peval (App (App minus one) zero) `eq` one

app_lam_if = App (Lam javaInt (\x -> If ((Var x) `eq` one)
                                 ((Var x) `sub` one)
                                 (zero `sub` one)))
          (zero `sub` one)
app_lam_app = App (Lam javaInt (\x -> App (Lam javaInt (\y -> (Var x) `sub` (Var y))) zero))
       one

fix = Fix (\f n -> If (((one `sub` zero) `eq` zero))
                   one
                   (Var n `mult` (Var f `App` (Var n `sub` one))))
      javaInt
      (javaInt `Fun` javaInt)
app_fix = App fix (Lit (S.Integer 10))
