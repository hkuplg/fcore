{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}
{- |
Module      :  Simplify
Description :  The simplifier turns SystemFI into Core.
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Zhiyuan Shi <zhiyuan.shi@gmail.com>, Haoyuan Zhang <zhanghaoyuan00@gmail.com>
Stability   :  experimental
Portability :  portable

The simplifier translates System F with intersection types to vanilla System F.
-}

module SimplifyImpl
  ( simplify
  , simplify'
  , infer
  , transExpr
  , transType
  , coerce
  , dedeBruT
  , dedeBruE
  ) where

import Core
import Panic
import qualified SystemFI             as FI
import qualified Src                  as S
import qualified Language.Java.Syntax as J

import Text.PrettyPrint.ANSI.Leijen

import Debug.Trace   (trace)
import Data.Maybe    (fromMaybe)
import Control.Monad (zipWithM)
import Unsafe.Coerce (unsafeCoerce)

simplify :: FI.FExp -> Expr t e
simplify = dedeBruE 0 [] 0 [] . transExpr 0 0 . FI.revealF

simplify' :: FI.Expr t e -> Expr t e
simplify' = dedeBruE 0 [] 0 [] . transExpr 0 0 . unsafeCoerce

infer :: Index -> Index -> FI.Expr Index (Index, FI.Type Index) -> FI.Type Index
infer i j (FI.Var _ (_, t))       = t
infer i j (FI.Lit (S.Int    _))   = FI.JClass "java.lang.Integer"
infer i j (FI.Lit (S.String _))   = FI.JClass "java.lang.String"
infer i j (FI.Lit (S.Bool   _))   = FI.JClass "java.lang.Boolean"
infer i j (FI.Lit (S.Char   _))   = FI.JClass "java.lang.Character"
infer i j (FI.Lit  S.UnitLit)     = FI.Unit
infer i j (FI.Lam n t f)          = FI.Fun t . infer i (j + 1) $ f (j, t)
infer i j (FI.Fix _ _ _ t1 t)     = FI.Fun t1 t
infer i j (FI.Let _ b f)          = infer i (j + 1) $ f (j, infer i j b)
infer i j (FI.LetRec _ ts _ e)    = infer i (j + n) $ e (zip [j..j+n-1] ts)      where n = length ts
infer i j (FI.BLam n f)           = FI.Forall n (\a -> infer (i + 1) j $ f a)
infer i j (FI.App f x)            = t                                            where FI.Fun _ t = infer i j f
infer i j (FI.TApp f x)           = FI.mapTVar (\n1 a1 -> if a1 == i + 10000 && n1 == n then x else FI.TVar n1 a1) (g (i + 10000))
  where FI.Forall n g = infer (i + 1) j f
  -- Warning! Workaround: please use distinct names for sorts.
infer i j (FI.If _ e _)           = infer i j e
infer i j (FI.PrimOp _ op _)      = case op of S.Arith   _ -> FI.JClass "java.lang.Integer"
                                               S.Compare _ -> FI.JClass "java.lang.Boolean"
                                               S.Logic   _ -> FI.JClass "java.lang.Boolean"
infer i j (FI.Tuple es)           = FI.Product . map (infer i j) $ es
infer i j (FI.Proj index e)       = ts !! (index - 1)                            where FI.Product ts = infer i j e
infer i j (FI.JNew c _)           = FI.JClass c
infer i j (FI.JMethod _ _ _ c)    = FI.JClass c
infer i j (FI.JField _ _ c)       = FI.JClass c
infer i j (FI.Seq es)             = infer i j (last es)
infer i j (FI.Merge e1 e2)        = FI.And (infer i j e1) (infer i j e2)
infer i j (FI.RecordCon (l, e))   = FI.RecordType (l, infer i j e)
infer i j (FI.RecordProj e l1)    = t1                                           where Just (t1, _) = getter i j (infer i j e) l1
infer i j (FI.RecordUpdate e _)   = infer i j e
infer i j (FI.PolyList _ t)       = FI.ListOf t
infer i j (FI.JProxyCall _ t)     = t
infer i j (FI.Constr c _)         = last . FI.constrParams $ c
infer i j (FI.Case _ alts)        = inferAlt . head $ alts
  where inferAlt (FI.ConstrAlt c _ e) = let (ts, n) = (FI.constrParams c, length ts - 1)
                                        in infer i (j + n) (e (zip [j..] (init ts)))
infer i j (FI.Data _ _ e)         = infer i j e
infer _ _ _                       = trace "Unsupported: Simplify.infer" FI.Unit

transExpr :: Index -> Index -> FI.Expr Index (Index, FI.Type Index) -> Expr Index Index
transExpr i j (FI.Var n (x, _))          = Var n x
transExpr i j (FI.Lit l)                 = Lit l
transExpr i j (FI.Lam n t f)             = Lam n (transType i t) (\x -> transExpr i (j + 1) $ f (x, t))
transExpr i j this@(FI.Fix fn pn e t1 t) = Fix fn pn e' t1' t'
  where
    e'        = \x x1 -> transExpr i (j + 2) $ e (x, infer i j this) (x1, t1)
    (t1', t') = (transType i t1, transType i t)
transExpr i j (FI.Let n b f)             = Let n (transExpr i j b) (\x -> transExpr i (j + 1) $ f (x, infer i j b))
transExpr i j (FI.LetRec ns ts bs e)     = LetRec ns ts' bs' e'
  where
    ts' = map (transType i) ts
    bs' args = map (transExpr i (j + n)) . bs $ zip args ts
    e'  args = transExpr i (j + n) . e $ zip args ts
    n = length ts
transExpr i j (FI.BLam n f)              = BLam n (\a -> transExpr (i + 1) j $ f a)
transExpr i j (FI.App f x) =
  let (FI.Fun t1 t2, e1) = (infer i j f, transExpr i j f)
      (t3, e2)           = (infer i j x, transExpr i j x)
      panic_doc          = text "Coercion failed" <$>
                           text "Function: " <> pretty_typing f (FI.Fun t1 t2) <$>
                           text "Argument: " <> pretty_typing x t3 <$>
                           text "Coercion: " <> pretty_coercion t3 t1
      pretty_typing temp1 temp2   = FI.prettyExpr (unsafeCoerce temp1 :: FI.Expr Index Index) <+> colon <+>
                                    FI.prettyType (unsafeCoerce temp2 :: FI.Type Index)
      pretty_coercion temp1 temp2 = FI.prettyType (unsafeCoerce temp1 :: FI.Type Index) <+> text "<:" <+>
                                    FI.prettyType (unsafeCoerce temp2 :: FI.Type Index)
  in let c = fromMaybe (prettyPanic "Simplify.transExpr" panic_doc) (coerce i t3 t1)
     in App e1 (App c e2)
transExpr i j (FI.TApp f x)                = TApp (transExpr i j f) (transType i x)
transExpr i j (FI.If e1 e2 e3)             = If e1' e2' e3'
  where [e1', e2', e3'] = map (transExpr i j) [e1, e2, e3]
transExpr i j (FI.PrimOp e1 op e2)         = PrimOp (transExpr i j e1) op (transExpr i j e2)
transExpr i j (FI.Tuple es)                = Tuple . map (transExpr i j) $ es
transExpr i j (FI.Proj index e)            = Proj index $ transExpr i j e
transExpr i j (FI.JNew c es)               = JNew c . map (transExpr i j) $ es
transExpr i j (FI.JMethod c m arg ret)     = JMethod (fmap (transExpr i j) c) m (map (transExpr i j) arg) ret
transExpr i j (FI.JField c m ret)          = JField (fmap (transExpr i j) c) m ret
transExpr i j (FI.Seq es)                  = Seq . map (transExpr i j) $ es
transExpr i j (FI.Merge e1 e2)             = Tuple . map (transExpr i j) $ [e1, e2]
transExpr i j (FI.RecordCon (l, e))        = transExpr i j e
transExpr i j (FI.RecordProj e l1)         = App c $ transExpr i j e
  where Just (_, c) = getter i j (infer i j e) l1
transExpr i j (FI.RecordUpdate e (l1, e1)) = App c $ transExpr i j e
  where Just (_, c) = putter i j (infer i j e) l1 (transExpr i j e1)
transExpr i j (FI.Constr (FI.Constructor n ts) es) = Constr (Constructor n . map (transType i) $ ts) . map (transExpr i j) $ es
transExpr i j (FI.Case e alts)             = Case e' . map transAlt $ alts
  where
    e' = transExpr i j e
    transAlt (FI.ConstrAlt (FI.Constructor n ts) ns f) =
      let m   = length ts - 1
          ts' = map (transType i) ts
      in ConstrAlt (Constructor n ts') ns (\es -> transExpr i (j + m) . f $ zip es ts) -- Right?
transExpr i j (FI.PolyList es t)           = PolyList (map (transExpr i j) es) (transType i t)
transExpr i j (FI.JProxyCall e t)          = JProxyCall (transExpr i j e) (transType i t)
transExpr i j (FI.Data recflag databinds e)= Data recflag (map transDatabind databinds) (transExpr i j e)
  where transCtr (FI.Constructor name cps) = Constructor name . map (transType i) $ cps
        transDatabind (FI.DataBind n params ctrs) = DataBind n params (map transCtr . ctrs)
transExpr _ _ _                            = trace "Unsupported: Simplify.transExpr" (Var "" (-1))

transType :: Index -> FI.Type Index -> Type Index
transType i (FI.TVar n a)          = TVar n a
transType i (FI.JClass c)          = JClass c
transType i (FI.Fun a1 a2)         = Fun (transType i a1) (transType i a2)
transType i (FI.Forall n f)        = Forall n (\a -> transType (i + 1) $ f a)
transType i (FI.Product ts)        = Product . map (transType i) $ ts
transType i (FI.Unit)              = Unit
transType i (FI.And a1 a2)         = Product . map (transType i) $ [a1, a2]
transType i (FI.RecordType (_, t)) = transType i t
transType i (FI.Datatype n ts ns)  = Datatype n (map (transType i) ts) ns
transType i (FI.ListOf t)          = ListOf . transType i $ t
transType _ _                      = trace "Unsupported: Simplify.transType" (TVar "" (-1))

coerce :: Index -> FI.Type Index -> FI.Type Index -> Maybe (Expr Index Index)
coerce i this@(FI.TVar _ a) (FI.TVar _ b)
  | a == b = return $ lam (transType i this) var
  | otherwise = Nothing
coerce i this@(FI.JClass c) (FI.JClass d)
  | c == d = return $ lam (transType i this) var
  | otherwise = Nothing
coerce i this@(FI.Fun t1 t2) (FI.Fun t3 t4) = do
  c1 <- coerce i t3 t1
  c2 <- coerce i t2 t4
  return $ lam (transType i this) (\f -> lam (transType i t3) $ (App c2 . App (var f) . App c1) . var)
coerce i this@(FI.Forall _ f) (FI.Forall _ g) = do
  c <- coerce (i + 1) (f i) (g i)
  return $ lam (transType i this) (\f' -> bLam $ (App c . TApp (var f')) . TVar "_")
coerce i this@(FI.Product ss) (FI.Product ts)
  | length ss /= length ts = Nothing
  | otherwise = do
      cs <- zipWithM (coerce i) ss ts
      let f x = Tuple $ zipWith (\c idx -> App c $ Proj idx x) cs [1..length ss]
      return $ lam (transType i this) (f . var)
coerce i this@(FI.Unit) (FI.Unit) = return $ lam (transType i this) var
coerce i t1 (FI.And t2 t3) = do
  c1 <- coerce i t1 t2
  c2 <- coerce i t1 t3
  return $ lam (transType i t1) (\x -> Tuple [App c1 (var x), App c2 (var x)])
coerce i this@(FI.And t1 t2) t3 =
  case coerce i t1 t3 of
    Just c  -> return $ lam (transType i this) (App c . Proj 1 . var)
    Nothing -> case coerce i t2 t3 of
                 Just c  -> return $ lam (transType i this) (App c . Proj 2 . var)
                 Nothing -> Nothing
coerce i (FI.RecordType (l1, t1)) (FI.RecordType (l2, t2))
  | l1 == l2  = coerce i t1 t2
  | otherwise = Nothing
coerce i this@(FI.Datatype n1 _ _) (FI.Datatype n2 _ _)
  | n1 == n2  = return $ lam (transType i this) var
  | otherwise = Nothing
coerce i (FI.ListOf t1) (FI.ListOf t2) = coerce i t1 t2
coerce _ _ _ = Nothing

getter :: Index -> Index -> FI.Type Index -> S.Label -> Maybe (FI.Type Index, Expr Index Index)
getter i j this@(FI.RecordType (l, t)) l1
  | l1 == l = return $ (t, lam (transType i this) var)
  | otherwise = Nothing
getter i j this@(FI.And t1 t2) l =
  case getter i j t2 l of
    Just (t, c) -> return $ (t, lam (transType i this) (App c . Proj 2 . var))
    Nothing     -> case getter i j t1 l of
                     Just (t, c) -> return $ (t, lam (transType i this) (App c . Proj 1 . var))
                     Nothing     -> Nothing
getter _ _ _ _ = Nothing

putter :: Index -> Index -> FI.Type Index -> S.Label -> Expr Index Index -> Maybe (FI.Type Index, Expr Index Index)
putter i j this@(FI.RecordType (l, t)) l1 e
  | l1 == l = return $ (t, lam (transType i this) (Prelude.const e))
  | otherwise = Nothing
putter i j this@(FI.And t1 t2) l e =
  case putter i j t2 l e of
    Just (t, c) -> return $ (t, lam (transType i this) (\x -> Tuple [Proj 1 . var $ x, App c . Proj 2 . var $ x]))
    Nothing     -> case putter i j t1 l e of
                     Just (t, c) -> return $ (t, lam (transType i this) (\x -> Tuple [App c . Proj 1 . var $ x, Proj 2 . var $ x]))
                     Nothing     -> Nothing
putter _ _ _ _ _ = Nothing

subst :: (S.ReaderId -> Index -> FI.Type Index) -> [FI.Type Index] -> FI.Type Index
subst g ts@((FI.TVar n a):_)      = if const then g n a else FI.TVar n a
  where const = all (== a) . map (\x -> let FI.TVar _ y = x in y) $ ts
subst g ((FI.JClass c):_)         = FI.JClass c
subst g ts@((FI.Fun _ _):_)       = FI.Fun (subst g ts1) (subst g ts2)
  where (ts1, ts2) = unzip . map (\x -> let FI.Fun t1 t2 = x in (t1, t2)) $ ts
subst g ts@((FI.Forall n _):_)    = FI.Forall n (\z -> subst g $ ts' z)
  where ts' z = concat . map (\x -> let FI.Forall _ f = x in [f z, f (z + 1)]) $ ts
subst g ts@((FI.Product _):_)     = FI.Product ts'
  where
    ts' = map (subst g) . transpose . map (\x -> let FI.Product hs = x in hs) $ ts
    transpose :: [[a]] -> [[a]]
    transpose [] = []
    transpose xs = (map head xs):(transpose . map tail $ xs)
subst g ((FI.Unit):_)             = FI.Unit
subst g ts@((FI.And _ _):_)       = FI.And (subst g ts1) (subst g ts2)
  where (ts1, ts2) = unzip . map (\x -> let FI.And t1 t2 = x in (t1, t2)) $ ts
subst g ts@((FI.RecordType (l, _)):_) = FI.RecordType (l, t')
  where t' = subst g . map (\x -> let FI.RecordType (_, t) = x in t) $ ts
subst g ts@((FI.Datatype n _ ns):_) = FI.Datatype n ts' ns
  where
    ts' = map (subst g) . transpose . map (\x -> let FI.Datatype _ hs _ = x in hs) $ ts
    transpose :: [[a]] -> [[a]]
    transpose [] = []
    transpose xs = (map head xs):(transpose . map tail $ xs)
subst g ts@((FI.ListOf _):_) = FI.ListOf t'
  where t' = subst g . map (\x -> let FI.ListOf t = x in t) $ ts

substTT :: Index -> FI.Type Index -> FI.Type Index -> FI.Type Index
substTT i x t = subst (\n a -> if a == i then x else FI.TVar n a) [t]

dedeBruT :: Index -> [t] -> Type Index -> Type t
dedeBruT _ as (TVar n i)         = TVar n (reverse as !! i)
dedeBruT _ _  (JClass c)         = JClass c
dedeBruT i as (Fun t1 t2)        = Fun (dedeBruT i as t1) (dedeBruT i as t2)
dedeBruT i as (Forall n f)       = Forall n (\a -> dedeBruT (i + 1) (a:as) (f i))
dedeBruT i as (Product ts)       = Product (map (dedeBruT i as) ts)
dedeBruT i as (Datatype n ts ns) = Datatype n (map (dedeBruT i as) ts) ns
dedeBruT i as (ListOf t)         = ListOf (dedeBruT i as t)
dedeBruT i as  Unit              = Unit

dedeBruE :: Index -> [t] -> Index -> [e] -> Expr Index Index -> Expr t e
dedeBruE _ _  _ xs (Var n i)                      = Var n (reverse xs !! i)
dedeBruE _ _  _ _  (Lit l)                        = Lit l
dedeBruE i as j xs (Lam n t f)                    = Lam n
                                                      (dedeBruT i as t)
                                                      (\x -> dedeBruE i as (j + 1) (x:xs) (f j))
dedeBruE i as j xs (Fix fn pn f t1 t)             = Fix fn pn
                                                      (\x x1 -> dedeBruE i as (j + 2) (x1:x:xs) $ f j (j + 1))
                                                      (dedeBruT i as t1)
                                                      (dedeBruT i as t)
dedeBruE i as j xs (Let n e f)                    = Let n
                                                      (dedeBruE i as j xs e)
                                                      (\x -> dedeBruE i as (j + 1) (x:xs) (f j))
dedeBruE i as j xs (LetRec ns ts fs e)            = LetRec ns
                                                      (map (dedeBruT i as) ts)
                                                      (\xs' -> map (dedeBruE i as (j + n) ((reverse xs') ++ xs)) (fs [j..j + n - 1]))
                                                      (\xs' -> dedeBruE i as (j + n) ((reverse xs') ++ xs) (e [j..j + n - 1]))
                                                      where n = length ts
dedeBruE i as j xs (BLam n f)                     = BLam n (\a -> dedeBruE (i + 1) (a:as) j xs (f i))
dedeBruE i as j xs (App f x)                      = App
                                                      (dedeBruE i as j xs f)
                                                      (dedeBruE i as j xs x)
dedeBruE i as j xs (TApp f a)                     = TApp
                                                       (dedeBruE i as j xs f)
                                                       (dedeBruT i as a)
dedeBruE i as j xs (If p b1 b2)                   = If p' b1' b2' where [p',b1',b2'] = map (dedeBruE i as j xs) [p,b1,b2]
dedeBruE i as j xs (PrimOp e1 op e2)              = PrimOp
                                                      (dedeBruE i as j xs e1) op
                                                      (dedeBruE i as j xs e2)
dedeBruE i as j xs (Tuple es)                     = Tuple (map (dedeBruE i as j xs) es)
dedeBruE i as j xs (Proj index e)                 = Proj index (dedeBruE i as j xs e)
dedeBruE i as j xs (JNew c args)                  = JNew c (map (dedeBruE i as j xs) args)
dedeBruE i as j xs (JMethod callee m args r)      = JMethod
                                                      (fmap (dedeBruE i as j xs) callee) m
                                                      (map (dedeBruE i as j xs) args) r
dedeBruE i as j xs (JField callee f r)            = JField (fmap (dedeBruE i as j xs) callee) f r
dedeBruE i as j xs (Seq es)                       = Seq (map (dedeBruE i as j xs) es)
dedeBruE i as j xs (Constr (Constructor n ts) es) = Constr
                                                      (Constructor n (map (dedeBruT i as) ts))
                                                      (map (dedeBruE i as j xs) es)
dedeBruE i as j xs (Case e alts)                  = Case (dedeBruE i as j xs e) (map dedeBruijnAlt alts)
  where dedeBruijnAlt (ConstrAlt (Constructor name ts) names fe) =
          ConstrAlt
            (Constructor name (map (dedeBruT i as) ts)) names
            (\xs' -> dedeBruE i as (j + n) ((reverse xs') ++ xs) (fe [j..j + n - 1])) where n = length ts - 1

dedeBruE i as j xs (Data recflag databinds e)     = Data recflag (map dedeBruijnDatabind databinds) (dedeBruE i as j xs e)
  where dedeBruijnConstr i' as' (Constructor name types) = Constructor name (map (dedeBruT i' as') types)
        dedeBruijnDatabind (DataBind n ns ctrs)   = DataBind n ns (\a -> map (dedeBruijnConstr (length ns + i) (a++as)) $
                                                                         (ctrs [i..length ns+i-1]) )

dedeBruE i as j xs (PolyList es t)                = PolyList (map (dedeBruE i as j xs) es) (dedeBruT i as t)
dedeBruE i as j xs (JProxyCall e t)               = JProxyCall (dedeBruE i as j xs e) (dedeBruT i as t)

