{- |
Module      :  Z3Backend
Description :  The Z3 backend for symbolic
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Weixin <zhangweixinxd@gmail.com>
Stability   :  unstable
Portability :  portable

Symbolic evaluator will generates all possible execution paths for a given program. We use Z3 to kick out those infeasible execution paths.
-}

module Z3Backend where

import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.IntMap                  as IntMap
import           Data.Maybe                   (fromJust)
import           Prelude                      hiding (EQ, GT, LT)
import           PrettyUtils
import qualified Src                          as S
import           SymbolicEvaluator
import           SystemFI
import           Text.PrettyPrint.ANSI.Leijen
import           Z3.Monad                     hiding (Z3Env)
import Data.Map (Map, fromList)

data Z3Env = Z3Env { index                      :: Int
                   , boolSort, intSort, adtSort :: Sort
                   -- , constrFuns :: SConstructor -> ConstrFun
                   , symVars                    :: IntMap.IntMap AST
                   , funVars                    :: IntMap.IntMap FuncDecl
                   , target                     :: Doc -> SymValue -> Z3 ()
                   }

solve :: Expr () ExecutionTree -> IO ()
solve = solve' 20

solve' :: Int -> Expr () ExecutionTree -> IO ()
solve' stop e =
    evalZ3 $ do
      int <- mkIntSort
      bool <- mkBoolSort
      -- All datatypes are treated as one adt
      adt <- mkStringSymbol "adtSort" >>= mkUninterpretedSort
      -- prelude (int,bool,adt) ts

      let (tree, i) = exec $ seval e
          env = Z3Env { index = i
                      , intSort = int, boolSort = bool , adtSort = adt
                      -- , constrFuns = cfs
                      , symVars = IntMap.empty
                      , funVars = IntMap.empty
                      , target = defaultTarget
                      }
      pathsZ3 env tree [] stop

defaultTarget :: Doc -> SymValue -> Z3 ()
defaultTarget s e = liftIO $ putDoc $ s <+> evalTo <+> pretty e <> linebreak

pathsZ3 :: Z3Env -> ExecutionTree -> [Doc] -> Int -> Z3 ()
pathsZ3 _ _ _ stop | stop <= 0 = return ()
pathsZ3 env (Exp e) conds _ =
    target env (foldr1 combineWithAnd conds') e
    where conds' = reverse $ case conds of
                               [] -> conds ++ [text "True"] -- sole result
                               _ -> conds

pathsZ3 env (NewSymVar i typ t) conds stop =
    do ast <- declareVar env i typ
       let env' = either
                  (\x -> env{symVars = IntMap.insert i x (symVars env)})
                  (\x -> env{funVars = IntMap.insert i x (funVars env)})
                  ast
       pathsZ3 env' t conds stop

pathsZ3 env (Fork e (Left (l,r))) conds stop =
    do ast <- assertProjs env e
       local $ assertCnstr ast >> whenSat (re l (pretty e : conds) (stop-1))
       local $ mkNot ast >>= assertCnstr >> whenSat (re r (prependNot (pretty e) : conds) (stop-1))
    where re = pathsZ3 env

pathsZ3 env (Fork e@(SConstr c vs) (Right ts)) conds stop =
    do let (cs, _, fs) = unzip3 ts
           f = fromJust $ lookup (sconstrName c) (map sconstrName cs `zip` fs)
       _ <- assertProjs env e
       pathsZ3 env (f $ map Exp vs) conds stop
pathsZ3 env (Fork e (Right ts)) conds stop =
    do ast <- assertProjs env e
       let (cs,_,_) = unzip3 ts
       assertConstrsDistinct env cs
       mapM_ (local . assertConstr ast) ts

       where assertConstr :: AST -> (SConstructor, [S.Name], [ExecutionTree] -> ExecutionTree) -> Z3 ()
             assertConstr ast (c,ns,f) =
                 do (cFd,params) <- mkConstrFun env c
                    let (paramSorts,paramFds) = unzip params
                        index' = index env + length params
                        ids = [index env..index'-1]
                    newVars <- zipWithM declareVarSort paramSorts ids
                    let varAsts = map snd newVars
                        env' =  env {index = index', symVars = IntMap.fromList newVars `IntMap.union` symVars env}
                    app <- mkApp cFd varAsts
                    astEq <- mkEq ast app
                    assertCnstr astEq
                    mapM_ (assertProj app) (zip paramFds varAsts)

                    -- whenSat $ pathsZ3 env' (f $ supply ns ids) (doc <+> text "&&" <+> pretty e <+> equals <+> hsep (map text $ sconstrName c : ns)) (stop-1)
                    let cond = pretty e <+> equals <+> hsep (map text $ sconstrName c : map (("x"++) . show) ids)
                    whenSat $ pathsZ3 env' (f $ supply (repeat "x") ids) (cond : conds) (stop-1)

symtype2sort :: Z3Env -> SymType -> Sort
symtype2sort env TInt = intSort env
symtype2sort env TBool = boolSort env
symtype2sort _ (TFun _ _) = error "symtype2sort: Function type"
symtype2sort env _ = adtSort env

declareVar :: Z3Env -> Int -> SymType -> Z3 (Either AST FuncDecl)
declareVar env i (TFun tArgs tRes) =
    fmap Right (declareSymFun i (map (symtype2sort env) tArgs) (symtype2sort env tRes))
declareVar env i typ = fmap (Left . snd) $ declareVarSort (symtype2sort env typ) i

declareVarSort :: Sort -> Int -> Z3 (Int, AST)
declareVarSort s n =
    do x <- mkIntSymbol n
       c <- mkConst x s
       return (n, c)

declareSymFun :: Int -> [Sort] -> Sort -> Z3 FuncDecl
declareSymFun i args res =
    do f <- mkIntSymbol i
       mkFuncDecl f args res

assertProj :: AST -> (FuncDecl, AST) -> Z3 ()
assertProj app (f, arg) =
    do ast <- mkApp f [app] >>= mkEq arg
       assertCnstr ast

assertProjs :: Z3Env -> SymValue -> Z3 AST
assertProjs env@Z3Env {symVars = vars, funVars = funs} = go
    where go (SConstr c vs) =
              do asts <- mapM go vs
                 (fd, params) <- mkConstrFun env c
                 ast <- mkApp fd asts
                 mapM_ (assertProj ast) (zip (map snd params) asts)
                 return ast
          go (SVar _ i _) = return $ vars IntMap.! i
          go (SInt i) = mkInt i
          go (SBool True) = mkTrue
          go (SBool False) = mkFalse
          go (SOp op v1 v2) =
              do x1 <- go v1
                 x2 <- go v2
                 case op of
                   ADD -> mkAdd [x1, x2]
                   SUB -> mkSub [x1, x2]
                   MUL -> mkMul [x1, x2]
                   OR -> mkOr [x1, x2]
                   AND -> mkAnd [x1, x2]
                   DIV -> mkDiv x1 x2
                   LT -> mkLt x1 x2
                   LE -> mkLe x1 x2
                   GT -> mkGt x1 x2
                   GE -> mkGe x1 x2
                   EQ -> mkEq x1 x2
                   NEQ -> do ast <- mkEq x1 x2
                             mkNot ast
          go (SApp v1 v2) = symFun v1 [v2]
          go (SFun{}) = error "symValueZ3 of SFun"

          symFun :: SymValue -> [SymValue] -> Z3 AST
          symFun (SApp v1 v2) vs = symFun v1 (v2:vs)
          symFun (SVar _ i _) vs =
              do args <- mapM go vs
                 let f = funs IntMap.! i
                 mkApp f args
          symFun _ _ = error "symFun"

whenSat :: Z3 () -> Z3 ()
whenSat m =
    do b <- fmap res2bool check
       when b m

res2bool :: Result -> Bool
res2bool Sat = True
res2bool Unsat = False
res2bool Undef = error "res2bool: Undef"

type ConstrMap = SConstructor -> ConstrFun
type ConstrFun = (FuncDecl, [(Sort, FuncDecl)])

-- prelude :: (Sort,Sort,Sort) -> [SymType] -> Z3 ConstrMap
-- prelude sorts ts =
--     do constrMap <- mkConstrMapM (constrFun sorts) ts
--        mapM_ (mkPrelude constrMap) ts
--        showContext >>= liftIO . putStrLn
--        return constrMap

forAll :: [Sort] -> ([AST] -> Z3 AST) -> Z3 AST
forAll [] f = f []
forAll sorts f = do
  syms <- mapM (\(s,n) -> mkIntSymbol n >>= \sym -> return (sym,s)) $ zip sorts [0..]
  bound <- mapM (\(s,n) -> mkBound n s) $ zip sorts [0..]
  res <- f bound
  uncurry (mkForall []) (unzip syms) res

assertConstrsDistinct :: Z3Env -> [SConstructor] -> Z3 ()
assertConstrsDistinct env cs =
    do constrFuns <- mapM (mkConstrFun env) cs
       let allSorts = concatMap constrFunSorts constrFuns
       ast <- forAll allSorts (\vars -> mkConstr vars (zip cs constrFuns) >>= mkDistinct)
       assertCnstr ast

    where mkConstr :: [AST] -> [(SConstructor, ConstrFun)] -> Z3 [AST]
          mkConstr [] [] = return []
          mkConstr vs ((c,cf):cs) =
              do x <- mkApp (fst cf) left
                 xs <- mkConstr right cs
                 return (x:xs)
              where (left,right) = splitAt (length (sconstrParams c)) vs
-- Assert that constuctors are distinct
-- mkPrelude :: ConstrMap ConstrFun -> SymType -> Z3 ()
-- mkPrelude cm t = do
--     do ast <- forAll allSorts (\vars -> mkConstr vars constrs >>= mkDistinct)
--        assertCnstr ast
--     where allSorts = concatMap (constrFunSorts . cm) constrs
--           constrs = dataConstrs t
--           mkConstr [] [] = return []
--           mkConstr vs (c:cs) =
--               do x <- mkApp (fst $ cm c) vs
--                  xs <- mkConstr right cs
--                  return (x:xs)
--               where (left,right) = splitAt (length (constrParams c)) vs

-- collect all constrs from symtypes, where f is constrFun
-- mkConstrMapM :: (SConstructor -> Z3 ConstrFun) -> [SymType] -> Z3 ConstrMap
-- mkConstrMapM f ts =
--     do mconstrs <- mapM (\c -> f c >>= \x -> return (constrId c, x)) (concatMap dataConstrs ts)
--        let mkConstrArr = Array.array (0, maximum (map constrId ts))
--        return $ (mkConstrArr mconstrs Array.!) . constrId

constrFunSorts :: ConstrFun -> [Sort]
constrFunSorts = fst . unzip . snd

mkConstrFun :: Z3Env -> SConstructor -> Z3 ConstrFun
mkConstrFun Z3Env {boolSort = bool, intSort = int, adtSort = adt} c =
    do s <- mkStringSymbol $ sconstrName c -- name
       let paramSorts = map dataSort (sconstrParams c)
       fd <- mkFuncDecl s paramSorts adt
       projectors <- mapM mkProjector (zip paramSorts [1..])
       return (fd, zip paramSorts projectors)

    where dataSort :: SymType -> Sort
          dataSort TInt = int
          dataSort TBool = bool
          dataSort _ = adt

          mkProjector :: (Sort, Int) -> Z3 FuncDecl
          mkProjector (s,i) =
              do sym <- mkStringSymbol $ sconstrName c ++ "_" ++ show i
                 mkFuncDecl sym [adt] s

-- declare a constructor as a function
-- constrFun :: (Sort,Sort,Sort) -> SConstructor -> Z3 ConstrFun
-- constrFun (int,bool,adt) c =
--     do s <- mkStringSymbol $ sconstrName c
--        let paramSorts = map dataSort (sconstrParams c)
--        fd <- mkFuncDecl s paramSorts adt
--        projectors <- mapM mkProjector (zip paramSorts [0..])
--        return (fd, zip paramSorts projectors)

--     where dataSort :: SymType -> Sort
--           dataSort TInt = int
--           dataSort TBool = bool
--           dataSort _ = adt

--           mkProjector :: (Sort, Int) -> Z3 FuncDecl
--           mkProjector (s,i) =
--               do sym <- mkStringSymbol $ "p" ++ show i ++ sconstrName c
--                  mkFuncDecl sym [adt] s
