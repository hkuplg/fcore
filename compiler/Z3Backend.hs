module Z3Backend where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Z3.Monad hiding (Z3Env, local)
import Data.IntMap (IntMap, (!), empty, insert)
import SymbolicEvaluator
import Prelude hiding (EQ, GT, LT)
import DataTypes
import Core

data Z3Env = Z3Env { index :: Int
                   , boolSort, intSort :: Sort
                   -- , adtSort :: Sort
                   , symVars :: IntMap AST
                   , funVars :: IntMap FuncDecl
                   , target :: String -> SymValue -> Z3 ()
                   }

defaultTarget :: String -> SymValue -> Z3 ()
defaultTarget s e = liftIO $ putStrLn $ s ++ " ==> " ++ show e

solve :: Expr () ExecutionTree -> Int -> IO ()
solve e n =
    -- evalZ3 :: Z3 a -> IO a
    evalZ3 $ do
      int <- mkIntSort
      bool <- mkBoolSort
      -- adtSym <- mkStringSymbol "adtSort"
      -- adt <- mkUninterpretedSort adtSym

      let (tree, i) = exec $ seval e
          env = Z3Env { index = i + 1
                      , intSort = int, boolSort = bool
                      -- , adtSort = adt
                      , symVars = empty
                      , funVars = empty
                      , target = defaultTarget
                      }
      pathsZ3 env tree "True" n

-- preludeZ3 :: (Sort, Sort, Sort) -> Z3 (ConMap ConFun)

-- [l r]
pathsZ3 :: Z3Env -> ExecutionTree -> String -> Int -> Z3 ()
pathsZ3 _ _ _ stop | stop <= 0 = return ()
pathsZ3 env (NewSymVar n t) s stop =
    do ast <- declareVar env n
       let env' = either (\x -> env{symVars = insert n x (symVars env)})
                  (\x -> env{funVars = insert n x (funVars env)})
                  ast
       pathsZ3 env' t s stop
pathsZ3 env (Exp e) s stop = target env s e
pathsZ3 env (Fork l e r) s stop =
    do ast <- assertProjs env e
-- pathsZ3 env (NewSymVar n e) s stop =
    -- do ast <- declareVar env
-- pathsZ3 env (Fork l e r) s stop =


dataToSort :: Z3Env -> DataType -> Sort
dataToSort env DataInt = intSort env
dataToSort env DataBool = boolSort env
dataToSort env DataFun{} = error "dataToSort: Function type"
dataToSort env _ = adtSort env

-- declareVar :: Z3Env -> Int -> Z3 (Either AST FuncDecl)
-- declareVar env (n, DataFun{dataParams = ps, dataResult = r}) =
--     fmap Right (declareSymFun n (map (dataToSort env) ps) (dataToSort env r))
-- declareVar env (n, t) = fmap (Left . snd) $ declareVarSort (dataToSort env t) n

declareVar :: Z3Env -> Int -> Z3 (Either AST FuncDecl)
declareVar env n =
    fmap Right (declareSymFun n (map (dataToSort env) ps) (dataToSort env r))
declareVar env (n, t) = fmap (Left . snd) $ declareVarSort (dataToSort env t) n

declareVarSort :: Sort -> Int -> Z3 (Int, AST)
declareVarSort s n = do
  x <- mkIntSymbol n
  c <- mkConst x s
  return (n, c)

declareSymFun :: Int -> [Sort] -> Sort -> Z3 FuncDecl
declareSymFun n ps r =
    do f <- mkIntSymbol n
       mkFuncDecl f ps r

assertProj :: AST -> (FuncDecl, AST) -> Z3 ()
assertProj app (fd, arg) =
    do ast <- mkApp fd [app] >>= mkEq arg
       assertCnstr ast

assertProjs :: Z3Env -> SymValue -> Z3 AST
assertProjs env@Z3Env { symVars = vars, funVars = funs} v = go v
    where go (SVar i) = return $ vars ! i
          go (SInt i) = mkInt i
          -- go (SBool b) = mkBool b
          go (SOp op v1 v2) =
              do x1 <- go v1
                 x2 <- go v2
                 case op of
                   ADD -> mkAdd [x1, x2]
                   MUL -> mkMul [x1, x2]
                   LT -> mkLt x1 x2
                   LE -> mkLe x1 x2
                   GT -> mkGt x1 x2
                   GE -> mkGe x1 x2
                   EQ -> mkEq x1 x2
                   OR -> mkOr [x1, x2]
                   AND -> mkAnd [x1, x2]
                   -- NEQ -> mkNot (mkEq x1 x2)
          go (SApp v1 v2) = symFun v1 [v2]



          symFun :: SymValue -> [SymValue] -> Z3 AST
          symFun (SApp v1 v2) vs = symFun v1 (v2:vs)
          symFun (SVar i) vs =
              do args <- mapM go vs
                 let f = funs ! i
                 mkApp f args
          symFun _ _ = error "symFun"

local :: Z3 a -> Z3 a
local m = do
  push
  a <- m
  pop 1
  return a


whenSat :: Z3 () -> Z3 ()
whenSat m =
    do b <- fmap res2bool check
       when b m


res2bool :: Result -> Bool
res2bool Sat = True
res2bool Unsat = False
res2bool Undef = error "res2bool: Undef"
