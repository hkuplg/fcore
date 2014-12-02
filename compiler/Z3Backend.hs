module Z3Backend where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Z3.Monad hiding (Z3Env)
import Data.IntMap (IntMap, (!), empty, insert)
import SymbolicEvaluator
import Prelude hiding (EQ, GT, LT)
-- import DataTypes
import Core

data Z3Env = Z3Env { index :: Int
                   , boolSort, intSort :: Sort
                   -- , adtSort :: Sort
                   , symVars :: IntMap AST
                   , funVars :: IntMap FuncDecl
                   , target :: String -> SymValue -> Z3 ()
                   }

solve :: Expr () ExecutionTree -> IO ()
solve = solve' 6

solve' :: Int -> Expr () ExecutionTree -> IO ()
solve' stop e =
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
      pathsZ3 env tree "True" stop

defaultTarget :: String -> SymValue -> Z3 ()
defaultTarget s e = liftIO $ putStrLn $ s ++ " ==> " ++ show e

pathsZ3 :: Z3Env -> ExecutionTree -> String -> Int -> Z3 ()
pathsZ3 _ _ _ stop | stop <= 0 = return ()
pathsZ3 env (Exp e) s _ =
    case s of
      "True" -> target env "" e -- sole result
      _ -> target env (drop (length "True && ") s) e
pathsZ3 env (NewSymVar i typ t) s stop =
    do ast <- declareVar env i typ
       let env' = either
                  (\x -> env{symVars = insert i x (symVars env)})
                  (\x -> env{funVars = insert i x (funVars env)})
                  ast
       pathsZ3 env' t s stop
pathsZ3 env (Fork l e r) s stop =
    do ast <- assertProjs env e
       local (assertCnstr ast >> whenSat (re l (s ++ " && " ++ show e) (stop - 1)))
       local (mkNot ast >>= assertCnstr >> whenSat (re r (s ++ " && not " ++ show e) (stop - 1)))
    where re = pathsZ3 env

stype2sort :: Z3Env -> SymType -> Sort
stype2sort env TInt = intSort env
stype2sort env TBool = boolSort env
stype2sort _ (TFun _ _) = error "stype2sort: Function type"
-- stype2sort env _ = adtSort env

declareVar :: Z3Env -> Int -> SymType -> Z3 (Either AST FuncDecl)
declareVar env i (TFun tArgs tRes) =
    fmap Right (declareSymFun i (map (stype2sort env) tArgs) (stype2sort env tRes))
declareVar env i typ = fmap (Left . snd) $ declareVarSort (stype2sort env typ) i

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
assertProjs Z3Env { symVars = vars, funVars = funs} v = go v
    where go (SVar i _) = return $ vars ! i
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
          go (SFun _ _) = error "symValueZ3 of SFun"

          symFun :: SymValue -> [SymValue] -> Z3 AST
          symFun (SApp v1 v2) vs = symFun v1 (v2:vs)
          symFun (SVar i _) vs =
              do args <- mapM go vs
                 let f = funs ! i
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
