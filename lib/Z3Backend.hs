{- |
Module      :  Z3Backend
Description :  The Z3 backend for symbolic
Copyright   :  (c) 2014—2015 The F2J Project Developers (given in AUTHORS.txt)
License     :  BSD3

Maintainer  :  Weixin Zhang <zhangweixinxd@gmail.com>
Stability   :  unstable
Portability :  portable

Symbolic evaluator will generates all possible execution paths for a given program. We use Z3 to kick out those infeasible execution paths.
-}

module Z3Backend where

import           Control.Monad
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.IntMap                  as IntMap
import           Data.Map                     (Map, fromList)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust)
import           Prelude                      hiding (EQ, GT, LT)
import           PrettyUtils
import qualified Src                          as S
import           SymbolicEvaluator
import qualified SystemFI                     as FI
import           Text.PrettyPrint.ANSI.Leijen
import           Z3.Monad                     hiding (Z3Env)
import           Z3ModelParser

data Z3Env = Z3Env { index             :: Int
                   , boolSort, intSort :: Sort
                   , constrDecls       :: Map.Map String FuncDecl
                   , adtSorts          :: Map.Map String Sort
                   , symVars           :: IntMap.IntMap AST
                   , funVars           :: IntMap.IntMap FuncDecl
                   , target            :: Doc -> SymValue -> Z3 ()
                   }

explore, counter :: Int -> FI.Expr () ExecutionTree -> IO ()
explore = traverse defaultTarget
counter = traverse counterTarget

-- Collcet and declare all datatype definitions in the expression
declareAllDatatypes :: Z3Env -> FI.Expr () ExecutionTree -> [Z3 (String, Sort)]
declareAllDatatypes env = go
    where tree = Exp $ SBool True
          trees = repeat tree
      --  go (Data Rec databinds e1) = concatMap databinds ++ go e1
          go (FI.Data S.NonRec [databind] e1) = declareDatatype env 0 databind : go e1
          go (FI.If e1 e2 e3) = go e1 ++ go e2 ++ go e3
          go (FI.PrimOp e1 _ e2) = go e1 ++ go e2
          go (FI.Let _ e f) = go e ++ go (f tree)
          go (FI.TApp e1 _) = go e1
          go (FI.App e1 e2) = go e1 ++ go e2
          go (FI.Constr _ es) = concatMap go es
          go (FI.Case e1 alts) = go e1 ++ concatMap (\(FI.ConstrAlt _ _ f) -> go (f trees)) alts
          go (FI.Lam _ _ e) = go (e tree)
          go (FI.BLam _ e) = go (e ())
          go (FI.Fix _ _ f _ _) = go (f tree tree)
          go (FI.LetRec _ _ binds body) = concatMap go (binds trees) ++ go (body trees)
          go _ = []

-- Declare a datatype, e.g. List: mkDatatype "List" [Nil, Cons]
declareDatatype :: Z3Env -> Int -> FI.DataBind () -> Z3 (String, Sort)
declareDatatype env recFlag (FI.DataBind name _ cs) =
    do sym <- mkStringSymbol name
       constrs <- mapM (declareConstructor env recFlag . transConstructor) (cs $ repeat ())
       sort <- mkDatatype sym constrs
       -- mapM_ delConstructor constrs
       return (name, sort)

-- Declare a constructor, e.g. Cons: mkConstructor "Cons" "isCons" [("Cons_1", Just intSort, 0), ("Cons_2", Nothing, 0)]
declareConstructor :: Z3Env -> Int -> SConstructor -> Z3 Constructor
declareConstructor env recFlag (SConstructor name types datatype) =
    do constr_sym <- mkStringSymbol name
       recognizer_sym <- mkStringSymbol ("is" ++ name)
       param_syms <- mapM (\i -> mkStringSymbol $ name ++ "_" ++ show i) [1..length types]
       let param_triples = zipWith (\sym t -> if t == datatype
                                              then (sym, Nothing, recFlag)
                                              else (sym, Just $ type2sort env t, recFlag))
                           param_syms
                           types
       mkConstructor constr_sym recognizer_sym param_triples

-- Given a datatype sort, returns a list of constructor name and its funcdecl pair
getAdtConstrNameDeclPairs :: Sort -> Z3 [(String, FuncDecl)]
getAdtConstrNameDeclPairs adtSort =
    do decls <- getDatatypeSortConstructors adtSort
       mapM pairWithName decls

    where pairWithName decl =
              do name <- getSymbolString =<< getDeclName decl
                 return (name, decl)

traverse :: (Int -> Doc -> SymValue -> Z3 ())
         -> Int
         -> FI.Expr () ExecutionTree
         -> IO ()
traverse target stop e =
    evalZ3 $ do
      int <- mkIntSort
      bool <- mkBoolSort
      -- tvar <- mkStringSymbol "T" >>= mkUninterpretedSort -- for type parameters

      let (tree, i) = exec $ seval e -- i is the # of symbolic values
      let env = Z3Env { index = i
                      , intSort = int, boolSort = bool--, tvarSort = tvar
                      , adtSorts = Map.empty
                      , constrDecls = Map.empty
                      , symVars = IntMap.empty
                      , funVars = IntMap.empty
                      , target = target i
                      }
      name_sort_pairs <- sequence (declareAllDatatypes env e)
      constr_decls <- foldM (\acc sort ->
                                     do pairs <- getAdtConstrNameDeclPairs sort
                                        return $ Map.fromList pairs `Map.union` acc)
                       Map.empty
                      (map snd name_sort_pairs)
      let env' = env { adtSorts = Map.fromList name_sort_pairs, constrDecls = constr_decls }
      pathsZ3 env' tree [] stop

defaultTarget :: Int -> Doc -> SymValue -> Z3 ()
defaultTarget _ cond e = liftIO $ putDoc $ cond <+> evalTo <+> pretty e <> linebreak

counterTarget :: Int -> Doc -> SymValue -> Z3 ()
counterTarget i cond (SBool False) =
    do liftIO $ putDoc $ cond <+> evalTo <+> text "False" <> linebreak
       withModel ((>>= (liftIO . putStrLn . counterExample i)) . showModel)
       return ()
counterTarget _ _ _ = return ()

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
       local $ assert ast >> whenSat (re l (pretty e : conds) (stop-1))
       local $ mkNot ast >>= assert >> whenSat (re r (prependNot (pretty e) : conds) (stop-1))
    where re = pathsZ3 env

pathsZ3 env (Fork e@(SConstr c vs) (Right ts)) conds stop =
    do let (cs, _, fs) = unzip3 ts
           f = fromJust $ lookup (sconstrName c) (map sconstrName cs `zip` fs)
       _ <- assertProjs env e
       pathsZ3 env (f $ map Exp vs) conds stop
pathsZ3 env (Fork e (Right ts)) conds stop =
    do ast <- assertProjs env e
       mapM_ (local . assertConstructor ast) ts

    where assertConstructor :: AST -> (SConstructor, [S.Name], [ExecutionTree] -> ExecutionTree) -> Z3 ()
          assertConstructor node (SConstructor name types _, vars, f) =
              let constr_decl = constrDecls env Map.! name
                  var_sorts = map (type2sort env) types
                  next_index = index env + length vars
                  var_ids = [index env .. next_index - 1]
              in do var_id_ast_pairs <- zipWithM declareVarSort var_sorts var_ids
                    let var_asts = map snd var_id_ast_pairs
                        env' = env { index = next_index, symVars = IntMap.fromList var_id_ast_pairs `IntMap.union` symVars env  }
                        cond = pretty e <+> equals <+> hsep (map text $ name : map (("x"++) . show) var_ids)

                    assert =<< mkEq node =<< mkApp constr_decl var_asts
                    whenSat $ pathsZ3 env' (f $ supply (repeat "x") var_ids) (cond : conds) (stop-1)

type2sort :: Z3Env -> SymType -> Sort
type2sort env TBool = boolSort env
type2sort _ (TFun _ _) = error "type2sort: Function type"
type2sort env (TData name) = fromJust $ Map.lookup name $ adtSorts env
-- type2sort env _ = tvarSort env
type2sort env _ = intSort env
-- type2sort env TInt = intSort env

declareVar :: Z3Env -> Int -> SymType -> Z3 (Either AST FuncDecl)
declareVar env i (TFun tArgs tRes) =
    fmap Right (declareSymFun i (map (type2sort env) tArgs) (type2sort env tRes))
declareVar env i typ = fmap (Left . snd) $ declareVarSort (type2sort env typ) i

declareVarSort :: Sort -> Int -> Z3 (Int, AST)
declareVarSort s n =
    do x <- mkIntSymbol n
       c <- mkConst x s
       return (n, c)

declareSymFun :: Int -> [Sort] -> Sort -> Z3 FuncDecl
declareSymFun i args res =
    do f <- mkIntSymbol i
       mkFuncDecl f args res

-- assertProj :: AST -> (FuncDecl, AST) -> Z3 ()
-- assertProj app (f, arg) =
--     do ast <- mkApp f [app] >>= mkEq arg
--        assert ast

assertProjs :: Z3Env -> SymValue -> Z3 AST
assertProjs Z3Env {intSort = int, symVars = vars, funVars = funs, constrDecls = constrs} = go
    where go (SConstr c vs) =
              do asts <- mapM go vs
                 let decl = constrs Map.! sconstrName c
                 mkApp decl asts
          go (SVar _ i _) = return $ vars IntMap.! i
          go (SInt i) = mkInt (fromInteger i) int
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
