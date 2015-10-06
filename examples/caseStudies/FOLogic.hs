{-# LANGUAGE TemplateHaskell #-}

module FOLogic where

{-
To simplify, the following structrues are not contained in this First order logic.
1 The Term do not contain function, namely, we do not have "P(f(x,y))", things like that
2 Every predicate can only have one arguement, we do not have P(x,y)
3 The domain of constant is just [1..1000]
4 The domain of identity is just ["o".."z"]
5 The domain of predicate is just ["O".."Z"]
6 We assume that the context is given, that is, we already know the relation between predicates
  and constants
----all the constrain of the model is kind to quickCheck.

and
1 Try "runTests" to test all the properties
2 Try "multiTestCl" to test "prop_theCl", the Clever version, 10 times
3 Try "multiTestNv" to test "prop_theNv", the Naive version, 10 times

-}


import Test.QuickCheck
import Test.QuickCheck.All
import qualified Data.Set as Set
import MonadLib
import Data.Char as C
import System.CPUTime
import System.Timeout
import qualified Data.Foldable


--type definition

type Pre = String
type Ide = String
type Cos = Int

data Term = Var Ide | Con Cos deriving (Show,Eq)

data FOrmula = Prd Pre Term        --only one arguement predicates involved
               | And FOrmula FOrmula
               | Or FOrmula FOrmula
               | Implies FOrmula FOrmula
               | Not FOrmula
               | E Ide FOrmula
               | A Ide FOrmula deriving (Show,Eq)

data Assigment = Asg Ide Cos deriving (Show,Eq)

type Valuation = [Assigment]


-- context  (the properties that every constants hold)----------
data Convention = Prop Pre [Cos] deriving (Show,Eq)
type Context = [Convention]

-------------a construction of a random context-------------
--try "context" to see the content of it

predicate = map (:[]) ['O'..'Z']
constant  = [1..36]

change :: [Int] -> Int -> [Int]
change [] x = []
change (c:cs) x = if (c `mod` 12) == x then c:(change cs x) else change cs x

helper = map (change [1..36]) [0..12]

multiconstant :: Int -> [Int] -> [[Int]]
multiconstant 0 c = [c]
multiconstant i c = c:(multiconstant (i-1) c)

context = zipWith Prop predicate helper
--------------------------------------------------------------

--getCon for Term
getCon (Con c) = c


--getfunction for Valuation
getConsV :: Valuation -> [Cos]
getConsV []               = []
getConsV ((Asg i1 c1):vs) = c1:(getConsV vs)

getIde :: Valuation -> [Ide]
getIde []               = []
getIde ((Asg i1 c1):vs) = i1:(getIde vs)

--getfunction for Assigment
getIdeA (Asg i _) = i
getCosA (Asg _ c) = c

--getfuntion for Convention
getPreC :: Convention -> Pre
getPreC (Prop p _) = p

getCosC :: Convention -> [Cos]
getCosC (Prop _ cc) = cc

showCons :: String -> Context -> [Cos]
showCons p (c1:cs) = if p == getPreC c1 then getCosC c1
                                        else showCons p cs

-----Navie generator------------------
instance Arbitrary Assigment where
 arbitrary = liftM2 Asg (elements (map (:[]) ['o'..'z'])) (elements [1..36])



----------clever generator------------
uniqueVal :: Valuation -> Valuation
uniqueVal va@(v@(Asg i c):vs) = if elem i (getIde vs) then uniqueVal vs
                                                      else v:(uniqueVal vs)
uniqueVal _ = []

clGen :: Gen Valuation
clGen = fmap uniqueVal xyzArb
        where xyzArb = liftM2 (:) (liftM2 Asg (return "x") (elements [1..36]))
                      (liftM2 (:) (liftM2 Asg (return "y") (elements [1..36]))
                      (liftM2 (:) (liftM2 Asg (return "z") (elements [1..36])) arbitrary))


---------------------------------------

--the statement & property (whether the statement is true under all valuations) & counterexample
the = Implies (A "x" (Or (Prd "P" (Var "x")) (Prd "Q" (Var "x")))) (Or (A "y" (Prd "P" (Var "y"))) (A "z" (Prd "Q" (Var "z"))))

prop_theNv :: Valuation -> Property
prop_theNv v = ((vars the) `Set.isSubsetOf` (Set.fromList (map getIdeA (uniqueVal v)))) ==>
                                  satisfy (pf the) (uniqueVal v)

prop_theCl :: Property
prop_theCl = forAll clGen $ \v -> satisfy (pf the) (uniqueVal v)

counterExample = [(Asg "x" 1), (Asg "y" 1), (Asg "z" 2), (Asg "r" 2)]

----------------------------------------


---------------------------prenex normal form----------------------------------------

--function "simplify"  (erase the implies and eat the not)
simplify :: FOrmula -> FOrmula
simplify (Prd p v)           = Prd p v
simplify (And l r)           = And (simplify l) (simplify r)
simplify (Or l r)            = Or (simplify l) (simplify r)
simplify (Implies l r)       = Or (Not (simplify l)) (simplify r)
simplify (Not (E i f))       = A i (simplify (Not f))
simplify (Not (A i f))       = E i (simplify (Not f))
simplify (Not (And l r))     = Or (simplify (Not l)) (simplify (Not r))
simplify (Not (Or l r))      = And (simplify (Not l)) (simplify (Not r))
simplify (Not (Implies l r)) = And (simplify l) (simplify (Not r))
simplify (Not f)             = Not (simplify f)
simplify (E i f)             = E i (simplify f)
simplify (A i f)             = A i (simplify f)


--function "isSimplified"
isSimplified :: FOrmula -> Bool
isSimplified (Prd p v)       = True
isSimplified (And l r)       = isSimplified l && isSimplified r
isSimplified (Or l r)        = isSimplified l && isSimplified r
isSimplified (Implies l r)   = False
isSimplified (Not (Prd _ _)) = True
isSimplified (Not _)         = False
isSimplified (E i f)         = isSimplified f
isSimplified (A i f)         = isSimplified f



--function "opf" (prenex form for one step) (outmost)
opf :: FOrmula -> FOrmula
opf (Prd p v)       = Prd p v
opf (And (A i f) r) = A i (And f (opf r))
opf (And l (A i f)) = A i (And (opf l) f)
opf (And (E i f) r) = E i (And f (opf r))
opf (And l (E i f)) = E i (And (opf l) f)
opf (And l r)       = And (opf l) (opf r)
opf (Or (A i f) r)  = A i (Or f (opf r))
opf (Or l (A i f))  = A i (Or (opf l) f)
opf (Or (E i f) r)  = E i (Or f (opf r))
opf (Or l (E i f))  = E i (Or (opf l) f)
opf (Or l r)        = Or (opf l) (opf r)
opf (Not (Prd p v)) = Not (Prd p v)
opf (Not f)         = opf (simplify (Not f))
opf (Implies l r)   = opf (simplify (Implies l r))
opf (A i f)         = A i (opf f)
opf (E i f)         = E i (opf f)


--function "lgh"    somehow like the lenght of a formula
lgh :: FOrmula -> Int
lgh (Prd p v) = 1
lgh (And l r) = (lgh l) + (lgh r)
lgh (Or l r) = (lgh l) + (lgh r)
lgh (Implies l r) = (lgh l) + (lgh r)
lgh (Not f) = lgh f
lgh (E i f) = lgh f
lgh (A i f) = lgh f


--function "pf"
pf :: FOrmula -> FOrmula
pf f = pfhelp f (lgh f)

pfhelp :: FOrmula -> Int -> FOrmula
pfhelp f 0 = f
pfhelp f n = pfhelp (opf f) (n-1)
---------------------------------------------------------------------------



------------------------------satisfy---------------------------------------
--function "vars"
vars :: FOrmula -> Set.Set Ide
vars (E i f)        = Set.singleton i `Set.union` vars f
vars (A i f)        = Set.singleton i `Set.union` vars f
vars (Prd p t) =
        case t of Var v -> Set.singleton v
                  Con c -> Set.empty
vars (Not f)        = vars f
vars (And l r)      = vars l `Set.union` vars r
vars (Or l r)       = vars l `Set.union` vars r
vars (Implies l r)  = vars l `Set.union` vars r

--function "prdof" get the predicate used in a FOrmula
prdof :: FOrmula -> Set.Set Pre
prdof (Prd p _)     = Set.singleton p
prdof (E i f)       = prdof f
prdof (A i f)       = prdof f
prdof (Not f)       = prdof f
prdof (And l r)     = prdof l `Set.union` prdof r
prdof (Or l r)      = prdof l `Set.union` prdof r
prdof (Implies l r) = prdof l `Set.union` prdof r



--function "verify" verify a FOrmula without variables, with context
verify :: FOrmula -> Bool
verify (Prd p c)     = if (elem p predicate) then elem (getCon c) (showCons p context)
                                             else False
verify (Not f)       = not (verify f)
verify (Or l r)      = verify l || verify r
verify (And l r)     = verify l && verify r
verify (Implies l r) = (not (verify l)) || verify r






--function "iequivalent" give the list of i-equivalent valuation of a valuation
iequivalent :: Valuation -> Ide -> [Valuation]
iequivalent v@((Asg i1 c1):vs) i = map (:(deleteIE v i)) (map (Asg i) (getConsV v))

--deleteIE
deleteIE :: Valuation -> Ide -> Valuation
deleteIE (v@(Asg i1 c1):vs) i = if i == i1 then vs
                                           else v:(deleteIE vs i)


--function "satisfy"
satisfy :: FOrmula -> Valuation -> Bool
satisfy f [] = if Set.null (vars f)
               then verify f
               else error "the valuation is not exhuasted."
satisfy f va = if (vars f) `Set.isSubsetOf` (Set.fromList (map getIdeA va))
               then case f of
                Prd p fo    -> verify $ Prd p (subst fo va)
                And l r     -> satisfy l va && satisfy r va
                Or l r      -> satisfy l va || satisfy r va
                Implies l r -> (not (satisfy l va)) || satisfy r va
                Not f       -> not (satisfy f va)
                A i fo -> foldl (&&) True (map (satisfy fo) (iequivalent va i))
                E i fo -> foldl (||) False (map (satisfy fo) (iequivalent va i))
               else error "the valuation is not exhuasted."

--function "subst" valua the constants into variables
subst :: Term -> Valuation -> Term
subst t va@(v:vs) =  case t of
                           Var t1 -> if t1 == (getIdeA v) then Con (getCosA v) else subst t vs
                           Con t1 -> Con t1


------------------------------------------------------------------------------------------


-----------------------------------timer--------------------------------------------------

timeIt :: (Fractional c) => (a -> IO b) -> a -> IO c
timeIt action arg = do
       startTime <- getCPUTime
       action arg
       finishTime <- getCPUTime
       return $ fromIntegral (finishTime - startTime)  / 1000000000

--timeIt' :: (Fractional c) => (a -> b) -> a -> IO c
--timeIt' f = timeIt (\x -> f x `seq` return())

oneTestNv = timeout 5000000 $ timeIt quickCheck prop_theNv >>= print --change prop here
oneTestCl = timeout 5000000 $ timeIt quickCheck prop_theCl >>= print --change prop here

multiTestNv = do
            startTime <- getCPUTime
            oneTestNv
            oneTestNv
            oneTestNv
            oneTestNv
            oneTestNv
            oneTestNv
            oneTestNv
            oneTestNv
            oneTestNv
            oneTestNv
            finishTime <- getCPUTime
            return $ fromIntegral (finishTime - startTime) / 1000000000

multiTestCl = do
            startTime <- getCPUTime
            oneTestCl
            oneTestCl
            oneTestCl
            oneTestCl
            oneTestCl
            oneTestCl
            oneTestCl
            oneTestCl
            oneTestCl
            oneTestCl
            finishTime <- getCPUTime
            return $ fromIntegral (finishTime - startTime) / 1000000000


-----------------------------------------------------------------------------------------


------for-quickCheck-test-------
return []                     --
runTests = $quickCheckAll     --
--------------------------------





