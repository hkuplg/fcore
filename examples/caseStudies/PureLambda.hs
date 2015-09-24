{-# LANGUAGE TemplateHaskell #-}

module PureLambda
       (Ide,
        LambdaCalculus,
        TermA(..),
        TermL(..),
        freeVars,
        fullForm,
        lgh,
        limitedReduce,
        simpleForm,
        allWords) where

import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.QuickCheck.All
import Test.QuickCheck
import Control.Monad
import System.CPUTime
import System.Timeout


type Ide            = String
data TermL          = Var Ide | App TermL TermL | Abs Ide TermL deriving Eq -- lambda term
data TermA          = Asg Ide TermL deriving Eq                             -- assignment term
type LambdaCalculus = Either TermL TermA


instance Arbitrary TermL where
 arbitrary = sized arbTermL

arbTermL 0 = liftM Var (elements initc)
arbTermL n = frequency [ 
             (1, (liftM Var (elements initc))),
             (3, (liftM2 App (arbTermL (n `div` 2)) 
                             (arbTermL (n `div` 2)))),
             (3, (liftM2 Abs (elements initc)
                             (arbTermL (n `div` 2)))) ]


initc :: [String]
initc = map (: []) ['a'..'z']

----------

absOpr  = "."
absHead = "\\"
appOpr  = " "

fullForm :: TermL -> String
fullForm (Var x)         = x
fullForm (Abs x y)       = "(" ++ absHead ++ x ++ absOpr ++ fullForm y ++ ")" 
fullForm (App x y)       = "(" ++ fullForm x ++ appOpr ++ fullForm y ++ ")"

simpleForm :: TermL -> String
simpleForm (Var x)                = x
simpleForm (Abs x (Var y))        = absHead ++ x ++ absOpr ++ y
simpleForm (Abs x y)              = absHead ++ x ++ absOpr ++ simpleForm y
simpleForm (App x y)              = helper1 x ++ appOpr ++ helper2 y False
  where helper1 (Var a)           = a
        helper1 (App x y)         = helper1 x ++ appOpr ++ helper2 y True
        helper1 a                 = "(" ++ simpleForm a ++ ")"
        helper2 (Var a) _         = a
        helper2 a@(Abs _ _) False = simpleForm a
        helper2 a@(Abs _ _) True  = "(" ++ simpleForm a ++ ")"
        helper2 a _               = "(" ++ simpleForm a ++ ")"

instance Show TermL where
  show = simpleForm

instance Show TermA where
  show (Asg id t) = id ++ " = " ++ show t

-- Theory part

freeVars :: TermL -> Set Ide
freeVars (Var x)     = Set.singleton x
freeVars (App t1 t2) = freeVars t1 `Set.union` freeVars t2
freeVars (Abs x t)   = Set.delete x $ freeVars t

isClosed :: TermL -> Bool
isClosed = Set.null . freeVars


-----property: freeVars

subst :: TermL -> Ide -> TermL -> TermL      -- substitution first term substitute into Ide
subst n x m@(Var y)                         -- if y = x, then just subst n to y, otherwise m
  | x == y             = n
  | otherwise          = m
subst n x (App p q)    = App (subst n x p) (subst n x q)   -- App, subst each respectively
subst n x m@(Abs y p)                                      -- Abs,  
  | x == y             = m                                 -- if it is bounded, no subst
  | x `Set.notMember` freeP  = m                           -- bounded in p, still no subst
  | y `Set.notMember` freeN  = Abs y (subst n x p)         -- then fv in n is still free
  | otherwise          = Abs z $ subst n x $ subst (Var z) y p --otherwise change y to z
  where freeP          = freeVars p
        freeN          = freeVars n
        freeNP         = freeP `Set.union` freeN
        z              = genNewIde freeNP

genNewIde :: Set Ide -> Ide                                --- get a new z
genNewIde ids = head $ filter (`Set.notMember` ids) allWords

allWords :: [String]
allWords            = concat $ iterate addPrefix initVars
  where addPrefix s = [a:b | a <- alphabet, b<-s]
        initVars    = map (: []) alphabet
        alphabet    = ['a'..'z']


----property : x would not appear in fv of m if it is subst "good and easy property"
prop_subst :: TermL -> Ide -> TermL -> Property
prop_subst n x m = x `Set.notMember` (freeVars n) ==> x `Set.notMember` (freeVars (subst n x m))



alphaCongruent :: TermL -> TermL -> Bool
alphaCongruent (Var x) (Var y)          = x == y
alphaCongruent (App x1 y1) (App x2 y2)  = alphaCongruent x1 x2 && alphaCongruent y1 y2
alphaCongruent (Abs x tx) (Abs y ty)
  | x == y                              = alphaCongruent tx ty
  | otherwise                           = alphaCongruent (subst (Var z) x tx) (subst (Var z) y ty)
  where z                               = genNewIde $ freeVars tx `Set.union` freeVars ty
alphaCongruent _ _                      = False

----property : alphaCon is reflexive, transitive, symtaxxx
prop_alphaCongruent_ref :: TermL -> Bool        --easy
prop_alphaCongruent_ref t = alphaCongruent t t

prop_alphaCongruent_traNv :: TermL -> TermL -> TermL -> Property 
prop_alphaCongruent_traNv t1 t2 t3 = 
    (alphaCongruent t1 t2) && (alphaCongruent t2 t3) ==> alphaCongruent t1 t3

prop_alphaCongruent_sym :: TermL -> TermL -> Property
prop_alphaCongruent_sym t1 t2 = alphaCongruent t1 t2 ==> alphaCongruent t2 t1


----property : alpha subst
prop_alphasubst :: TermL -> TermL -> TermL -> TermL -> Ide -> Property
prop_alphasubst m1 m2 n1 n2 x = (alphaCongruent m1 m2) && (alphaCongruent n1 n2) ==> 
                                 alphaCongruent (subst n1 x m1) (subst n2 x m2)


-- Leftmost Outmost Reduce

loReduce :: TermL -> Maybe TermL        --just one step
loReduce (Var _) = Nothing
loReduce (App (Abs x t1) t2) = Just $ subst t2 x t1   --beta reduction
loReduce (App t1 t2) =
  case loReduce t1 of                   
    Just t1' -> Just $ App t1' t2        -- leftmost
    Nothing ->
      case loReduce t2 of
        Just t2' -> Just $ App t1 t2'    -- just as it has parens 
        Nothing -> Nothing
loReduce (Abs x t) =
  case loReduce t of
    Just t' -> Just $ Abs x t'
    Nothing -> Nothing

lgh :: TermL -> Int                      -- number of Var
lgh (Var _)     = 1
lgh (App t1 t2) = lgh t1 + lgh t2
lgh (Abs _ t)   = 1 + lgh t


limitedReduce :: TermL -> [TermL]
limitedReduce x
  | length trace < steps = map fromJust trace
  | otherwise            = []
  where steps            = (lgh x)*2
        trace            = take steps . takeWhile (/=Nothing) . iterate (>>= loReduce) $ Just x
--take steps is a partial function


----property : P to be reduced to M and N, then exists T, such that can be reduced by both M, N
prop_limitedReduce :: TermL -> Bool
prop_limitedReduce p = allAlphaCon $ limitedReduce p


allAlphaCon :: [TermL] -> Bool
allAlphaCon [] = True
allAlphaCon [t] = True
allAlphaCon (x:xs@(y:ys)) = (alphaCongruent xl yl) && allAlphaCon xs 
                              where xl = head $ reverse (limitedReduce x)
                                    yl = head $ reverse (limitedReduce y)

exhaustedIterate :: Eq a => (a -> a) -> a -> a  --till it deplicate it exhausted 
exhaustedIterate f x   = helper trace
  where trace          = iterate f x
        helper (x:xs@(y:_))
          | x == y     = x
          | otherwise  = helper xs



----------------------------timer--------------------------------
timeIt :: (Fractional c) => (a -> IO b) -> a -> IO c
timeIt action arg = do
       startTime <- getCPUTime
       action arg
       finishTime <- getCPUTime
       return $ fromIntegral (finishTime - startTime)  / 1000000000

--timeIt' :: (Fractional c) => (a -> b) -> a -> IO c
--timeIt' f = timeIt (\x -> f x `seq` return())

oneTestNv = timeout 5000000 $ timeIt quickCheck prop_alphaCongruent_traNv >>= print 
--oneTestCl = timeout 5000000 $ timeIt quickCheck prop_alphaCongruent_tra >>= print 

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
{-
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
-}




------for-quickCheck-test------
return []                    -- 
runTests = $quickCheckAll    --
-------------------------------
