{-# LANGUAGE TemplateHaskell #-}

module RedBlackTree where

{-
1 try "runTests" to test all the properties
2 try "multiTestNv" to test the naive version of "prop_buggyInsNv" with time showed below
3 try "multiTestCl" to test the clever version of "prop_buggyInsCl" with time showed below
-}

import Test.QuickCheck
import Test.QuickCheck.All
import qualified Data.Set as Set
import MonadLib
import System.CPUTime
import System.Timeout



--"type for Color and Tree"
data Color = Red | Black deriving(Show,Eq)

data Tree = Empty | Node Color Tree Int Tree deriving(Show,Eq)


  ------------test data generator for Color--------------------------
instance Arbitrary Color where
  arbitrary = oneof [return Red, return Black]

  ------------test data generator for Tree---------------------------
instance Arbitrary Tree where
  arbitrary = sized arbTree
  shrink = shrinkTree

arbTree :: Int -> Gen Tree
arbTree 0 = return Empty
arbTree n = frequency
  [ (1, return Empty)
  , (4, liftM4 Node (oneof [return Red, return Black])
                    (arbTree (n `div` 2))
                    (choose (-65536,65536))
                    (arbTree (n `div` 2))) ]

shrinkTree :: Tree -> [Tree]
shrinkTree (Node c l v r) = [Node c l' v r | l' <- shrinkTree l] ++
                            [Node c l v' r | v' <- shrink v] ++
                            [Node c l v r' | r' <- shrinkTree r] ++
                            [Empty]
shrinkTree _              = []

---------------------------------------------------------------------

-------Generate black-balanced trees with no red-red violations:------

lg :: Int -> Int
lg n = round(logBase (fromIntegral(2)) (fromIntegral(n)))

rbTree :: Gen Tree
rbTree = sized (\n -> arbRBTree Black (lg(n))) where

 arbRBTree :: Color -> Int -> Gen Tree
 arbRBTree Black 0 = return Empty
 arbRBTree Black 1 =
   oneof [return Empty,
          liftM4 Node (return Red) (return Empty)
                      (choose (-65536,65536)) (return Empty)]

 arbRBTree Black n | n>0 =
   oneof [liftM4 Node (return Black) subtree arbnum subtree,
          liftM4 Node (return Red) subtree' arbnum subtree']
   where subtree  = arbRBTree Black (n-1)
         subtree' = arbRBTree Red n
         arbnum   = (choose (-65536,65536))

 arbRBTree Red 0 = return Empty
 arbRBTree Red 1 = return Empty
 arbRBTree Red n | n>0 =
   oneof [liftM4 Node (return Black) subtree arbnum subtree]
   where subtree = arbRBTree Black (n-1)
         arbnum  = (choose (-65536,65536))

---------------------------------------------------------------


--function "content" using Set which is interior in Data.Set
content :: Tree -> (Set.Set Int)
content Empty = Set.empty
content (Node _ l v r) = Set.unions
                         [(content l), (Set.singleton v),(content r)]

--function "size"
size :: Tree -> Int
size Empty = 0
size (Node _ l v r) = (size l) + 1 + (size r)


--function "isBlack"
isBlack :: Tree -> Bool
isBlack Empty = True
isBlack (Node Black _ _ _) = True
isBlack _ = False


--function "redNodesHaveBlackChildren"
redNodesHaveBlackChildren :: Tree -> Bool
redNodesHaveBlackChildren Empty = True
redNodesHaveBlackChildren (Node Black l _ r) = redNodesHaveBlackChildren l && redNodesHaveBlackChildren r
redNodesHaveBlackChildren (Node Red l _ r) = isBlack l &&
                                        isBlack r &&
                                        redNodesHaveBlackChildren l &&
                                        redNodesHaveBlackChildren r

isRBT :: Tree -> Bool
isRBT t = redNodesHaveBlackChildren t && blackBalanced t

--function "redDescHaveBlackChildren"
redDescHaveBlackChildren :: Tree -> Bool
redDescHaveBlackChildren Empty = True
redDescHaveBlackChildren (Node _ l _ r) = redNodesHaveBlackChildren l
                                       && redNodesHaveBlackChildren r

--function "blackBalanced"
blackBalanced :: Tree -> Bool
blackBalanced Empty = True
blackBalanced (Node _ l _ r) = blackBalanced l && blackBalanced r
                             && (blackHeight l) == (blackHeight r)

--function "blackHeight"
blackHeight :: Tree -> Int
blackHeight Empty = 1
blackHeight (Node Black l _ _) = (blackHeight l) + 1
blackHeight (Node Red l _ _) = (blackHeight l)


--function "ins" (insert an element into a tree)
ins :: Int -> Tree -> Tree
ins x t
   | (redNodesHaveBlackChildren t) && (blackBalanced t)
        = case t of
            Empty -> Node Red Empty x Empty
            (Node c a y b) ->   if x < y then balance c (ins x a) y b
                                else if x == y then Node c a y b
                                     else balance c a y (ins x b)
   | otherwise = error "The tree is not a Red-Black-Tree"



----test for "ins"
pprop_ins :: Int -> Tree -> Property
pprop_ins x t = (redNodesHaveBlackChildren t) && (blackBalanced t) ==>
     -- forAll rbTree $ \ t ->
         collect (size t) $
         content (ins x t) == Set.union (content t) (Set.singleton x)
    .&&. (size t) <= (size (ins x t))
    .&&. (size (ins x t)) <= (size t) + 1
    .&&. redDescHaveBlackChildren (ins x t)
    .&&. blackBalanced (ins x t)

prop_ins :: Int -> Tree -> Bool
prop_ins x t =
    --  forAll rbTree $ \ t ->
         content (ins x t) == Set.union (content t) (Set.singleton x)
    && (size t) <= (size (ins x t))
    && (size (ins x t)) <= (size t) + 1
    && redDescHaveBlackChildren (ins x t)
    && blackBalanced (ins x t)


prop_buggyInsNv :: Int -> Tree -> Property
prop_buggyInsNv x t = (redNodesHaveBlackChildren t) && (blackBalanced t) ==>
         content (ins x t) == Set.union (content t) (Set.singleton x)
    .&&. (size t) < (size (ins x t))
    .&&. (size (ins x t)) <= (size t) + 1
    .&&. redDescHaveBlackChildren (ins x t)
    .&&. blackBalanced (ins x t)


prop_buggyInsCl :: Int -> Property
prop_buggyInsCl x =
         forAll rbTree $ \ t ->
         content (ins x t) == Set.union (content t) (Set.singleton x)
    .&&. (size t) < (size (ins x t))
    .&&. (size (ins x t)) <= (size t) + 1
    .&&. redDescHaveBlackChildren (ins x t)
    .&&. blackBalanced (ins x t)



--function "makeBlack"
makeBlack :: Tree -> Tree
makeBlack t = case t of
                (Node Red l v r) -> Node Black l v r
                t -> t

----test for "makeBlack"
prop_makeBlack :: Property
prop_makeBlack = --(redDescHaveBlackChildren t)&&(blackBalanced t) ==>
         forAll rbTree $ \ t ->
         collect (size t) $
         redNodesHaveBlackChildren (makeBlack t) .&&. blackBalanced (makeBlack t)


--function "add"
add :: Int -> Tree -> Tree
add n t = makeBlack (ins n t)


----test for "add"
prop_add :: Int -> Property
prop_add n = -- (redNodesHaveBlackChildren t) && (blackBalanced t) ==>
         forAll rbTree $ \ t ->
         collect (size t) $
         content (add n t) == Set.union (content t) (Set.singleton n)
    .&&. redDescHaveBlackChildren (add n t)
    .&&. blackBalanced (add n t)


buggyAdd :: Int -> Tree -> Tree
buggyAdd n t = ins n t


----test for "buggyAdd"
prop_buggyAdd :: Int -> Property
prop_buggyAdd n = -- redNodesHaveBlackChildren t && (blackBalanced t) ==>
         forAll rbTree $ \ t ->
         collect (size t) $
         content (buggyAdd n t) == Set.union (content t) (Set.singleton n)
    .&&. redNodesHaveBlackChildren (buggyAdd n t)
    .&&. blackBalanced (buggyAdd n t)


--function "balance"
balance :: Color -> Tree -> Int -> Tree -> Tree
balance Black (Node Red (Node Red a xV b) yV c) zV d =
              Node Red (Node Black a xV b) yV (Node Black c zV d)

balance Black (Node Red a xV (Node Red b yV c)) zV d =
              Node Red (Node Black a xV b) yV (Node Black c zV d)

balance Black a xV (Node Red (Node Red b yV c) zV d) =
              Node Red (Node Black a xV b) yV (Node Black c zV d)

balance Black a xV (Node Red b yV (Node Red c zV d)) =
              Node Red (Node Black a xV b) yV (Node Black c zV d)

balance c a xV b = Node c a xV b

----test for "balance"
prop_balance :: Color -> Tree -> Int -> Tree -> Property
prop_balance c a x b =
            collect (size (Node c a x b)) $
                       content (balance c a x b) ==
                       content (Node c a x b)


--function "buggyBalance"
buggyBalance :: Color -> Tree -> Int -> Tree -> Tree
buggyBalance Black (Node Red (Node Red a xV b) yV c) zV d =
              Node Red (Node Black a xV b) yV (Node Black c zV d)
buggyBalance Black (Node Red a xV (Node Red b yV c)) zV d =
              Node Red (Node Black a xV b) yV (Node Black c zV d)
buggyBalance Black a xV (Node Red (Node Red b yV c) zV d) =
              Node Red (Node Black a xV b) yV (Node Black c zV d)
buggyBalance Black a xV (Node Red b yV (Node Red c zV d)) =
              Node Red (Node Black a xV b) yV (Node Black c zV d)

----test for "buggyBalance"
prop_buggyBalance :: Color -> Tree -> Int -> Tree -> Bool
prop_buggyBalance c a x b = content (buggyBalance c a x b) ==
                            content (Node c a x b)





----------------------------timer--------------------------------
timeIt :: (Fractional c) => (a -> IO b) -> a -> IO c
timeIt action arg = do
       startTime <- getCPUTime
       action arg
       finishTime <- getCPUTime
       return $ fromIntegral (finishTime - startTime)  / 1000000000

--timeIt' :: (Fractional c) => (a -> b) -> a -> IO c
--timeIt' f = timeIt (\x -> f x `seq` return())

oneTestNv = timeout 5000000 $ timeIt quickCheck prop_buggyInsNv >>= print
oneTestCl = timeout 5000000 $ timeIt quickCheck prop_buggyInsCl >>= print

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



------for-quickCheck-test------
return []                    --
runTests = $quickCheckAll    --
-------------------------------








