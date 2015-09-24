{-# LANGUAGE TemplateHaskell #-}

module HeapSort where


{-
1. Try "runTests" in the terminal to test all the properties
2. Try "multiTestNv" to test the naive version of "prop_buggyRemoveMaxNv" 10 times with time   
   showed below.
3. Try "multiTestCl" to test the clever version of "prop_buggyRemoveMaxCl" 10 times with time   
   showed below.
-}

import Test.QuickCheck
import Test.QuickCheck.All
import qualified Data.Set as Set
import MonadLib
import System.CPUTime
import System.Timeout

--type Heap
data Heap = Leaf | Node { rk :: Int,
                          value :: Int,
                          left :: Heap,
                          right :: Heap
                        } deriving (Show, Eq)

-----------------test data generator and shrink for Heap-------------------------
instance Arbitrary Heap where
  arbitrary = sized arbHeap
  shrink = shrinkHeap 

arbHeap 0 = return Leaf
arbHeap n =frequency
         [ (1, return Leaf)
         , (4, liftM4 Node (choose (0,65536)) 
                           (choose (-65536, 65536))
                           (arbHeap (n `div` 2))
                           (arbHeap (n `div` 2))) ]

shrinkHeap :: Heap -> [Heap]
shrinkHeap (Node rk v l r) = [Leaf] ++
                             [(Node rk' v l r) | rk' <- shrink rk] ++
                             [(Node rk v' l r) | v'  <- shrink v ] ++
                             [(Node rk v l' r) | l' <- shrinkHeap l] ++
                             [(Node rk v l r') | r' <- shrinkHeap r] 
shrinkHeap _               = []
                             
------------------------------------------------------------------------


--function "rightHeight"
rightHeight :: Heap -> Int
rightHeight Leaf = 0
rightHeight (Node _ _ _ r) = (rightHeight r) + 1


--function "rank"
rank :: Heap -> Int
rank Leaf = 0
rank (Node rk _ _ _) = rk


--function "rootVal"
rootVal :: Heap -> Int
rootVal h@(Node _ v _ _) 
  | (h /= Leaf) = v
  | otherwise = error "the input heap is a Leaf"


--function "hasLeftistProperty"
hasLeftistProperty :: Heap -> Bool
hasLeftistProperty Leaf = True
hasLeftistProperty h@(Node _ v l r) = (hasLeftistProperty l) 
                                 && (hasLeftistProperty r) 
                                 && (rightHeight l) >= (rightHeight r)
                                 && (rank h) == (rightHeight h)
                                 && ((l == Leaf) || v >= (rootVal l)) 
                                 && ((r == Leaf) || v >= (rootVal r))



--function "heapSize"
heapSize :: Heap -> Int
heapSize Leaf = 0
heapSize (Node _ v l r) = (heapSize l) + 1 + (heapSize r)
----test for heapSize
prop_heapSize :: Heap -> Bool
prop_heapSize h = (heapSize h) >= 0


--function "heapContents"
heapContents :: Heap -> Set.Set Int
heapContents Leaf = Set.empty
heapContents (Node _ v l r) = Set.unions [(Set.singleton v), 
                                      (heapContents l),
                                      (heapContents r)] 


--function "merge"
merge :: Heap -> Heap -> Heap
merge Leaf h2 = h2
merge h1 Leaf = h1
merge h1@(Node _ v1 l1 r1) h2@(Node _ v2 l2 r2) = if v1 > v2 
                                      then makeT v1 l1 (merge r1 h2) 
                                      else makeT v2 l2 (merge h1 r2)
----test for merge
prop_merge :: Property
prop_merge =
  forAll hasLeftistHeap $ \h1 ->  
   forAll hasLeftistHeap $ \h2 -> 
        collect (heapSize h1) $
        collect (heapSize h2) $
     (if (merge h1 h2) == Leaf then (h1 == Leaf) && (h2 == Leaf)
                              else (((h1 /= Leaf) && ((rootVal h1) == (value (merge h1 h2)))) || ((h2 /= Leaf) && ((rootVal h2) == (value (merge h1 h2))))))
 && (hasLeftistProperty (merge h1 h2))
 && ((heapSize h1) + (heapSize h2) == heapSize (merge h1 h2))
-- && (Set.union (heapContents h1) (heapContents h2) == heapContents (merge h1 h2))
         


prop_hasLeftistHeap :: Heap -> Property
prop_hasLeftistHeap h = collect (heapSize h) $ hasLeftistProperty $ mkrV $ mkLP h
----------------------------------------------------------------------
---try to make a generator to provide test data with hasLeftistProperty

--help-function to trun a heap into a heap satisfied hasLeftistProperty
mkLP :: Heap -> Heap
mkLP Leaf = Leaf
mkLP h@(Node rk v Leaf Leaf) = (Node 1 0 Leaf Leaf)
mkLP h@(Node rk v l Leaf) = (Node 1 (rootVal l) (mkLP l) Leaf)
mkLP h@(Node rk v Leaf r) = (Node 1 (rootVal r) (mkLP r) Leaf)
mkLP h@(Node rk v l r) = 
      if rl >= rr 
         then (Node (rr + 1) v (mkLP l) (mkLP r))
         else (Node (rl + 1) v (mkLP r) (mkLP l))
          where rl = rightHeight (mkLP l)
                rr = rightHeight (mkLP r)
--help-function to make root rooVal larger than its subheap rootVal
mkrV :: Heap -> Heap
mkrV Leaf = Leaf
mkrV (Node rk v Leaf Leaf) = (Node rk v Leaf Leaf)
mkrV (Node rk v l Leaf) = if v >= (rootVal (mkrV l))
                       then (Node rk v (mkrV l) Leaf)
                       else (Node rk (rootVal (mkrV l)) (mkrV l) Leaf)
mkrV (Node rk v Leaf r) = if v >= (rootVal (mkrV r)) 
                       then (Node rk v Leaf (mkrV r))
                       else (Node rk (rootVal (mkrV r)) Leaf (mkrV r))
mkrV (Node rk v l r) = if v >= ma then (Node rk v (mkrV l) (mkrV r))
                                  else (Node rk ma (mkrV l) (mkrV r))
                  where ma = max (rootVal (mkrV l)) (rootVal (mkrV r))

prop_mkrV :: Heap -> Bool
prop_mkrV Leaf = True
prop_mkrV h@(Node rk v l r) = ((ml == Leaf) || mv >= (rootVal ml)) 
                           && ((mr == Leaf) || mv >= (rootVal mr))
           where (Node mrk mv ml mr) = mkrV h
           
    
----generator for test data which satisfy hasLeftistProperty
hasLeftistHeap :: Gen Heap
hasLeftistHeap = mkrV `fmap` (mkLP `fmap` arbitrary) 

----------------------------------------------------------------------
           

--function "makeT"
makeT :: Int -> Heap -> Heap -> Heap
makeT v l r = if (rank l) >= (rank r) then (Node ((rank r) + 1) v l r)
                                      else (Node ((rank l) + 1) v r l)


--function "insert"
insert :: Int -> Heap -> Heap
insert e h 
  | hasLeftistProperty h = merge (Node 1 e Leaf Leaf) h
  | otherwise = error "the input heap does not have leftist property"

----test for insert 
prop_insert :: Int -> Heap -> Property
prop_insert e h = hasLeftistProperty h ==>
              heapSize res == (heapSize h) + 1 
           && heapContents res == Set.union (Set.singleton e)         
                                            (heapContents h) 
                where res = insert e h



--function "findMax"
findMax :: Heap -> Int
findMax h 
   | hasLeftistProperty h && h /= Leaf =
    rootVal h
   | otherwise = error "the Input does not have leftist property or empty"


--function "removeMax"
removeMax :: Heap -> Heap
removeMax h = case h of (Node _ _ l r) -> merge l r
                        Leaf           -> h

----test for removeMax
prop_removeMax :: Heap -> Property
prop_removeMax h =  hasLeftistProperty h ==>
     -- forAll hasLeftistHeap $ \ h ->
      collect (heapSize h) $
      hasLeftistProperty (removeMax h) && 
    --  (h == Leaf || heapContents h == 
    --   (Set.union (heapContents (removeMax h)) (Set.singleton (findMax h)))) &&
      ((removeMax h) == Leaf || ((findMax (removeMax h)) < (findMax h)))


prop_buggyRemoveMaxNv :: Heap -> Property
prop_buggyRemoveMaxNv h =  hasLeftistProperty h ==>
    --  collect (heapSize h) $
      hasLeftistProperty (removeMax h) && 
      ((removeMax h) == Leaf || ((findMax (removeMax h)) < (findMax h)))


prop_buggyRemoveMaxCl :: Property
prop_buggyRemoveMaxCl = 
     forAll hasLeftistHeap $ \ h ->
    --  collect (heapSize h) $
      hasLeftistProperty (removeMax h) && 
      ((removeMax h) == Leaf || ((findMax (removeMax h)) < (findMax h)))



--function "sortedDescending"
sortedDescending :: [Int] -> Bool
sortedDescending [] = True
sortedDescending [x] = True
sortedDescending (x:t@(y:_)) = (x >=y) && sortedDescending t


--function "removeElements"
removeElements :: Heap -> [Int]
removeElements h
   | hasLeftistProperty h = case h of Leaf -> []
                                      _ -> (:) (findMax h) (removeElements (removeMax h))

----test for removeElements
prop_removeElements :: Property
prop_removeElements =  -- hasLeftistProperty h ==> 
           forAll hasLeftistHeap $ \ h ->
            collect (heapSize h) $
            sortedDescending (removeElements h)
         && heapSize h == length (removeElements h)
         && heapContents h == Set.fromList (removeElements h)
     


--function "buildHeap"
buildHeap :: [Int] -> Heap
buildHeap [] = Leaf
buildHeap (x:xs) = insert x (buildHeap xs)

----test for buildHeap 
prop_buildHeap :: [Int] -> Bool
prop_buildHeap l = hasLeftistProperty res &&
                   heapContents res == Set.fromList l &&
                   heapSize res == length l
                    where res = buildHeap l


--function "heapSort"
heapSort :: [Int] -> [Int]
heapSort l = removeElements (buildHeap l)

----test for heapSort
prop_heapSort :: [Int] -> Bool
prop_heapSort l = sortedDescending res &&
                  length res == length l &&
                  Set.fromList res == Set.fromList l
                    where res = heapSort l



------------------------------------timer---------------------------------------
timeIt :: (Fractional c) => (a -> IO b) -> a -> IO c
timeIt action arg = do
       startTime <- getCPUTime
       action arg
       finishTime <- getCPUTime
       return $ fromIntegral (finishTime - startTime)  / 1000000000

oneTestNv = timeout 5000000 $ timeIt quickCheck prop_buggyRemoveMaxNv >>= print 
oneTestCl = timeout 5000000 $ timeIt quickCheck prop_buggyRemoveMaxCl >>= print 

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
---------------------------------------------------------------------------


------for-quickCheck-test------
return []
runTests = $quickCheckAll             
------------------------------- 


          





                           
