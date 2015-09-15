
{-# LANGUAGE TemplateHaskell #-}


module FWJava 
       (Classname, Superclass, Fieldname,
        Variable, Methodname,
        CT,
        CL,
        Field,
        Constructor,
        Method,
        Term,
        fields,
        mtype,
        mbody,
        override,
        subtype,
        isValue,
        eval) where

import qualified Data.Map as Map
import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad
import System.CPUTime
import System.Timeout

type Classname = String
type Superclass = String
type Fieldname = String
type Variable = String
type Methodname = String
type CT = Map.Map Classname CL --a mapping from class to its CL

{-data Type = TyVar String 
          | TyTop
          | 
-}

data CL = Class Classname Superclass [Field] Constructor [Method] deriving Eq

data Field = Field Classname Fieldname deriving Eq

data Constructor = Mkcon Classname [Field] deriving Eq

data Method = Mkmtd Classname Methodname [Field] Term deriving Eq

data Term = Var Variable 
          | FieAcc Term Fieldname
          | MetInv Term Methodname [Term]
          | New Classname [Term]
          | Cast Classname Term deriving Eq

{-
instance Arbitrary Term where
  arbitrary = sized arbTerm

arbTerm 0 = liftM Var (listOf (elements ['a'..'z']))
arbTerm n = frequency 
          [ (1, liftM Var (listOf (elements ['a'..'z'])))
          , (1, liftM2 FieAcc (arbTerm (n `div` 2)) 
                              (listOf (elements ['a'..'z']))) 
          , (1, liftM3 MetInv (arbTerm (n `div` 2)) 
                              (listOf (elements ['a'..'z'])) 
                              (listOf (arbTerm (n `div` 2))))
          , (1, liftM2 New (listOf (elements ['a'..'z']))
                           (listOf (arbTerm (n `div` 2)))) 
          , (1, liftM2 Cast (listOf (elements ['a'..'z']))
                            (arbTerm (n `div` 2)))  ] 
-}

instance Arbitrary Term where
  arbitrary = sized arbTerm
arbTerm 0 = liftM Var (elements contextVa)
arbTerm n = frequency 
          [ (1, liftM Var  (elements contextVa))
          , (1, liftM2 FieAcc (arbTerm (n `div` 6)) 
                              (elements contextFn))
          , (1, liftM3 MetInv (arbTerm (n `div` 6)) 
                              (elements contextMn) 
                              (listOf (arbTerm (n `div` 6))))
          , (1, liftM2 New (elements context)
                           (listOf (arbTerm (n `div` 6)))) 
          , (1, liftM2 Cast (elements context)
                            (arbTerm (n `div` 6))) ] 


----show----
mergeS :: [String] -> String
mergeS (x:xs) = x ++ (mergeS xs)
mergeS [] = " "

classdeclaration :: CL -> String
classdeclaration (Class cn sc f c m) = "class " ++ cn ++ " extends " ++
                                       sc ++ " {\n" ++ showFields f ++ 
                                       "\n" ++ (showCon c) ++ "\n" ++ (mergeS $ map showMethod m) ++ " }"

showField :: Field -> String
showField (Field cn fn) = cn ++ " " ++ fn

showFields :: [Field] -> String
showFields [] = ""
showFields (f:fs) = (showField f) ++ "; " ++ (showFields fs)

showCon :: Constructor -> String
showCon (Mkcon cn []) = cn ++ "() {super();}"
showCon (Mkcon cn f) = cn ++ "(" ++ (showFields f) ++ ") { super; this.f = f;}"

showMethod :: Method -> String
showMethod (Mkmtd cn mn f t) = cn ++ " " ++ mn ++ "(" ++ (showFields f) ++ ") {return " ++ (showTerm t) ++ ";}"

showTerm :: Term -> String
showTerm (Var v) = v 
showTerm (FieAcc t fn) = "(" ++ (showTerm t) ++ "." ++ fn ++ ")" 
showTerm (MetInv t m fn) = "(" ++ (showTerm t) ++ "." ++ m ++ "(" ++ (showTerms fn) ++ ")"++ ")"
showTerm (New cn t) = "new " ++ cn ++ "(" ++ (showTerms t) ++ ")"
showTerm (Cast cn t) = "(" ++ cn ++ ")" ++ (showTerm t)

--getValue :: Value -> String
--getValue (New cn v) = "new " ++ cn ++ "(" ++ (mergeS $ map getValue v) ++ ")"

showTerms :: [Term] -> String
showTerms [] = ""
showTerms (t:ts) = (showTerm t) ++ ", " ++ (showTerms ts)

instance Show CL where
 show = classdeclaration

instance Show Field where
 show = showField

instance Show Constructor where
 show = showCon

instance Show Method where
 show = showMethod

instance Show Term where
 show = showTerm

--instance Show Value where
-- show = getValue


---try----
ffst = Field "Object" "fst"
newfst = Field "Object" "newfst"
ssnd = Field "Object" "snd"
m = Mkmtd "Pair" "setfst" [newfst] (New "Pair" [(Var "newfst"), (FieAcc (Var "this") "snd")])
p = Class "Pair" "Object" [ffst,ssnd] (Mkcon "Pair" [(Field "Object" "fst"),(Field "Object" "snd")]) [m]
a = Class "A" "Object" [] (Mkcon "A" []) []
b = Class "B" "Object" [] (Mkcon "B" []) []
o = Class "Object" "Object" [] (Mkcon "Object" []) []
term =FieAcc (Cast "Pair" (FieAcc (New "Pair" [(New "Pair" [(New "A" []), (New "B" [])]), (New "A" [])]) "fst")) "snd"
this = "this"


-----some method

ct :: CT
ct = Map.insert "Object" o (Map.insert "B" b (Map.insert "A" a (Map.singleton "Pair" p)))

context :: [Classname]
context = Map.keys ct

contextFn :: [Fieldname]
contextFn = helper (Map.elems ct)
            where helper [] = []
                  helper (c1:cs) = case c1 of 
                                   Class cc s fs c ms -> (fnsFromFs fs) ++ (helper cs)

contextMn :: [Methodname]
contextMn = helper (Map.elems ct)
            where helper [] = []
                  helper (c1:cs) = case c1 of
                                   Class cc s fs c ms -> (mnsFromMs ms) ++ (helper cs)

contextVa :: [Variable]
contextVa = "this":(map (:[]) ['a'..'z'])

mnsFromMs :: [Method] -> [Methodname]
mnsFromMs []     = []
mnsFromMs (m:ms) = case m of Mkmtd c m ts t -> m:(mnsFromMs ms)


--Field lookup
fields :: Classname -> [Field]
fields "Object" = []
fields cn = case (ct Map.! cn) of 
                   (Class cn sc fs c ms) -> fs ++ (fields sc)

--Method type lookup
mtype :: Methodname -> Classname -> ([Classname], Classname)
mtype m cn = case (ct Map.! cn) of
                 (Class cn sc fs c ms) -> if Map.member m mapMs 
                                          then case (mapMs Map.! m) of
                                               (Mkmtd b m bs t) -> ((cnsFromFs bs),b)
                                          else mtype m sc
                  where mapMs = methodstoMap ms 

--helper function
methodstoMap :: [Method] -> Map.Map Methodname Method
methodstoMap [] = Map.empty
methodstoMap (m@(Mkmtd cn mn fs t):ms) = Map.insert mn m (methodstoMap ms) 

cnsFromFs :: [Field] -> [Classname]
cnsFromFs [] = []
cnsFromFs ((Field cn fn):fs) = (:) cn (cnsFromFs fs)    


--Method body lookup
mbody :: Methodname -> Classname -> ([Fieldname], Term)
mbody m cn = case (ct Map.! cn) of
                 (Class cn sc fs c ms) -> if Map.member m mapMs
                                          then case (mapMs Map.! m) of
                                               (Mkmtd b m bs t) -> ((fnsFromFs bs),t)
                                          else mbody m sc
                  where mapMs = methodstoMap ms

--helper function
fnsFromFs :: [Field] -> [Fieldname]
fnsFromFs [] = []
fnsFromFs ((Field cn fn):fs) = fn : (fnsFromFs fs)


--Valid method overriding : judge whether a method m is defined in a subclass of D
override :: Methodname -> Classname -> ([Classname],Classname) -> Bool
override m d (cs,c0) = case (mtype m d) of 
                           (ds,d0) -> ds == cs && c0 == d0 


--subtyping
subtype :: Classname -> Classname -> Bool
subtype c e = if c == e 
              then True 
              else if case (ct Map.! c) of
                      (Class c sc fs co ms) -> e == sc
                   then True
                   else or [subtype c d && subtype d e | d <- context, c /= d, e /= d]


-----evaluation
isValue :: Term -> Bool
isValue (New cn ts) = and (map isValue ts)
isValue _ = False

eval1 :: Term -> Term
-- E-PROJNEW 
eval1 (FieAcc c@(New cn ts) fn) = if isValue c        
                                  then if elem fn (fnsFromFs $ fields cn) 
                                       then ts !! (seeOrder fn (fields cn))
                                       else error "There is no such field"   
                                  else FieAcc (eval1 c) fn -- E-FIELD
-- E-INVKNEW	
eval1 (MetInv c@(New cn ts) m ar) = if isValue c  
                                    then let (xs,t0) = (mbody m cn) 
                                         in subst [this] [c] (subst xs ar t0)--danger
                                    else MetInv (eval1 c) m ar  -- E-INVK-RECV
-- E-CASTNEW
eval1 (Cast d (New c vs)) = if subtype c d        
                            then New c vs
                            else error "Cast error"
-- E-FIELD
eval1 (FieAcc t f) = FieAcc (eval1 t) f
-- E-INVK-ARG
eval1 (MetInv c m ar) = if isValue c && (or (map isValue ar))  
                        then MetInv c m (map helper ar)
                        else MetInv (eval1 c) m ar
                        where helper = (\x-> if isValue x then x else eval1 x) 
-- E-NEW-ARG
eval1 (New c vs) = New c (map helper vs)        
                   where helper = (\x-> if isValue x then x else eval1 x)
-- E-CAST
eval1 (Cast c t0) = Cast c (eval1 t0)           
eval1 t           = t --error "No Evaluation rules applied"


seeOrder :: Fieldname -> [Field] -> Int
seeOrder fn f@(fi@(Field c f1):fs) = if fn == f1 then 0 else 1 + (seeOrder fn fs)

subst :: [Fieldname] -> [Term] -> Term -> Term
subst [] ts t = t
subst fs [] t = t
subst fs ts t = if length fs /= length ts then error "Do not have suffice substition"
                else helper1 (size t) t
                where mapFsTs = zip (helper fs) ts
                      helper []     = []
                      helper (t:ts) = (Var t):(helper ts)
                      helper1 0 t   = t
                      helper1 n t   = helper1 (n-1) (move t mapFsTs)
                      
move :: Term -> [(Term,Term)] -> Term
move t@(Var v) (m:ms)           = if (fst m) == t then snd m else move t ms
move t@(FieAcc t1 fn) (m:ms)    = if (fst m) == t1 then FieAcc (snd m) fn else move t ms
move t@(MetInv t1 mn ar) (m:ms) = if (fst m) == t1 then MetInv (snd m) mn ar else move t ms
move t@(New cn ts) ma           = if ts == [] then t else New cn (map helper ts)
                                  where helper = (\x -> move x ma)
move t@(Cast cn t1) (m:ms)      = if (fst m) == t1 then Cast cn (snd m) else move t ms
move t []                       = t

size :: Term -> Int
size (Var v) = 1
size (FieAcc t fn)   = 1 + (size t)
size (MetInv t m ts) = 1 + (size t) + (sum $ map size ts)
size (New c ts)      = 1 + (sum $ map size ts)
size (Cast c t)      = 1 + (size t)
 

eval :: Term -> Term
eval t = helper ((size t)*2) t
         where helper 0 t = t
               helper n t = helper (n-1) (eval1 t)


prop_eval :: Term -> Bool
prop_eval (Var v) = True
prop_eval t = isValue $ eval t

prop_try :: Term -> Bool
prop_try t = t /= New "A" []



------timer--------------------------------------------------

timeIt :: (Fractional c) => (a -> IO b) -> a -> IO c
timeIt action arg = do
       startTime <- getCPUTime
       action arg
       finishTime <- getCPUTime
       return $ fromIntegral (finishTime - startTime)  / 1000000000

--timeIt' :: (Fractional c) => (a -> b) -> a -> IO c
--timeIt' f = timeIt (\x -> f x `seq` return())

oneTestNv = timeout 5000000 $ timeIt quickCheck prop_tryNv >>= print --change prop here
--oneTestCl = timeout 5000000 $ timeIt quickCheck prop_tryCl >>= print --change prop here

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

-----------------------------------------------------------------------------------------


------for-quickCheck-test-------
return []                     --
runTests = $quickCheckAll     --
--------------------------------
