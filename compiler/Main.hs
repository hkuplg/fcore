{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -XMultiParamTypeClasses -XRankNTypes -XFlexibleContexts -XTypeOperators  -XOverlappingInstances #-}
module Main where

import SystemF.Syntax
import Language.Java.Syntax as J
import Prelude hiding (const)
-- import qualified HM
-- import HMParser         (readHM)
import qualified SystemF.Parser
import BaseTransCFJava (createCU)
import Language.Java.Pretty
import MonadLib
import System.Process
import System.Directory

import Translations
import BaseTransCFJava
import ApplyTransCFJava
import Data.Map
import qualified Data.Set as Set
import Data.List
import ClosureF
import Inheritance
import qualified TestSuite as TS
import System.Environment
import System.Exit

type M1 = StateT (Map String Int) (State Int)

type M2 = StateT Int (State (Map J.Exp Int)) 
type M3 = StateT Int (Writer Bool) 

type MAOpt = StateT Int (StateT (Map J.Exp Int) (ReaderT (Set.Set Int) (Writer Bool))) 

sopt :: Translate MAOpt  -- instantiation; all coinstraints resolved
sopt = naive

translate ::  PCExp Int (Var, PCTyp Int) -> MAOpt ([BlockStmt], Exp, PCTyp Int)
translate e = translateM (up sopt) e

compile ::  PFExp Int (Var, PCTyp Int) -> (Block, Exp, PCTyp Int)
compile e = 
  case fst $ runWriter $ (runReaderT (evalStateT (evalStateT (translate (fexp2cexp e)) 0) empty) Set.empty) of
      (ss,exp,t) -> (J.Block ss,exp, t)

{-
type MAOpt = StateT Int (StateT (Map J.Exp Int) (Reader (Set.Set Int))) 
sopt :: Translate MAOpt  -- instantiation; all coinstraints resolved
sopt = naive

translate ::  PCExp Int (Var, PCTyp Int) -> MAOpt ([BlockStmt], Exp, PCTyp Int)
translate e = translateM (up sopt) e

compile ::  PFExp Int (Var, PCTyp Int) -> (Block, Exp, PCTyp Int)
compile e = 
  case runReader (evalStateT (evalStateT (translate (fexp2cexp e)) 0) empty) Set.empty of
      (ss,exp,t) -> (J.Block ss,exp, t)
-}
{-
sopt :: ApplyOptTranslate Translate MAOpt  -- instantiation; all coinstraints resolved
sopt = applyopt

translate e = translateM (to sopt) e
-}
{-
sopt :: TranslateStack (ApplyOptTranslate Translate) MAOpt
sopt = stack

translate e = translateM (to sopt) e
-}

prettyJ :: Pretty a => a -> IO ()
prettyJ = putStrLn . prettyPrint




compilePretty e = let (b,exp,t) = compile e in (prettyJ b >> prettyJ exp >> putStrLn (show t))

compileCU e (Just nameStr) = let (cu,t) = (createCU (compile e) (Just nameStr)) in (prettyJ cu >> putStrLn (show t))

compileCU e Nothing = let (cu,t) = (createCU (compile e) Nothing) in (prettyJ cu >> putStrLn (show t))

-- SystemF to Java
sf2java :: String -> String
sf2java src = let (cu, _) = (createCU (compile (SystemF.Parser.reader src)) Nothing) in prettyPrint cu

-- SystemF file path to Java
-- Example:
--      loadsf2java "id.sf"
loadsf2java :: FilePath -> IO String
loadsf2java = readFile >=> (return . sf2java)

-- `compilesf2java srcPath outputPath` loads a SystemF file at `srcPath`,
-- and writes the compiled Java code to `outputPath`.
-- Example:
--      compilesf2java "id.sf" "id.java"
compilesf2java :: FilePath -> FilePath -> IO ()
compilesf2java srcPath outputPath = loadsf2java srcPath >>= writeFile outputPath
                          
main = do args <- getArgs
          if (length args < 2)
            then putStrLn "help: compiler inputFN outputFN"
            else compilesf2java (args!!0) (args!!1)
