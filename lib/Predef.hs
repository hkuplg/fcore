{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}

module Predef (getPredefInfoTH) where

import System.IO.Unsafe (unsafePerformIO)
import System.Directory
import System.FilePath

import JvmTypeQuery
import RuntimeProcessManager
import StringUtils
import Data.List
import Src
import Language.Haskell.TH.Syntax (lift)

-- Here I am doing crazy things ...

-- grab all the information of prelude modules, and inject it to
-- the compiler itself
getPredef h = do
  filePaths <- getDirectoryContents "lib/predef"
  let lists = ((map (\path -> (Just "f2j.prelude", takeBaseName path))
                 (filter (isSuffixOf "sf") filePaths)))
  -- return lists
  res <- fmap sequence (mapM (\info -> getModuleInfo h info) lists)
  let info =
        case res of
          Nothing  -> [] -- we cannot panic here, as this will leave the Java process hanging around
          Just ret -> (concatMap flatInfo ret)
  return info

  where
    flatInfo (p, mname) = map (\(ModuleInfo f g t) -> (f, (Just "f2j.prelude", t, g, capitalize mname))) p

getPredefInfo :: [(String, ModuleMapInfo)]
getPredefInfo = unsafePerformIO $ withTypeServer getPredef True

getPredefInfoTH = [| $(lift (getPredefInfo)) |]
