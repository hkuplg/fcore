{-# LANGUAGE TemplateHaskell #-}
module Predef (processPredef) where


import Panic
import JvmTypeQuery
import StringUtils
import JavaUtils


processPredef h = do
  let lists = $(predefList)
  res <- fmap sequence (mapM (\info -> getModuleInfo h info True) lists)
  let info =
        case res of
          Nothing  -> panic "Failed to load prelude"
          Just ret -> (concatMap flatInfo ret)
  return info

  where
    flatInfo (p, mname) = map (\(ModuleInfo f g t) -> (f, (Just "f2j.prelude", t, g, capitalize mname))) p
