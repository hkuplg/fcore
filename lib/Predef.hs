{-# LANGUAGE TemplateHaskell #-}
module Predef (processPredef) where


import JvmTypeQuery
import StringUtils
import JavaUtils


processPredef h = do
  let lists = $(predefList)
  res <- fmap sequence (mapM (\info -> getModuleInfo h info True) lists)
  let info =
        case res of
          Nothing  -> [] -- we cannot panic here, as this will leave the Java process hanging around
          Just ret -> (concatMap flatInfo ret)
  return info

  where
    flatInfo (p, mname) = map (\(ModuleInfo f g t) -> (f, (Just "f2j.prelude", t, g, capitalize mname))) p
