module RuntimeProcessManager (withRuntimeProcess) where

import JavaUtils      (getClassPath)
import StringPrefixes (namespace)

import System.IO      (Handle, hSetBuffering, BufferMode(..))
import System.Process

withRuntimeProcess :: String -> BufferMode -> ((Handle,Handle) -> IO a) -> Bool -> IO a
withRuntimeProcess class_name buffer_mode do_this loadPrelude
  = do cp <- if loadPrelude then return "runtime/runtime.jar" else getClassPath
       let p = (proc "java" ["-cp", cp, namespace ++ class_name, cp])
                  {std_in = CreatePipe, std_out = CreatePipe}
       (Just inP, Just outP, _, proch) <- createProcess p
       hSetBuffering inP buffer_mode
       hSetBuffering outP buffer_mode
       result <- do_this (inP, outP)
       terminateProcess proch
       return result
