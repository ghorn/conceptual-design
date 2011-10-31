-- Warn.hs

{-# OPTIONS_GHC -Wall #-}

module Warn( warn
           ) where

import System.IO
import System.IO.Unsafe

warningsEnabled :: Bool
warningsEnabled = True

warn :: String -> a -> a
warn msg x
  | warningsEnabled = (unsafePerformIO $ hPutStrLn stderr msg) `seq` x
  | otherwise       = x
