-- Parasitic.hs

{-# OPTIONS_GHC -Wall #-}

module Drag.Parasitic( cD_parasitic
                     ) where

import Drag.FormAndFrictional(cD_form, cD_frictional)
import Drag.Upsweep(cD_upsweep)
import Warn(warn)

import Config(Config)

cD_parasitic :: Config -> Double
cD_parasitic config = warn msg $ (cD_form config) + (cD_frictional config) + (cD_upsweep config)
  where
    msg = "WARNING: cD_parasitic currently only form/frictional/upsweep" 
