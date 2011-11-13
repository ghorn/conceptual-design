-- Parasitic.hs

{-# OPTIONS_GHC -Wall #-}

module Aero.Drag.Parasitic( cD_parasitic
                          ) where

import Aero.Drag.FormAndFrictional(cD_form, cD_frictional)
import Aero.Drag.Upsweep(cD_upsweep)
import Warn(warn)

import Config(Config)

cD_parasitic :: Floating a => Config a -> a
cD_parasitic config = warn msg $ (cD_form config) + (cD_frictional config) + (cD_upsweep config)
  where
    msg = "WARNING: cD_parasitic currently only form/frictional/upsweep" 
