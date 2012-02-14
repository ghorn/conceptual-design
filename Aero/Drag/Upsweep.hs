-- Upsweep.hs

{-# OPTIONS_GHC -Wall #-}

module Aero.Drag.Upsweep( cD_upsweep
                        , cD_pUpsweep
                        ) where

import Design.Config(Config(..))
import Warn(warn)

cD_pUpsweep :: Fractional a => Config a -> a
cD_pUpsweep _ = 0.075*hOverL075
  where
    hOverL075 = warn "WARNING: using fuse upsweep estimated from Eclipse 500" $ 16/215

cD_upsweep :: Floating a => Config a -> a
cD_upsweep config = cD_wing
  where 
    maxDiameter = diameter_feet config
    exposedWingArea = exposedWingArea_ft2 config
    
    cD_wing = (cD_pUpsweep config) * maxFuseCrossSectionalArea / exposedWingArea
    maxFuseCrossSectionalArea = 0.25*pi*maxDiameter*maxDiameter

