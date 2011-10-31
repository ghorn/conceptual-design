-- Upsweep.hs

{-# OPTIONS_GHC -Wall #-}

module Drag.Upsweep( cD_upsweep
                   , cD_pUpsweep
                   ) where

import Config(Config(..))

import Debug.Trace

cD_pUpsweep :: Config -> Double
cD_pUpsweep _ = 0.075*hOverL075
  where
    hOverL075 = trace "WARNING: using fuse upsweep estimated from Eclipse 500" 16/215    

cD_upsweep :: Config -> Double
cD_upsweep config = cD_wing
  where 
    maxDiameter = diameter_feet config
    wingArea = wingArea_sqFeet config
    
    cD_wing = (cD_pUpsweep config) * maxFuseCrossSectionalArea / wingArea
    maxFuseCrossSectionalArea = 0.25*pi*maxDiameter*maxDiameter

