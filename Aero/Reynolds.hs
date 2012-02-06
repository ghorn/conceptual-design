-- Reynolds.hs    

{-# OPTIONS_GHC -Wall #-}

module Aero.Reynolds( reynolds
                    , configReynoldsOfRefLenMeters
                    ) where

import Design.Config
import Aero.Atmosphere

reynolds :: Fractional a => a -> a -> a -> a -> a
reynolds rho v referenceLength mu = rho*v*referenceLength/mu

configReynoldsOfRefLenMeters :: (Ord a, Floating a) => Config a -> a -> a
configReynoldsOfRefLenMeters config' referenceLengthMeters = reynolds rho v referenceLengthMeters mu
  where
    config = gaCruiseConfig
    rho = densitySIOfHeightFeet $ cruiseAltitude_feet config
    mu = rho*kv
      where
        kv = kinematicViscositySIOfAltitudeFeet $ cruiseAltitude_feet config
    
    v = (cruise_mach config)*a
      where
        a = speedOfSoundMetersPerSecondOfAltitudeFeet $ cruiseAltitude_feet config
