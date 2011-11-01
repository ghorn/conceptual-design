-- Config.hs

{-# OPTIONS_GHC -Wall #-}

module Config( Config(..)
             , gaCruiseConfig
             , cruiseReynolds
             , bodyFineness
             ) where

import Text.Printf

import Warn(warn)

data Config a = Config { diameter_feet :: a
                       , totalLength_feet :: a
                       , noseFineness :: a
                       , tailFineness :: a
                       , cruiseAltitude_feet :: a
                       , cruise_mach :: a
                       , wingArea_sqFeet :: a
                       , thicknessToChordRatio :: a
                       , maxTakeoffWeight_lb :: a
                       }
instance PrintfArg a => Show (Config a) where
  show config = printf "diameter:           %.3f ft\n" (diameter_feet config) ++
                printf "total length:       %.3f ft\n" (totalLength_feet config) ++
                printf "nose fineness:      %f\n" (noseFineness config) ++ 
                printf "tail fineness:      %f\n" (tailFineness config) ++
                printf "cruise altitude:    %f ft\n" (cruiseAltitude_feet config) ++
                printf "cruise mach:        %f\n" (cruise_mach config) ++
                printf "wing area:          %f ft^2\n" (wingArea_sqFeet config) ++
                printf "t/c:                %f\n" (thicknessToChordRatio config) ++
                printf "max takeoff weight: %f lbs" (maxTakeoffWeight_lb config)
              
gaCruiseConfig :: Fractional a => Config a
gaCruiseConfig = Config { diameter_feet         = 61/12
                        , totalLength_feet      = 400/12
                        , noseFineness          = 2
                        , tailFineness          = 3
                        , cruiseAltitude_feet   = 25000
                        , cruise_mach           = 0.55
                        , wingArea_sqFeet       = 144
                        , thicknessToChordRatio = 0.1
                        , maxTakeoffWeight_lb   = 6000
                        }

cruiseReynolds :: Num a => Config a -> a
cruiseReynolds _ = warn "WARNING: using fixed reynolds only valid at 25,000 ft, 331 knots (mach 0.55), length 400 inches" re
  where
    re = 61685521

bodyFineness :: Fractional a => Config a -> a
bodyFineness config = totalLength_feet config / diameter_feet config
