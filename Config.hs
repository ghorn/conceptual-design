-- Config.hs

{-# OPTIONS_GHC -Wall #-}

module Config( Config(..)
             , gaCruiseConfig
             , cruiseReynolds
             , bodyFineness
             , getMachNumber
             ) where

import Text.Printf

import Warn(warn)

data Config a = Config { diameter_feet :: a
                       , totalLength_feet :: a
                       , noseFineness :: a
                       , tailFineness :: a
                       , altitude_feet :: a
                       , airspeed_knots :: a
                       , wingArea_sqFeet :: a
                       , thicknessToChordRatio :: a
                       }
instance PrintfArg a => Show (Config a) where
  show config = printf "diameter:       %.3f ft\n" (diameter_feet config) ++
                printf "total length:   %.3f ft\n" (totalLength_feet config) ++
                printf "nose fineness:  %f\n" (noseFineness config) ++ 
                printf "tail fineness:  %f\n" (tailFineness config) ++
                printf "altitude:       %f ft\n" (altitude_feet config) ++
                printf "airspeed:       %f knots\n" (airspeed_knots config) ++
                printf "wing area:      %f ft^2" (wingArea_sqFeet config) ++
                printf "t/c:            %f" (thicknessToChordRatio config)
              
gaCruiseConfig :: Fractional a => Config a
gaCruiseConfig = Config { diameter_feet         = 61/12
                        , totalLength_feet      = 400/12
                        , noseFineness          = 2
                        , tailFineness          = 3
                        , altitude_feet         = 25000
                        , airspeed_knots        = 331
                        , wingArea_sqFeet       = 144
                        , thicknessToChordRatio = 0.1
                        }

cruiseReynolds :: Num a => Config a -> a
cruiseReynolds _ = warn "WARNING: using fixed reynolds only valid at 25,000 ft, 331 knots (mach 0.55), length 400 inches" re
  where
    re = 61685521

bodyFineness :: Fractional a => Config a -> a
bodyFineness config = totalLength_feet config / diameter_feet config

getMachNumber :: Fractional a => Config a -> a
getMachNumber _ = warn "WARNING: using fixed mach number of 0.55 only valid at from 25,000 ft, 331 knots" 0.55
