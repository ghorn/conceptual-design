-- Config.hs

{-# OPTIONS_GHC -Wall #-}

module Config( Config(..)
             , gaCruiseConfig
             , cruiseReynolds
             , bodyFineness
             , getMachNumber
             ) where

import Debug.Trace
import Text.Printf

data Config = Config { diameter_feet :: Double
                     , totalLength_feet :: Double
                     , noseFineness :: Double
                     , tailFineness :: Double
                     , altitude_feet :: Double
                     , airspeed_knots :: Double
                     , wingArea_sqFeet :: Double
                     }
instance Show Config where
  show config = printf "diameter:       %.3f ft\n" (diameter_feet config) ++
                printf "total length:   %.3f ft\n" (totalLength_feet config) ++
                printf "nose fineness:  %f\n" (noseFineness config) ++ 
                printf "tail fineness:  %f\n" (tailFineness config) ++
                printf "altitude:       %f ft\n" (altitude_feet config) ++
                printf "airspeed:       %f knots\n" (airspeed_knots config)++
                printf "wing area:      %f ft^2" (wingArea_sqFeet config)
              
gaCruiseConfig :: Config
gaCruiseConfig = Config { diameter_feet       = 61/12
                        , totalLength_feet    = 400/12
                        , noseFineness        = 2
                        , tailFineness        = 3
                        , altitude_feet       = 25000
                        , airspeed_knots      = 331
                        , wingArea_sqFeet     = 144
                        }

cruiseReynolds :: Config -> Double
cruiseReynolds _ = trace "WARNING: using fixed reynolds only valid at 25,000 ft, 331 knots (mach 0.55), length 400 inches" re
  where
    re = 61685521

bodyFineness :: Config -> Double
bodyFineness config = totalLength_feet config / diameter_feet config

getMachNumber :: Config -> Double
getMachNumber _ = trace "WARNING: using fixed mach number of 0.55 only valid at from 25,000 ft, 331 knots" 0.55
