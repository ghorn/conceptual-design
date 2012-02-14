-- Config.hs

{-# OPTIONS_GHC -Wall #-}

module Design.Config( Config(..)
                    , gaCruiseConfig
                    , cruiseReynolds
                    , bodyFineness
                    , chordFeet
                    ) where

import Warn(warn)
import Data.List(intersperse)

data Config a = Config { diameter_feet :: a
                       , totalLength_feet :: a
                       , noseFineness :: a
                       , tailFineness :: a
                       , cruiseAltitude_feet :: a
                       , cruise_mach :: a
                       , wingArea_sqFeet :: a
                       , aspectRatio :: a
                       , thicknessToChordRatio :: a
                       , maxTakeoffWeight_lb :: a
                       }

                   
prettyShow :: Show b => String -> a -> [(String, a -> b, String)] -> String
prettyShow title thingy stuffToPrint = "------ " ++ title ++ ": -------\n" ++
                                        (concat $ (intersperse "\n" $ map nicePrint stuffToPrint))
  where
    maxNumChars = maximum $ map (\(x,_,_) -> length x) stuffToPrint
    nicePrint (name,f,units) = name ++ ":" ++ (replicate (1 + maxNumChars - length name) ' ') ++
                               show (f thingy) ++ (unit units)
    unit "" = ""
    unit x = " ("++x++")"
    
    
instance (Floating a, Show a) => Show (Config a) where
  show config = prettyShow "main configuration" config
                [ ("diameter", diameter_feet, "ft")
                , ("total length", totalLength_feet, "ft")
                , ("nose fineness", noseFineness, "")
                , ("tail fineness", tailFineness, "")
                , ("cruise altitude", cruiseAltitude_feet, "ft")
                , ("cruise mach", cruise_mach, "")
                , ("wing area", wingArea_sqFeet, "ft^2")
                , ("aspect ratio", aspectRatio, "")
                , ("t/c", thicknessToChordRatio, "")
                , ("max takeoff weight", maxTakeoffWeight_lb, "lbs")
                , ("zero fuel weight", zeroFuelWeight_lb, "lbs")
                , ("sweep", sweep_deg, "deg")
                , ("taper ratio", taperRatio, "")
                , ("n_ult", n_ult, "")
                , ("wingspan", wSpan_ft, "ft")
                ] ++ "\n\n" ++ show (horizTail config)
              
gaCruiseConfig :: Fractional a => Config a
gaCruiseConfig = Config { diameter_feet         = 61/12
                        , totalLength_feet      = 400/12
                        , noseFineness          = 2
                        , tailFineness          = 3
                        , cruiseAltitude_feet   = 25000
                        , cruise_mach           = 0.55
                        , wingArea_sqFeet       = 144
                        , aspectRatio           = 9
                        , thicknessToChordRatio = 0.1
                        , maxTakeoffWeight_lb   = 6000
                        }

chordFeet :: Floating a => Config a -> a
chordFeet config = sqrt $ (wingArea_sqFeet config)/(aspectRatio config)

cruiseReynolds :: Num a => Config a -> a
cruiseReynolds _ = warn "WARNING: using fixed reynolds only valid at 25,000 ft, 331 knots (mach 0.55), length 400 inches" re
  where
    re = 61685521

bodyFineness :: Fractional a => Config a -> a
bodyFineness config = totalLength_feet config / diameter_feet config
