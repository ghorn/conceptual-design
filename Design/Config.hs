-- Config.hs

{-# OPTIONS_GHC -Wall #-}

module Design.Config( Config(..)
                    , HorizTail(..)
                    , VertTail(..)
                    , SurfaceControl(..)
                    , ACType(..)
                    , cruiseReynolds
                    , bodyFineness
                    , meanAerodynamicChord_ft
                    , rootChord_ft
                    , wSpan_ft
                    , htSpan_ft
                    , vtSpan_ft
                    ) where

import Warn(warn)
import Data.List(intersperse)

data SurfaceControl = FullyPowered | PartPower | FullAerodynamic deriving (Eq, Show)
data ACType = BusinessJet | DomesticTransport | LongRangeOrOverwater deriving (Eq, Show)

data Config a = Config { diameter_feet :: a
                       , totalLength_feet :: a
                       , noseFineness :: a
                       , tailFineness :: a
                       , cruiseAltitude_feet :: a
                       , cruise_mach :: a
                       , exposedWingArea_ft2 :: a
                       , grossWingArea_ft2 :: a
                       , aspectRatio :: a
                       , thicknessToChordRatio :: a
                       , maxTakeoffWeight_lb :: a
                       , zeroFuelWeightEst_lb :: a
                       , sweep_deg :: a
                       , taperRatio :: a
                       , n_ult :: a
                       , cabinPressAlt_ft :: a
                       , ceiling_ft :: a
                       , enginesDryWeight_lb :: a
                       , electricalAndElectronics_lb :: a
                       , furnishings_lb :: a
                       , numPax :: Int
                       , numCrew :: Int
                       , numFlightAttendants :: Int
                       , tTail :: Bool
                       , allCargo :: Bool
                       , surfaceControl :: SurfaceControl
                       , acType :: ACType
                       , horizTail :: HorizTail a
                       , vertTail :: VertTail a
                       }

data HorizTail a = HorizTail { ht'ar :: a -- aspect ratio
                             , ht'sHe_ft2 :: a -- exposed area
                             , ht'sHg_ft2 :: a -- gross area
                             , ht'sweep_deg :: a -- sweep
                             , ht'tc :: a -- average thickness/chord
                             , ht'lH_ft :: a -- distance behind aircraft cg of horiz tail's aerodynamic center
                             }
                   
data VertTail a = VertTail { vt'ar :: a -- aspect ratio
                           , vt'sV_ft2 :: a -- area including rudder
                           , vt'sweep_deg :: a -- sweep
                           , vt'tc :: a -- average thickness/chord
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
    
    
instance (Floating a, Show a) => Show (HorizTail a) where
  show ht = prettyShow "horizontal tail" ht
            [ ("ar", ht'ar, "")
            , ("exposed area", ht'sHe_ft2, "ft^2")
            , ("gross area", ht'sHg_ft2, "ft^2")
            , ("sweep", ht'sweep_deg, "deg")
            , ("t/c", ht'tc, "")
            , ("lH", ht'lH_ft, "ft behind cg of tail aerodynamic center")
            , ("span", htSpan_ft, "ft")
            ]

instance (Floating a, Show a) => Show (VertTail a) where
  show vt = prettyShow "vertical tail" vt
            [ ("ar", vt'ar, "")
            , ("area", vt'sV_ft2, "ft^2, including rudder")
            , ("sweep", vt'sweep_deg, "deg")
            , ("t/c", vt'tc, "")
            , ("span", vtSpan_ft, "ft")
            ]

instance (Floating a, Show a) => Show (Config a) where
  show config = prettyShow "main configuration" config
                [ ("diameter", diameter_feet, "ft")
                , ("total length", totalLength_feet, "ft")
                , ("nose fineness", noseFineness, "")
                , ("tail fineness", tailFineness, "")
                , ("cruise altitude", cruiseAltitude_feet, "ft")
                , ("cruise mach", cruise_mach, "")
                , ("exposed wing area", exposedWingArea_ft2, "ft^2")
                , ("gross wing area", grossWingArea_ft2, "ft^2")
                , ("aspect ratio", aspectRatio, "")
                , ("t/c", thicknessToChordRatio, "")
                , ("max takeoff weight", maxTakeoffWeight_lb, "lbs")
                , ("zero fuel weight (est)", zeroFuelWeightEst_lb, "lbs")
                , ("sweep", sweep_deg, "deg")
                , ("taper ratio", taperRatio, "")
                , ("n_ult", n_ult, "")
                , ("wingspan", wSpan_ft, "ft")
                , ("cabin pressure altitude", cabinPressAlt_ft, "ft")
                , ("ceiling", ceiling_ft, "ft")
                , ("engines dry weight", enginesDryWeight_lb, "lb")
                , ("electrical/electronics weight", electricalAndElectronics_lb, "lb")
                , ("furnishings weight", furnishings_lb, "lb")
--                , ("T-tail?", tTail, "")
--                , ("all cargo?", allCargo, "")
--                , ("surface control", surfaceControl, "")
--                , ("# crew", numCrew, "")
--                , ("# passengers", numPax, "")
--                , ("# flight attendants", numFlightAttendants, "")
--                , ("type", acType, "")
                ] ++
                "\n\n" ++ show (horizTail config) ++
                "\n\n" ++ show (vertTail config)
              

-- derived quantities
htSpan_ft :: Floating a => HorizTail a -> a
htSpan_ft (HorizTail {ht'ar = ar, ht'sHg_ft2 = sHg_ft2}) = sqrt(sHg_ft2*ar)
                   
vtSpan_ft :: Floating a => VertTail a -> a
vtSpan_ft (VertTail {vt'ar = ar, vt'sV_ft2 = sV_ft2}) = sqrt(sV_ft2*ar)
                   
wSpan_ft :: Floating a => Config a -> a
wSpan_ft (Config {aspectRatio = ar, grossWingArea_ft2 = s}) = sqrt(s*ar)

meanAerodynamicChord_ft :: Floating a => Config a -> a
meanAerodynamicChord_ft config = sqrt $ (grossWingArea_ft2 config)/(aspectRatio config)

rootChord_ft :: Floating a => Config a -> a
rootChord_ft config = 2*(meanAerodynamicChord_ft config)/(1 + taperRatio config)

cruiseReynolds :: Num a => Config a -> a
cruiseReynolds _ = warn "WARNING: using fixed reynolds only valid at 25,000 ft, 331 knots (mach 0.55), length 400 inches" re
  where
    re = 61685521

bodyFineness :: Fractional a => Config a -> a
bodyFineness config = totalLength_feet config / diameter_feet config
