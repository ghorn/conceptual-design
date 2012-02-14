{-# OPTIONS_GHC -Wall #-}

module Design.Weight( weightSummary
                    , trueZeroFuelWeight_lb
                    , manufacturersEmptyWeight_lb
                    ) where

import Design.Config
import Design.WorkingConfig
import Aero.Drag.WettedArea( grossFuseWettedArea )
import Atmosphere.Convenience(pressure_lb_per_ft2_from_altitude_ft)
import Plackard.Linesearch(gss)

-- 1. wing
wingWeight_lb :: Floating a => Config a -> a
wingWeight_lb config =
  4.22*s_wg + 1.642e-6*(n_ult'*(b**3)*sqrt(tow*zfw)*(1+2*taperRatio'))/(tc_avg*(cos(sweepEA))**2*s_wg*(1+taperRatio'))
{-
s_wg - gross wing area (ft^2)
n_ult' - ultimate load factor
b  - span (ft)
tow - take off weight (lbs)
zfw - zero fuel weight (lbs)
taperRatio' - taper ratio (little/big)
tc_avg - average thickness/chord
sweepEA - sweep of structural axis (radians)
-}
  where
    s_wg = grossWingArea_ft2 config
    n_ult' = n_ult config
    b = wSpan_ft config
    tow = maxTakeoffWeight_lb config
    zfw = zeroFuelWeightEst_lb config
    taperRatio' = taperRatio config
    tc_avg = thicknessToChordRatio config
    sweepEA = (sweep_deg config)*pi/180

-- 2. horizontal tail
horizTailWeight_lb :: Floating a => Config a -> a
horizTailWeight_lb config@(Config {horizTail = ht}) =
  5.25*s_he + 0.8e-6*(n_ult'*(b_h**3)*tow*mac_w*sqrt(s_he))/(tc_avg*(cos(sweepEA))**2*l_h*s_hg**1.5)
{-
s_he - exposed horizontal tail area (ft^2)
s_hg - gross horizontal tail area (ft^2)
mac_w - main wing's mean aerodynamic chord (ft)
n_ult' - ultimate load factor
b_h  - horizontal tail span (ft)
tow - take off weight (lbs)
tc_avg - horizontal tail average thickness/chord
sweepEA - horizontal tail sweep of structural axis (radians)
l_h - distance behing cg of horizontal tail's aerodynamic center
-}
  where
    s_he = ht'sHe_ft2 ht
    s_hg = ht'sHg_ft2 ht
    mac_w = meanAerodynamicChord_ft config
    n_ult' = n_ult config
    b_h = htSpan_ft ht
    tow = maxTakeoffWeight_lb config
    tc_avg = ht'tc ht
    sweepEA = (ht'sweep_deg ht)*pi/180
    l_h = ht'lH_ft ht

-- 3. vertical tail and rudder
vertTailWeight_lb :: Floating a => Config a -> a
vertTailWeight_lb config@(Config {vertTail = vt}) = tTailPenalty*(wVert + wRudder)
{-
s_v - vertical tail area including rudder (ft^2)
n_ult' - ultimate load factor
b_v  - vertical tail span aka height (ft)
tow - take off weight (lbs)
s_wg - gross main wing area (ft^2)
tc_avg - vertical tail average thickness/chord
sweepEA - vertical tail sweep of structural axis (radians)
-}
  where
    tTailPenalty
      | tTail config = 1.25
      | otherwise    = 1.0
    wRudder = 1.6/3*wVert
    wVert = 2.62*s_v + 1.5e-5*(n_ult'*(b_v**3)*(8.0 + 0.44*tow/s_wg)/(tc_avg*(cos(sweepEA))**2))
      where
        s_v = vt'sV_ft2 vt
        n_ult' = n_ult config
        b_v = vtSpan_ft vt
        tow = maxTakeoffWeight_lb config
        s_wg = grossWingArea_ft2 config
        tc_avg = vt'tc vt
        sweepEA = (vt'sweep_deg vt)*pi/180

-- 4. fuselage
fuseWeight_lb :: (Floating a, Ord a) => Config a -> a
fuseWeight_lb config = (1.051 + 0.102*iFuse)*sFuse*allCargoMarkdown
  where
{-
sFuse - gross fuse wetted area (no cutouts for wings or anything)
p - max pressure differential (lb/ft^2)
b - fuse width
h - fuse height
l - fuse effective length = lTrue - 0.5*(wing root chord)
n - load limit factor at zero fuel weight
w - ZFW_max - wing weight (wing weight including wing-mounted engines, nacelles, and pylons)
-}
    allCargoMarkdown
      | allCargo config = 1 - 0.085
      | otherwise       = 1
    
    iFuse
      | ip >= ib  = ip
      | otherwise = (ip*ip + ib*ib)/(2*ib)
    
    ip = 1.5e-3*p*b
    ib = 1.91e-4*n*w*l/(h*h)
    
    sFuse = grossFuseWettedArea config
    p = (press $ cabinPressAlt_ft config) - (press $ ceiling_ft config)
      where
        press = pressure_lb_per_ft2_from_altitude_ft
    b = diameter_feet config
    h = diameter_feet config
    l = totalLength_feet config - 0.5*(rootChord_ft config)
--    n = warn "fuseWeight_lbs using load limit factor at fixed zero fuel weight" 5.6
    n = 3.95
    w = zeroFuelWeightEst_lb config - wingWeight_lb config

-- 5. landing gear
landingGearWeight_lb :: Fractional a => Config a -> a
landingGearWeight_lb config = 0.04*(maxTakeoffWeight_lb config)

-- 6. surface controls
surfaceControlWeight_lb :: Fractional a => Config a -> a
surfaceControlWeight_lb config = isc*(sH + sV)
  where
    sH = ht'sHg_ft2 $ horizTail config
    sV = vt'sV_ft2 $ vertTail config
    isc = case (surfaceControl config) of FullyPowered -> 3.5
                                          PartPower -> 2.5
                                          FullAerodynamic -> 1.7

-- 7. propulsion system
propulsionWeight_lb :: Fractional a => Config a -> a
propulsionWeight_lb config = 1.6*(enginesDryWeight_lb config)
-- (could use enginesDryWeight = 0.4054*slsThrust**0.9255)

-- 8. auxiliary power unit (APU)
apuWeight_lb :: Num a => Config a -> a
apuWeight_lb config
  | numPax config < 9 = 0
  | otherwise         = 7*(fromIntegral $ numPax config)

-- 9. instruments/navigational equipment               
instrumentsNavEquipmentWeight_lb :: Num a => Config a -> a
instrumentsNavEquipmentWeight_lb config = case acType config of BusinessJet -> 100
                                                                DomesticTransport -> 800
                                                                LongRangeOrOverwater -> 1200

-- 10. hydraulics/pnumatics
hydraulicsPnumaticsWeight_lb :: Fractional a => Config a -> a
hydraulicsPnumaticsWeight_lb config = 0.65*(exposedWingArea_ft2 config)

-- 11. electrical and 12. electronics
electricalAndElectronicsWeight_lb :: Config a -> a
electricalAndElectronicsWeight_lb = electricalAndElectronics_lb

-- 13. furnishings
furnishingsWeight_lb :: Config a -> a
furnishingsWeight_lb = furnishings_lb
--furnishingsWeight_lb config = case acType config of DomesticTransport -> model
--                                                    LongRangeOrOverwater -> model + 23*nSeats
--  where
--    model
--      | nSeats <= 300 = (43.7 -.037*nSeats)*nSeats + 46*nSeats
--      | otherwise     = (43.7 -.037*300   )*nSeats + 46*nSeats

-- 14. air conditioning and anti-ice
airCondAndAntiIceWeight_lb :: Num a => Config a -> a
airCondAndAntiIceWeight_lb config = 15*(fromIntegral $ numPax config)

-- 15. operating items less crew
operItemsWeight_lb :: Num a => Config a -> a
operItemsWeight_lb config = 17*(fromIntegral $ numPax config)

-- 16. crew
crewWeight_lb :: Num a => Config a -> a
crewWeight_lb config = (190 + 50)*(fromIntegral $ numCrew config)

-- 17. flight attendants
flightAttendantsWeight_lb :: Num a => Config a -> a
flightAttendantsWeight_lb config = (170 + 40)*(fromIntegral $ numFlightAttendants config)

-- 18. payload: passengers, bags, and cargo
--     calculate at 225 lb/passenger (including checked and carried baggage)
payloadWeight_lb :: Num a => Config a -> a
payloadWeight_lb config = 225*(fromIntegral $ numPax config)


--------------------- aggregate weights ---------------------------------
manufacturersEmptyWeight_lb :: (Floating a, Ord a) => Config a -> a
manufacturersEmptyWeight_lb config = sum $ map (\f -> f config)
                                     [ wingWeight_lb
                                     , horizTailWeight_lb
                                     , vertTailWeight_lb
                                     , fuseWeight_lb
                                     , airCondAndAntiIceWeight_lb
                                     , electricalAndElectronicsWeight_lb
                                     , surfaceControlWeight_lb
                                     , landingGearWeight_lb
                                     , hydraulicsPnumaticsWeight_lb
                                     , propulsionWeight_lb
                                     , instrumentsNavEquipmentWeight_lb
                                     , apuWeight_lb
                                     , furnishingsWeight_lb
                                     ]

zeroFuelWeight_lb :: (Floating a, Ord a) => Config a -> a
zeroFuelWeight_lb config = manufacturersEmptyWeight_lb config + (sum $ map (\f -> f config)
                                                                 [ crewWeight_lb
                                                                 , operItemsWeight_lb
                                                                 , flightAttendantsWeight_lb
                                                                 , payloadWeight_lb
                                                                 ])

reserveWeight_lb :: (Floating a, Ord a) => Config a -> a
reserveWeight_lb config = 0.08*(zeroFuelWeight_lb config)

landingWeight_lb :: (Floating a, Ord a) => Config a -> a
landingWeight_lb config = zeroFuelWeight_lb config + reserveWeight_lb config + 0.0035*(maxTakeoffWeight_lb config)

fuelWeight_lb :: (Floating a, Ord a) => Config a -> a
fuelWeight_lb config = maxTakeoffWeight_lb config - zeroFuelWeight_lb config - reserveWeight_lb config

trueZeroFuelWeight_lb :: (Floating a, Ord a) => Config a -> a
trueZeroFuelWeight_lb config = gss f (1,15000) 1e-3 1e-3
  where
    f zfw = abs $ zfw - zeroFuelWeight_lb (config {zeroFuelWeightEst_lb = zfw})

main :: IO ()
main = do
  let config = gaCruiseConfig :: Config Double
  print config
  putStrLn "\n\n"  
  putStrLn "-------------------------------------------"
  putStrLn "-------- guessed zero fuel weight: --------"
  putStrLn "-------------------------------------------"
  weightSummary config
  putStrLn "\n\n--------------------------------------------"
  putStrLn "--------- solved zero fuel weight: ---------"
  putStrLn "--------------------------------------------"
  weightSummary (config {zeroFuelWeightEst_lb = trueZeroFuelWeight_lb config})
  

weightSummary :: (Floating a, Ord a, Show a) => Config a -> IO ()
weightSummary config = do
  putStrLn $ "wing:                   " ++ show (wingWeight_lb config) ++ " lb"
  putStrLn $ "horiz tail:             " ++ show (horizTailWeight_lb config) ++ " lb"
  putStrLn $ "vert tail:              " ++ show (vertTailWeight_lb config) ++ " lb"
  putStrLn $ "fuse:                   " ++ show (fuseWeight_lb config) ++ " lb"
  putStrLn $ "gear:                   " ++ show (landingGearWeight_lb config) ++ " lb"
  putStrLn $ "surface control:        " ++ show (surfaceControlWeight_lb config) ++ " lb"
  putStrLn $ "propulsion:             " ++ show (propulsionWeight_lb config) ++ " lb"
  putStrLn $ "apu:                    " ++ show (apuWeight_lb config) ++ " lb"
  putStrLn $ "instr/nav equipment:    " ++ show (instrumentsNavEquipmentWeight_lb config) ++ " lb"
  putStrLn $ "hydraulics/pnumatics:   " ++ show (hydraulicsPnumaticsWeight_lb config) ++ " lb"
  putStrLn $ "electrical/electronics: " ++ show (electricalAndElectronicsWeight_lb config) ++ " lb"
  putStrLn $ "furnishings:            " ++ show (furnishingsWeight_lb config) ++ " lb"
  putStrLn $ "air cond and anti-ice:  " ++ show (airCondAndAntiIceWeight_lb config) ++ " lb"
  putStrLn $ "operating items:        " ++ show (operItemsWeight_lb config) ++ " lb"
  putStrLn $ "crew:                   " ++ show (crewWeight_lb config) ++ " lb"
  putStrLn $ "flight attendants:      " ++ show (flightAttendantsWeight_lb config) ++ " lb"
  putStrLn $ "payload:                " ++ show (payloadWeight_lb config) ++ " lb"

  putStrLn "_______________________________________________" 
  putStrLn $ "a priori zero fuel weight:  " ++ show (zeroFuelWeightEst_lb config) ++ " lb"
  putStrLn $ "zero fuel weight:           " ++ show (zeroFuelWeight_lb config) ++ " lb"
  putStrLn $ "manufacturers empty weight: " ++ show (manufacturersEmptyWeight_lb config) ++ " lb"
  putStrLn $ "reserve:                    " ++ show (reserveWeight_lb config) ++ " lb"
  putStrLn $ "landing weight:             " ++ show (landingWeight_lb config) ++ " lb"
  putStrLn $ "fuel allowance:             " ++ show (fuelWeight_lb config) ++ " lb"
