{-# OPTIONS_GHC -Wall #-}

module Design.Weight where

import Design.Config
import Design.WorkingConfig

wingWeight_lbs :: Floating a => Config a -> a
wingWeight_lbs config =
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
    zfw = zeroFuelWeight_lb config
    taperRatio' = taperRatio config
    tc_avg = thicknessToChordRatio config
    sweepEA = (sweep_deg config)*pi/180


horizTailWeight_lbs :: Floating a => Config a -> a
horizTailWeight_lbs config@(Config {horizTail = ht}) =
  5.25*s_he + 0.8e-6*(n_ult'*(b_h**3)*tow*mac_w*sqrt(s_he))/(tc_avg*(cos(sweepEA))**2*l_h*s_hg**1.5)
{-
s_he - exposed horizontal tail area (ft^2)
s_hg - gross horizontal tail area (ft^2)
mac_w - ????????????
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
    mac_w = undefined
    n_ult' = n_ult config
    b_h = htSpan_ft ht
    tow = maxTakeoffWeight_lb config
    tc_avg = ht'tc ht
    sweepEA = (ht'sweep_deg ht)*pi/180
    l_h = ht'lH_ft ht


-- weight of vertical tail
vertTailWeight_lbs :: Floating a => Config a -> a
vertTailWeight_lbs config@(Config {vertTail = vt}) = tTailPenalty*(wVert + wRudder)
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


main :: IO ()
main = do
  let config = gaCruiseConfig :: Config Double
  print config
  putStrLn "\n\n"
  putStrLn $ "wing weight:       " ++ show (wingWeight_lbs config) ++ " lbs"
  putStrLn $ "horiz tail weight: " ++ show (horizTailWeight_lbs config) ++ " lbs"
  putStrLn $ "vert tail weight:  " ++ show (vertTailWeight_lbs config) ++ " lbs"
