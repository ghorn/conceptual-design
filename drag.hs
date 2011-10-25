-- drag.hs

{-# OPTIONS_GHC -Wall #-}

import Text.Printf

cD_upsweep :: Floating a => a -> a -> a -> (a, a)
cD_upsweep maxDiameter wingArea hOverL075 = (cD_wing, cD_pUpsweep)
  where 
    cD_pUpsweep = 0.075*hOverL075
    cD_wing = cD_pUpsweep*maxFuseCrossSectionalArea/wingArea
    maxFuseCrossSectionalArea = 0.25*pi*maxDiameter*maxDiameter

main :: IO ()
main = do
  let hOverL075_eclipse500Est = 16/215
      wingAreaSqrInch_eclipse500Est = 21390
      (cD_wing, cD_pUpsweep) = cD_upsweep 61 wingAreaSqrInch_eclipse500Est hOverL075_eclipse500Est
  _ <- printf "upsweep drag referenced to fuselage: %.7f\n" (cD_pUpsweep::Double)
  _ <- printf "upsweep drag referenced to wing:     %.7f\n" (cD_wing::Double)
  return ()
