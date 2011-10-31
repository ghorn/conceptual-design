-- dCd_dDiameter.hs

{-# OPTIONS_GHC -Wall #-}

import Config
import Drag.WettedArea
import Drag.Parasitic

diff :: (Config -> Double) -> Double -> Double
diff f delta = ((f configPlus) - (f configMinus)) / (2*delta)
  where
    configPlus = gaCruiseConfig { diameter_feet =  ((diameter_feet gaCruiseConfig) + delta) }
    configMinus = gaCruiseConfig { diameter_feet = ((diameter_feet gaCruiseConfig) - delta) }


main :: IO ()
main = do
  let deltas = [10.0**x | x <- [-3,-2]] -- [6,(-5.9)..(-1)]]
      wettedAreas = map (diff wettedArea) deltas
      cdParasitics = map (diff cD_parasitic) deltas
  
  let delta = 0.1
      config0 = gaCruiseConfig
      configPlus = gaCruiseConfig { diameter_feet =  ((diameter_feet gaCruiseConfig) + delta) }
      configMinus = gaCruiseConfig { diameter_feet = ((diameter_feet gaCruiseConfig) - delta) }

  print config0
  putStrLn ""
  print configPlus
  putStrLn ""
  print configMinus
  
  let seqAll (x:[]) = seq x
      seqAll (x:xs) = x `seq` (seqAll xs)
      seqAll _ = (\x -> x)

  seqAll (cdParasitics++wettedAreas) mapM_ print cdParasitics
  mapM_ print wettedAreas
