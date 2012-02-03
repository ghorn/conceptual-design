-- Lift.hs

{-# OPTIONS_GHC -Wall #-}

module Aero.Lift( cruiseCL
                , liftSummary
                ) where

import Aero.Atmosphere
import Design.Config(Config(..))

cruiseCL :: (Ord a, Floating a) => Config a -> a
cruiseCL config = 0.97*maxTakeoffWeight_kg*g/(0.5*rho*v**2*wingArea_sqrMeters)
  where
    g = accelerationDueToGravity
    maxTakeoffWeight_kg = (maxTakeoffWeight_lb config)*0.45359237
    rho = densitySIOfHeightFeet (cruiseAltitude_feet config)
    wingArea_sqrMeters = (wingArea_sqFeet config)/(3.2808399)**2
    v = (cruise_mach config)/a
      where
        a = speedOfSoundMetersPerSecondOfAltitudeFeet (cruiseAltitude_feet config)

liftSummary :: (Ord a, Floating a, Show a) => Config a -> IO ()
liftSummary config = do
  let cl = 0.97*maxTakeoffWeight_kg*g/(0.5*rho*v**2*wingArea_sqrMeters)

      g = accelerationDueToGravity
      maxTakeoffWeight_kg = (maxTakeoffWeight_lb config)*0.45359237
      rho = densitySIOfHeightFeet (cruiseAltitude_feet config)
      wingArea_sqrMeters = (wingArea_sqFeet config)/(3.2808399)**2
      v = (cruise_mach config)*a

      a = speedOfSoundMetersPerSecondOfAltitudeFeet (cruiseAltitude_feet config)

  putStrLn $ "a: " ++ show a
  putStrLn $ "v: " ++ show v
  putStrLn $ "mach: " ++ show (cruise_mach config)
  putStrLn $ "CL: " ++ show cl
  putStrLn $ "rho: " ++ show rho
  putStrLn $ "maxTakeoffWeight_kg: " ++ show maxTakeoffWeight_kg
  putStrLn $ "wingArea_sqrMeters: " ++ show wingArea_sqrMeters
  putStrLn $ "g: " ++ show g
