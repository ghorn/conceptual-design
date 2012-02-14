{-# OPTIONS_GHC -Wall #-}

module Atmosphere.Convenience( pressure_lb_per_ft2_from_altitude_ft
                             ) where

import Atmosphere.Atmosphere
pressure_lb_per_ft2_from_altitude_ft :: (Floating a, Ord a) => a -> a
pressure_lb_per_ft2_from_altitude_ft alt_ft = pressure
  where
    (_, pressure, _, _, _, _) = usAtmosphere alt_ft
