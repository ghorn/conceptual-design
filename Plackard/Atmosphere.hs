{-# OPTIONS_GHC -Wall #-}

module Plackard.Atmosphere( Atmos
                          , atmos
                          , densityAtAlt
                          , speedOfSoundAtAlt
                          ) where

densityAtAlt :: Double -> Double
densityAtAlt z = density (atmos z undefined)

speedOfSoundAtAlt :: Double -> Double
speedOfSoundAtAlt z = soundSpeed (atmos z undefined)

data Atmos a = Atmos { staticTemp :: a
                     , staticPress :: a
                     , density :: a
                     , soundSpeed :: a
                     , dynamicPress :: a
                     , totalTemp :: a
                     , forceRatio :: a
                     } deriving Show

-- inputs in feet and miles per hour
atmos :: (Floating a, Ord a) => a -> a -> Atmos a
atmos alt trueAirspeed = Atmos { staticTemp = temp
                               , staticPress = press
                               , density = rho
                               , soundSpeed = a0
                               , dynamicPress = q0
                               , totalTemp = tt0
                               , forceRatio = lrat
                               }
  where
    -- English (Farenheit and psi)
    temp = temp' - 459.7
    press = press' / 144.0
    a0 = a0' * 60.0 / 88.0
    tt0 = tt0' - 459.7
    rho = rho'
    q0 = q0'
               
     -- -- convert to metric (Celius and k-PA)
     -- temp = temp' * 0.55555 - 273.1
     -- press = press' * 0.04788
     -- rho = rho' * 515.4
     -- a0 = a0' * 0.3048 * 3.6
     -- q0 = q0' * 0.04788
     -- tt0 = tt0' * .55555 - 273.1
    
    rgas = 1718  -- ft2/sec2 R
    gamma = 1.4
    a0' = sqrt( gamma*rgas*temp')
    rho' = press'/(rgas*temp')
    mach = trueAirspeed/a0
    q0' = 0.5*gamma*press'*mach*mach
    tt0' = temp'*(1.0+0.5*(gamma-1.0)*mach*mach)
    
    -- force ratio
    rho0 = 0.00237
    lrat = rho' / rho0

    (temp', press')
      -- Troposphere
      | alt <= 36152                 = (518.6 - 3.56 * alt/1000, 2116.217 * ((temp'/518.6)**5.256))
      -- Stratosphere
      | alt >= 36152 && alt <= 82345 = (389.98, 2116.217 * 0.2236 * exp((36000-alt)/(53.35*389.98)))
      -- 
      | alt >= 82345 = (389.98 + 1.645 * (alt - 82345)/1000, 2116.217 *0.02456 * ((temp'/389.98)**(-11.388)))
