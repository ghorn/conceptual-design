{-# OPTIONS_GHC -Wall #-}

module Main where

import Graphics.Rendering.Chart hiding (c)
import Graphics.Rendering.Chart.Gtk
import Data.Accessor

--------- INPUTS -------
-- typical cruise mach number
m_typicalCruise :: Double
m_typicalCruise = 0.6

-- structural design altitude
structuralDesignAltitude :: Double
structuralDesignAltitude = 25000

-------- DESIGN CHOICE ----------
-- structural design cruise mach number
m_c :: Double
m_c = m_mo

-------- "MODELING" -------
-- max operating mach number
m_mo :: Double
m_mo = 1.06*m_typicalCruise

-- design dive mach number
m_d :: Double
m_d = m_mo*1.07

-- dive airspeed
v_d :: Double -> Double
v_d z = 1.15*(v_c z)

--------- OUTPUTS ---------
-- design cruise airspeed at alt
v_c_atDesignAlt :: Double
v_c_atDesignAlt = m_c*(speedOfSoundAtAlt structuralDesignAltitude)

-- structural design cruise equivalent airspeed
v_c :: Double -> Double
v_c z = v_c_atDesignAlt*sqrt((densityAtAlt structuralDesignAltitude)/(densityAtAlt z))

-- dive intersection
zCrit_d :: Double
zCrit_d = gss (\z -> abs( (speedOfSoundAtAlt z)*m_d - (v_d z))) (0,100000) (1e-6) (1e-6)

main :: IO ()
main = do
  putStrLn $ "V_c: " ++ show (v_c 20000) ++ " mph at 20,000 ft"
  putStrLn $ "V_d: " ++ show (v_d 20000) ++ " mph at 20,000 ft"
  let dz = 20
      zMax = 49900
      v_c_line = plot_lines_values ^= [[ (v_c z, z) | z <- [0,dz..structuralDesignAltitude::Double]]]
                 $ plot_lines_title ^= "v_c"
                 $ defaultPlotLines
      v_d_line = plot_lines_values ^= [[ (v_d z, z) | z <- [0,dz..zCrit_d::Double]]]
                 $ plot_lines_title ^= "v_d"
                 $ defaultPlotLines
      m_c_line = plot_lines_values ^= [[ ((speedOfSoundAtAlt z)*m_c, z) | z <- [structuralDesignAltitude,structuralDesignAltitude+dz..zMax::Double]]]
                 $ plot_lines_title ^= "m_c"
                 $ defaultPlotLines
      m_d_line = plot_lines_values ^= [[ ((speedOfSoundAtAlt z)*m_d, z) | z <- [zCrit_d,zCrit_d+dz..zMax::Double]]]
                 $ plot_lines_title ^= "m_d"
                 $ defaultPlotLines
      chart = layout1_title ^= "Plackard Diagram"
              $ layout1_plots ^= map (Left . toPlot) [ v_c_line
                                                     , v_d_line
                                                     , m_c_line
                                                     , m_d_line]
              $ defaultLayout1
  
  renderableToWindow (toRenderable chart) 640 480
--  _ <- renderableToPNGFile (toRenderable chart) 640 480 "cf_model.png"
  return ()

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



tau :: Floating a => a
tau = 2/(1+sqrt(5))

gss :: (Floating a, Ord a) => (a -> a) -> (a,a) -> a -> a -> a
gss f (x0,x3) epsX epsF = 0.5*(xl+xr)
  where
    stoppingCriterion (xl',xr') = xr'-xl' > epsX || f (0.5*(xl'+xr')) > epsF
    (xl, xr) = head $ dropWhile stoppingCriterion $ goldenSectionSteps f (x0,x1,x2,x3)
    x1 = x0 + (x3-x0)*(1-tau)
    x2 = x0 + (x3-x0)*tau

goldenSectionSteps :: (Floating a, Ord a) => (a -> a) -> (a, a, a, a) -> [(a,a)]
goldenSectionSteps f (x0, x1, x2, x3)
  | (f x1) < (f x2) = (x0,x2):(goldenSectionSteps f (x0,x1',x1,x2))
  | otherwise       = (x1,x3):(goldenSectionSteps f (x1,x2,x2',x3))
  where
    x1' = x0 + (x2-x0)*(1-tau)
    x2' = x1 + (x3-x1)*tau
