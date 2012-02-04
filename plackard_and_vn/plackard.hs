{-# OPTIONS_GHC -Wall #-}

module Main where

--import Graphics.Rendering.Chart hiding (c)
--import Graphics.Rendering.Chart.Gtk
--import Data.Accessor

-- typical cruise mach number
m_typicalCruise :: Double
m_typicalCruise = 0.55

-- max operating mach number
m_mo :: Double
m_mo = 1.06*m_typicalCruise

-- 1. structural design cruise mach number
m_c :: Double
m_c = m_mo

-- 2. design dive mach number
m_d :: Double
m_d = m_mo*1.07

-- dive airspeed number
v_d :: Double
v_d = 1.15*v_c

-- design altitude
designAltitude :: Double
designAltitude = 25000 -- feet

-- design cruise airspeed
v_c :: Double
v_c = 0


main :: IO ()
main = do
--  let config :: Config Double
--      config = gaCruiseConfig
--    
  return ()


--plotCfModel :: IO ()
--plotCfModel = do
--  let line = plot_lines_values ^= [[ (LogValue re, LogValue (cfOfReynolds' re))
--                                   | y <- [5,5.1..9::Double], let re = 10**y]]
--             $ plot_lines_title ^= "cf"
--             $ defaultPlotLines
--      chart = layout1_title ^= "cf vs Reynolds"
--              $ layout1_plots ^= [Left (toPlot line)]
--              $ defaultLayout1
--  renderableToWindow (toRenderable chart) 640 480
--  _ <- renderableToPNGFile (toRenderable chart) 640 480 "cf_model.png"
--  return ()

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
