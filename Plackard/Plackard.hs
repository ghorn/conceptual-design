{-# OPTIONS_GHC -Wall #-}

module Main where

import Graphics.Rendering.Chart hiding (c)
import Graphics.Rendering.Chart.Gtk
import Data.Accessor

import Plackard.Atmosphere
import Plackard.Linesearch(gss)

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
  putStrLn $ "V_c: " ++ show (v_c 25000) ++ " mph at 25,000 ft"
  putStrLn $ "V_d: " ++ show (v_d 25000) ++ " mph at 25,000 ft"
  putStrLn $ "zCrit_d: " ++ show zCrit_d ++ " ft"
  putStrLn $ "V_d(crit): " ++ show (v_d zCrit_d) ++ " mph"
  putStrLn $ "V_d(crit): " ++ show ((speedOfSoundAtAlt zCrit_d)*m_d) ++ " mph"
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
