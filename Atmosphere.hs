-- Atmosphere.hs

{-# OPTIONS_GHC -Wall #-}

module Atmosphere( densitySIOfHeightFeet
                 , atmospherePlots
                 ) where

import Warn(warn)

import Graphics.Rendering.Chart hiding (c)
import Graphics.Rendering.Chart.Gtk
import Data.Accessor

---- SI units
--pressureOfHeight :: Floating a => a -> a
--pressureOfHeight h = warn "WARNING: pressureOfHeight (SI) is untested" $ p0*(1 - l*h/t0)**(g*m/(r*l))
--  where
--    p0 = 101325
--    l = 0.0065
--    t0 = 288.15
--    g = 9.807
--    m = 0.029
--    r = 8.314

densitySIOfHeightFeet :: (Ord a, Floating a) => a -> a
densitySIOfHeightFeet z_feet = warn "WARNING: densityOfHeight (SI) is untested" $ rho_SI
  where
    rho_SI = rho0*exp(-gamma*g*z_meters/(a0*a0))
    rho0 = 1.225
    gamma = 1.4
    g = 9.807
    z_meters = z_feet/3.2808399
    tempC = temperatureCOfHeightFeet z_feet
    a0 = speedOfSoundMetersPerSecondOfTemperatureC tempC

speedOfSoundMetersPerSecondOfAltitudeFeet :: (Ord a, Floating a) => a -> a
speedOfSoundMetersPerSecondOfAltitudeFeet alt_feet = a
  where
    a = speedOfSoundMetersPerSecondOfTemperatureC tempC
    tempC = temperatureCOfHeightFeet alt_feet


speedOfSoundMetersPerSecondOfTemperatureC :: Floating a => a -> a
speedOfSoundMetersPerSecondOfTemperatureC tempC = warn "WARNING: speedOfSoundOfTemperatureC (SI) is untested" $
                                                  331.3*sqrt(1 + tempC/273.15)

temperatureCOfHeightFeet :: (Ord a, Fractional a) => a -> a
temperatureCOfHeightFeet alt
  | alt < -10   = error "can't calculate temperature at negative altitude"
  | alt < 36000 = 15 - 143/72*alt/1000
  | alt < 65000 = -56.5
  | otherwise   = error "can't calculate temperature above 65000 ft"

atmospherePlots :: IO ()
atmospherePlots = do
  plotTemperatureVsAltitude
  plotDensity
  plotSpeedOfSoundVsTemp
  plotSpeedOfSoundVsAltitude

plotDensity :: IO ()
plotDensity = do
  let line = plot_lines_values ^= [[ (alt, densitySIOfHeightFeet alt)
                                   | alt <- [0,100..40000::Double]]]
             $ plot_lines_title ^= "density SI"
             $ defaultPlotLines
      chart = layout1_title ^= "density (SI) vs altitude (ft)"
              $ layout1_plots ^= [Left (toPlot line)]
              $ defaultLayout1
  renderableToWindow (toRenderable chart) 640 480
  _ <- renderableToPNGFile (toRenderable chart) 640 480 "density_vs_alt.png"
  return ()

plotTemperatureVsAltitude :: IO ()
plotTemperatureVsAltitude = do
  let line = plot_lines_values ^= [[ (alt, temperatureCOfHeightFeet alt)
                                   | alt <- [0,100..50000::Double]]]
             $ plot_lines_title ^= "temperature"
             $ defaultPlotLines
      chart = layout1_title ^= "temperatureC vs height ft"
              $ layout1_plots ^= [Left (toPlot line)]
              $ defaultLayout1
  renderableToWindow (toRenderable chart) 640 480
  _ <- renderableToPNGFile (toRenderable chart) 640 480 "temperatureC_vs_alt_feet.png"
  return ()

plotSpeedOfSoundVsAltitude :: IO ()
plotSpeedOfSoundVsAltitude = do
  let line = plot_lines_values ^= [[ (alt, speedOfSoundMetersPerSecondOfAltitudeFeet alt)
                                   | alt <- [0,100..50000::Double]]]
             $ plot_lines_title ^= "speed of sound"
             $ defaultPlotLines
      chart = layout1_title ^= "speed of sound (m/s) vs height ft"
              $ layout1_plots ^= [Left (toPlot line)]
              $ defaultLayout1
  renderableToWindow (toRenderable chart) 640 480
  _ <- renderableToPNGFile (toRenderable chart) 640 480 "temperatureC_vs_alt_feet.png"
  return ()


plotSpeedOfSoundVsTemp :: IO ()
plotSpeedOfSoundVsTemp = do
  let line = plot_lines_values ^= [[ (tempC, speedOfSoundMetersPerSecondOfTemperatureC tempC)
                                   | tempC <- [-100,-99..100::Double]]]
             $ plot_lines_title ^= "speedOfSound"
             $ defaultPlotLines
      chart = layout1_title ^= "speed of sound vs temperatureC"
              $ layout1_plots ^= [Left (toPlot line)]
              $ defaultLayout1
  renderableToWindow (toRenderable chart) 640 480
  _ <- renderableToPNGFile (toRenderable chart) 640 480 "speedOfSound_vs_temperatureC.png"
  return ()
