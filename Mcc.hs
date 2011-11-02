-- Mcc.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import Graphics.Rendering.Chart hiding (c)
import Graphics.Rendering.Chart.Gtk
import Data.Accessor

main :: IO ()
main = do
  
  let lambdasDeg = [0,5..30::Double]
  
  let chart = layout1_title ^= "MDiv vs t/c at CL=0.3 over Sweep"
              $ layout1_plots ^= map (\x -> Left (toPlot (lineOfSweepDeg x))) lambdasDeg
              $ defaultLayout1
  
  renderableToWindow (toRenderable chart) 640 480
  _ <- renderableToPNGFile (toRenderable chart) 640 480 "mDiv_vs_tc.png"
  return ()

lineOfSweepDeg :: (Floating a, Enum a) => a -> PlotLines a a
lineOfSweepDeg sweepDeg = line
  where
    line = plot_lines_values ^= [[ (tc, computeMDiv 0.3 tc sweepDeg)
                                 | tc <- [0.06,0.061..0.2]]]
           $ plot_lines_title ^= "lambda: "++show sweepDeg
           $ defaultPlotLines


computeMDiv :: Floating a => a -> a -> a -> a
computeMDiv a b c = (computeMcc a b c)/0.97

computeMcc :: Floating a => a -> a -> a -> a
computeMcc cl tc lambda_deg = mcc2/cos(lambda_rad)
  where
    lambda_rad = lambda_deg*pi/180
    clp = cl/cos(lambda_rad)**2
    tcp = tc/cos(lambda_rad)
    
    mcc0 = 0.954 - 0.235*clp + 0.0259*clp*clp
    mcc1 = mcc0 - (1.963 - 1.078*clp + 0.350*clp*clp)*tcp
    mcc2 = mcc1 + (2.969 - 2.738*clp + 1.469*clp*clp)*tcp*tcp
