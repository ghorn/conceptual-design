{-# OPTIONS_GHC -Wall #-}

module Main where

import Graphics.Rendering.Chart hiding (c)
import Graphics.Rendering.Chart.Gtk
import Data.Accessor
import Text.Printf

import Design.Config
import Design.WorkingConfig
import Aero.Drag.WettedArea
import Aero.Drag.Upsweep
import Aero.Drag.FormAndFrictional

main :: IO ()
main = do
  let config :: Config Double
      config = gaCruiseConfig
    
      k = formFactorMarkup (cruise_mach config) (bodyFineness config)
      cF = cF_skinFriction config
      cD_formAndFrictional' = cD_formAndFrictional config
      sWet = wettedArea config
      sWing = exposedWingArea_ft2 config
  
  putStrLn "-------------------------- configuration: -----------------------------"
  print config

  putStrLn "\n------------------------- wetted area: ------------------------------"
  printWettedArea config

  putStrLn "\n------------------------ upsweep drag: ------------------------------"
  let cD_pUpsweep' = cD_pUpsweep config
      cD_upsweep' = cD_upsweep config
  _ <- cD_pUpsweep' `seq` printf "Cd upsweep referenced to fuselage: %.7f\n" cD_pUpsweep'
  _ <- cD_upsweep' `seq` printf "Cd upsweep referenced to wing:     %.7f\n" cD_upsweep'

  putStrLn "\n---------------------- frictional drag: -----------------------------"
  _ <- cF   `seq` printf "skin friction coeff Cf referenced to wetted area:\t%.5f\n" cF
  _ <- sWet `seq` printf "skin friction coeff Cf referenced to wing:\t\t%.5f\n\n" (cF*sWet/sWing)
  
  putStrLn "\n------------------------- form drag: --------------------------------"
  _ <- k `seq`  printf "form factor k:\t\t\t\t%.5f\n" k
  _ <- printf "Cd_form referenced to wetted area:\t%.5f\n" ((k-1)*cF)
  _ <- printf "Cd_form referenced to wing area:\t%.5f\n" ((k-1)*cF*sWet/sWing)
  
  putStrLn "\n----------------- frictional drag + form drag: ----------------------"
  _ <- cD_formAndFrictional' `seq` printf "Cd_frictional_form referenced to wing:\t%.5f\n" cD_formAndFrictional'
  
  putStrLn "\n---- total parasitic drag (for now: frictional + form + upsweep): ----"
  _ <- printf "Cd_parasitic referenced to wing:\t%.5f\n\n" (cD_formAndFrictional' + cD_upsweep')
  
  -- make some plots
  --plotCfModel
  --plotFormFactorModel
  return ()


plotCfModel :: IO ()
plotCfModel = do
  let line = plot_lines_values ^= [[ (LogValue re, LogValue (cfOfReynolds' re))
                                   | y <- [5,5.1..9::Double], let re = 10**y]]
             $ plot_lines_title ^= "cf"
             $ defaultPlotLines
      chart = layout1_title ^= "cf vs Reynolds"
              $ layout1_plots ^= [Left (toPlot line)]
              $ defaultLayout1
  renderableToWindow (toRenderable chart) 640 480
  _ <- renderableToPNGFile (toRenderable chart) 640 480 "cf_model.png"
  return ()

plotFormFactorModel :: IO ()
plotFormFactorModel = do
  let line = plot_lines_values ^= [[ (fr, formFactorMarkup 0.55 fr) | fr <- [4,4.1..10::Double]]]
             $ plot_lines_title ^= "k"
             $ defaultPlotLines
  
      chart = layout1_title ^= "form factor markup k vs body fineness ratio (mach 0.55)"
              $ layout1_plots ^= [Left (toPlot line)]
              $ defaultLayout1

  renderableToWindow (toRenderable chart) 640 480
  _ <- renderableToPNGFile (toRenderable chart) 640 480 "k_model.png"
  return ()
