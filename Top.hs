-- top.hs

{-# OPTIONS_GHC -Wall #-}

import Config
import WettedArea
import UpsweepDrag
import ParasiticDrag

import Graphics.Rendering.Chart hiding (c)
import Graphics.Rendering.Chart.Gtk
import Data.Accessor
import Text.Printf

main :: IO ()
main = do
  let config = gaCruiseConfig
    
      k = formFactorMarkup (getMachNumber config) (bodyFineness config)
      cF = cF_skinFriction config
      cD_parasitic' = cD_parasitic config
      sWet = wettedArea config
      sWing = wingArea_sqFeet config
  
  putStrLn "-------------------------- configuration: -----------------------------"
  print gaCruiseConfig

  putStrLn "\n------------------------- wetted area: ------------------------------"
  printWettedArea gaCruiseConfig

  putStrLn "\n------------------------ upsweep drag: ------------------------------"
  let cD_pUpsweep' = cD_pUpsweep config
      cD_upsweep' = cD_upsweep config
  _ <- cD_pUpsweep' `seq` printf "Cd upsweep referenced to fuselage: %.7f\n" cD_pUpsweep'
  _ <- cD_upsweep' `seq` printf "Cd upsweep referenced to wing:     %.7f\n" cD_upsweep'

  putStrLn "\n---------------------- frictional drag: -----------------------------"
  _ <- cF `seq` printf "skin friction coeff Cf referenced to wetted area:\t%.5f\n" cF
  _ <-          printf "skin friction coeff Cf referenced to wing:\t\t%.5f\n\n" (cF*sWet/sWing)
  
  putStrLn "\n------------------------- form drag: --------------------------------"
  _ <- k `seq`  printf "form factor k:\t\t\t\t%.5f\n" k
  _ <- printf "Cd_form referenced to wetted area:\t%.5f\n" ((k-1)*cF)
  _ <- printf "Cd_form referenced to wing area:\t%.5f\n" ((k-1)*cF*sWet/sWing)
  
  putStrLn "\n----------------- frictional drag + form drag: ----------------------"
  _ <- cD_parasitic' `seq` printf "Cd_frictional_form referenced to wing:\t%.5f\n" cD_parasitic'
  
  putStrLn "\n---- total parasitic drag (for now: frictional + form + upsweep): ----"
  _ <- printf "Cd_parasitic referenced to wing:\t%.5f\n\n" (cD_parasitic' + cD_upsweep')
  -- make some plots
--  plotCfModel
--  plotFormFactorModel
  return ()


printWettedArea :: Config -> IO ()
printWettedArea (Config { diameter_feet = diameter
                   , totalLength_feet = overallLength
                   , noseFineness = fNose
                   , tailFineness = fTail
                   }) = do
  let centerLength = overallLength - diameter*fNose - diameter*fTail
      
      noseArea = paraboloidArea diameter fNose
      tailArea = paraboloidArea diameter fTail
      centerArea = cylinderArea diameter centerLength
      totalFuseArea = noseArea + tailArea + centerArea

  putStrLn $ "nose cone area:        " ++ show noseArea ++ " ft^2"
  putStrLn $ "tail cone area:        " ++ show tailArea ++ " ft^2"
  putStrLn $ "main fuse area:        " ++ show centerArea ++ " ft^2"
  putStrLn $ "total fuselage area:   " ++ show totalFuseArea ++ " ft^2"

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
  
      chart = layout1_title ^= "form factor markup k vs body fineness ration (mach 0.55)"
              $ layout1_plots ^= [Left (toPlot line)]
              $ defaultLayout1

  renderableToWindow (toRenderable chart) 640 480
  _ <- renderableToPNGFile (toRenderable chart) 640 480 "k_model.png"
  return ()
