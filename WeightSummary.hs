{-# OPTIONS_GHC -Wall #-}

module Main where

import Design.Config
import Design.WorkingConfig
import Design.Weight

main :: IO ()
main = do
  let config = gaCruiseConfig :: Config Double
  print config
  putStrLn "\n\n"  
  putStrLn "-------------------------------------------"
  putStrLn "-------- guessed zero fuel weight: --------"
  putStrLn "-------------------------------------------"
  weightSummary config
  putStrLn "\n\n--------------------------------------------"
  putStrLn "--------- solved zero fuel weight: ---------"
  putStrLn "--------------------------------------------"
  weightSummary (config {zeroFuelWeightEst_lb = trueZeroFuelWeight_lb config})
