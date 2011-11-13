-- LiftSummary.hs

{-# OPTIONS_GHC -Wall #-}

module Main where

import Aero.Lift(liftSummary)
import Design.Config

main :: IO ()
main = do
  putStrLn "================== cruise configuration lift summary =================="
  liftSummary (gaCruiseConfig :: Config Double)
