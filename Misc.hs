-- Misc.hs

{-# OPTIONS_GHC -Wall #-}

module Misc( pressureOfHeight
           ) where

import Debug.Trace

-- SI units
pressureOfHeight :: Floating a => a -> a
pressureOfHeight h = trace "WARNING: pressureOfHeight (SI) is untested" $ p0*(1 - l*h/t0)**(g*m/(r*l))
  where
    p0 = 101325
    l = 0.0065
    t0 = 288.15
    g = 9.807
    m = 0.029
    r = 8.314
