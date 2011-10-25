-- Reynolds.hs    

{-# OPTIONS_GHC -Wall #-}

module Reynolds( reynolds
               ) where

reynolds :: Fractional a => a -> a -> a -> a -> a
reynolds rho v referenceLength mu = rho*v*referenceLength/mu
