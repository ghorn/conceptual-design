{-# OPTIONS_GHC -Wall #-}

module Plackard.Linesearch(gss) where

tau :: Floating a => a
tau = 2/(1+sqrt(5))

gss :: (Floating a, Ord a) => (a -> a) -> (a,a) -> a -> a -> a
gss f (x0,x3) epsX epsF = 0.5*(xl+xr)
  where
    stoppingCriterion (xl',xr') = xr'-xl' > epsX || f (0.5*(xl'+xr')) > epsF
    (xl, xr) = head $ dropWhile stoppingCriterion $ goldenSectionSteps f (x0,x1,x2,x3)
    x1 = x0 + (x3-x0)*(1-tau)
    x2 = x0 + (x3-x0)*tau

goldenSectionSteps :: (Floating a, Ord a) => (a -> a) -> (a, a, a, a) -> [(a,a)]
goldenSectionSteps f (x0, x1, x2, x3)
  | (f x1) < (f x2) = (x0,x2):(goldenSectionSteps f (x0,x1',x1,x2))
  | otherwise       = (x1,x3):(goldenSectionSteps f (x1,x2,x2',x3))
  where
    x1' = x0 + (x2-x0)*(1-tau)
    x2' = x1 + (x3-x1)*tau
