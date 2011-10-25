-- wettedArea.hs

{-# OPTIONS_GHC -Wall #-}

module WettedArea( wettedArea
                 , paraboloidArea
                 , cylinderArea
                 ) where

import Config(Config(..))

wettedArea :: Config -> Double
wettedArea (Config { diameter_feet = diameter
                   , totalLength_feet = overallLength
                   , noseFineness = fNose
                   , tailFineness = fTail
                   }) = totalArea
  where
    centerLength = overallLength - diameter*fNose - diameter*fTail
    
    noseArea = paraboloidArea diameter fNose
    tailArea = paraboloidArea diameter fTail
    centerArea = cylinderArea diameter centerLength
    
    totalArea = noseArea + tailArea + centerArea

--main :: IO ()
--main = do
--  putStrLn $ "noseArea: " ++ show (paraboloidArea (diameter_feet gaCruiseConfig) (noseFineness gaCruiseConfig))
--  let diameter = diameter_feet gaCruiseConfig
--      lNose = diameter*(noseFineness gaCruiseConfig)
--  putStrLn $ "noseArea: " ++ show (0.75*pi*diameter*lNose)

--coneSectionArea d0 d1 h0
--  | d0 == d1  = 2*pi*r0*h0
--  | d0 > d1   = area d0 d1
--  | otherwise = area d1 d0
--  where
--    area d0 d1 = sTotal - s1
--        
--    r0 = 0.5*d0
--    r1 = 0.5*d1
--    h1 = h0*r1/(r0 - r1)
--    sTotal = pi*r0*sqrt( r0^2 + (h0+h1)^2 )
--    s1 = pi*r1*sqrt( r1^2 + h1^2 )

cylinderArea :: Floating a => a -> a -> a
cylinderArea d h = pi*d*h

paraboloidArea :: Floating a => a -> a -> a
paraboloidArea d fineness = pi*r/(6*h*h)*( (r*r + 4*h*h)**(3/2) - r*r*r )
  where
    r = 0.5*d
    h = fineness*d
