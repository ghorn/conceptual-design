-- WettedArea.hs

{-# OPTIONS_GHC -Wall #-}

module Drag.WettedArea( wettedArea
                      , wingWettedArea
                      , paraboloidArea
                      , cylinderArea
                      , printWettedArea
                      ) where

import Config(Config(..))

wettedArea :: Floating a => Config a -> a
wettedArea (Config { diameter_feet = diameter
                   , totalLength_feet = overallLength
                   , noseFineness = fNose
                   , tailFineness = fTail
                   , wingArea_sqFeet = wingArea
                   , thicknessToChordRatio = tOverC
                   }) = totalWettedArea
  where
    centerLength = overallLength - diameter*fNose - diameter*fTail
    
    noseArea = paraboloidArea diameter fNose
    tailArea = paraboloidArea diameter fTail
    centerArea = cylinderArea diameter centerLength
    
    wingWettedArea' = wingWettedArea wingArea tOverC
    
    totalWettedArea = noseArea + tailArea + centerArea + wingWettedArea'

wingWettedArea :: Fractional a => a -> a -> a
wingWettedArea wingArea tOverC = 2.0*(1 + 0.2*tOverC)*wingArea

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



printWettedArea :: Floating a => Config a -> IO ()
printWettedArea (Config { diameter_feet = diameter
                   , totalLength_feet = overallLength
                   , noseFineness = fNose
                   , tailFineness = fTail
                   , wingArea_sqFeet = wingArea
                   , thicknessToChordRatio = tOverC
                   }) = do
  let centerLength = overallLength - diameter*fNose - diameter*fTail
      
      noseArea = paraboloidArea diameter fNose
      tailArea = paraboloidArea diameter fTail
      centerArea = cylinderArea diameter centerLength
      totalFuseArea = noseArea + tailArea + centerArea

      wingWettedArea' = wingWettedArea wingArea tOverC
      totalWettedArea = totalFuseArea + wingWettedArea'

  putStrLn $ "nose cone area:        " ++ show noseArea ++ " ft^2"
  putStrLn $ "tail cone area:        " ++ show tailArea ++ " ft^2"
  putStrLn $ "main fuse area:        " ++ show centerArea ++ " ft^2"
  putStrLn $ "total fuselage area:   " ++ show totalFuseArea ++ " ft^2"
  putStrLn ""
  putStrLn $ "wing wetted area:      " ++ show wingWettedArea' ++ " ft^2"
  putStrLn $ "total wetted area:     " ++ show totalWettedArea ++ " ft^2"
