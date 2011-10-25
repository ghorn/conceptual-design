-- wettedArea.hs

{-# OPTIONS_GHC -Wall #-}

module WettedArea( wettedArea
                 ) where

wettedArea :: Floating a => a -> a -> a -> a -> a
wettedArea diameter overallLength noseFineness tailFineness = totalArea
  where
    centerLength = overallLength - diameter*noseFineness - diameter*tailFineness
    
    noseArea = paraboloidArea diameter noseFineness
    tailArea = paraboloidArea diameter tailFineness
    centerArea = cylinderArea diameter centerLength
    
    totalArea = noseArea + tailArea + centerArea


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
    

main :: IO ()
main = do
  
  let diameter = 61 / 12 :: Double
      overallLength = 400 / 12
      noseFineness = 2
      tailFineness = 3
      
      centerLength = overallLength - diameter*noseFineness - diameter*tailFineness
      
      noseArea = paraboloidArea diameter noseFineness
      tailArea = paraboloidArea diameter tailFineness
      centerArea = cylinderArea diameter centerLength
      
      totalArea = noseArea + tailArea + centerArea

  putStrLn $ "noseArea: " ++ show noseArea
  putStrLn $ "tailArea: " ++ show tailArea
  putStrLn $ "centerArea: " ++ show centerArea
  putStrLn $ "totalArea: " ++ show totalArea
