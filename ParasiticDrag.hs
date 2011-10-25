-- ParasiticDrag.hs

{-# OPTIONS_GHC -Wall #-}

module ParasiticDrag( cfOfReynolds
                    , cfOfReynolds'
                    , cD_parasitic
                    , formFactorMarkup
                    , cF_skinFriction
                    ) where

import Debug.Trace

import WettedArea
import Config(Config(..), cruiseReynolds, bodyFineness, getMachNumber)

cD_parasitic :: Config -> Double
cD_parasitic config = k*cF*sWet/sWing
  where
    k = formFactorMarkup (getMachNumber config) (bodyFineness config)
    cF = cF_skinFriction config
    sWing = wingArea_sqFeet config
    sWet = wettedArea config
    
cF_skinFriction :: Config -> Double
cF_skinFriction config = cF_incompressible*(compressibilityFactor config)
  where
    cF_incompressible = cfOfReynolds (cruiseReynolds config)

formFactorMarkup :: Floating a => a -> a -> a
formFactorMarkup mach bodyFineness' = (1 + c*bigDuMaxU0)^(2::Int)
  where
    c = 2.3
    bigDuMaxU0 = a/(2-a)/sqrt( 1 - m**2 )
    a = 2 * (1 - m**2) * d**2 / (bigD**3) * (atanh bigD - bigD)
    bigD = sqrt( 1 - (1 - m**2) * d**2 )
    d = 1/bodyFineness'
    m = mach

compressibilityFactor :: Config -> Double
compressibilityFactor _ = trace message markdown
  where
    message = "WARNING: using fixed compressibility markdown: " ++ show (markdown*100) ++ "%"
    markdown = 0.97

cfOfReynolds :: Floating a => a -> a
cfOfReynolds = trace "WARNING: using fully turbulent fit (xt/c == 0) in parasitic Cf calculation" cfOfReynolds'

cfOfReynolds' :: Floating a => a -> a
cfOfReynolds' re = trace message $ (1 + skinRoughnessMarkup)*logFit
  where
    message :: String
    message = "MESSAGE: using fixed skin roughness Cf markup: "++show (100*skinRoughnessMarkup)++"%"
    skinRoughnessMarkup = 0.075
    logFit = 0.455/(logBase 10 re)**2.58
    -- fit = 0.074/re**0.2
