-- FormAndFrictional.hs

{-# OPTIONS_GHC -Wall #-}

module Aero.Drag.FormAndFrictional( cfOfReynolds
                                  , cfOfReynolds'
                                  , cD_formAndFrictional
                                  , cD_form
                                  , cD_frictional
                                  , formFactorMarkup
                                  , cF_skinFriction
                                  ) where

import Aero.Drag.WettedArea
import Design.Config(Config(..), cruiseReynolds, bodyFineness)
import Warn(warn)

cD_formAndFrictional :: (Show a, Floating a) => Config a -> a
cD_formAndFrictional config = k*(cD_frictional config)
  where
    k = formFactorMarkup (cruise_mach config) (bodyFineness config)

cD_form :: Floating a => Show a => Config a -> a
cD_form config = (cD_formAndFrictional config) - (cD_frictional config)

cD_frictional :: (Show a, Floating a) => Config a -> a
cD_frictional config = cF*sWet/sWing
  where
    cF = cF_skinFriction config
    sWing = exposedWingArea_ft2 config
    sWet = wettedArea config

cF_skinFriction :: (Show a, Floating a) => Config a -> a
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

compressibilityFactor :: (Show a, Fractional a) => Config a -> a
compressibilityFactor _ = warn message markdown
  where
    message = "WARNING: using fixed compressibility markdown: " ++ show (markdown*100) ++ "%"
    markdown = 0.97

cfOfReynolds :: (Show a, Floating a) => a -> a
cfOfReynolds = warn "WARNING: using fully turbulent fit (xt/c == 0) in parasitic Cf calculation" cfOfReynolds'

cfOfReynolds' :: (Show a, Floating a) => a -> a
cfOfReynolds' re = warn message $ (1 + skinRoughnessMarkup)*logFit
  where
    message :: String
    message = "MESSAGE: using fixed skin roughness Cf markup: "++show (100*skinRoughnessMarkup)++"%"
    skinRoughnessMarkup = 0.075
    logFit = 0.455/(logBase 10 re)**2.58
    -- fit = 0.074/re**0.2
