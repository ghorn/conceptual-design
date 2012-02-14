{-# OPTIONS_GHC -Wall #-}

module Design.WorkingConfig( gaCruiseConfig
                           ) where

import Design.Config

gaCruiseConfig :: Fractional a => Config a
gaCruiseConfig = Config { diameter_feet         = 61/12
                        , totalLength_feet      = 400/12
                        , noseFineness          = 2
                        , tailFineness          = 3
                        , cruiseAltitude_feet   = 25000
                        , cruise_mach           = 0.55
                        , exposedWingArea_ft2   = 144
                        , grossWingArea_ft2     = 144
                        , aspectRatio           = 9
                        , thicknessToChordRatio = 0.12
                        , maxTakeoffWeight_lb   = 6000
                        , sweep_deg             = 0
                        , zeroFuelWeight_lb     = 3000
                        , taperRatio            = 0.454
                        , n_ult                 = 3.64*1.5
                        , tTail                 = True
                        , horizTail = HorizTail { ht'ar = 6
                                                , ht'sHe_ft2 = 10
                                                , ht'sHg_ft2 = 10
                                                , ht'sweep_deg = 0
                                                , ht'tc = 0.12
                                                , ht'lH_ft = 18
                                                }
                        , vertTail = VertTail { vt'ar = 6
                                              , vt'sV_ft2 = 10
                                              , vt'sweep_deg = 0
                                              , vt'tc = 0.12
                                              }
                        }
