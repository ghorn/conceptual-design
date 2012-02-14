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
                        , zeroFuelWeightEst_lb  = 4500
                        , taperRatio            = 0.454
                        , n_ult                 = 3.64*1.5
                        , cabinPressAlt_ft      = 8000
                        , ceiling_ft            = 41000
                        , enginesDryWeight_lb   = 2*300
                        , electricalAndElectronics_lb = 300
                        , furnishings_lb        = 100
                        , structuralWeightFactor = 0.9
                        , numPax                = 5
                        , numCrew               = 1
                        , numFlightAttendants   = 0
                        , tTail                 = True
                        , allCargo              = False
                        , surfaceControl        = FullAerodynamic
                        , acType                = BusinessJet
                        , horizTail = HorizTail { ht'ar = 6
                                                , ht'sHe_ft2 = 30
                                                , ht'sHg_ft2 = 30
                                                , ht'sweep_deg = 0
                                                , ht'tc = 0.12
                                                , ht'lH_ft = 15
                                                }
                        , vertTail = VertTail { vt'ar = 2
                                              , vt'sV_ft2 = 20
                                              , vt'sweep_deg = 0
                                              , vt'tc = 0.12
                                              }
                        }
