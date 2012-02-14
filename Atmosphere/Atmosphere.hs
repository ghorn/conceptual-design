{-# OPTIONS_GHC -Wall #-}

module Atmosphere.Atmosphere( siAtmosphere
                            , usAtmosphere
                            ) where

-- 1976 US Standard Atmosphere
--  Adapted by
--      Greg Horn
--  Adapted by
--      Richard J. Kwan, Lightsaber Computing
--  from original programs by
--      Ralph L. Carmichael, Public Domain Aeronautical Software
--  
--  Revision History
--  Date             Vers Person Statement of Changes
--  2004 Oct 04  1.0  RJK    Initial program


-- P H Y S I C A L   C O N S T A N T S
_FT2METERS :: (Ord a, Floating a) => a
_KELVIN2RANKINE :: (Ord a, Floating a) => a
_PSF2NSM :: (Ord a, Floating a) => a
_SCF2KCM :: (Ord a, Floating a) => a
_TZERO :: (Ord a, Floating a) => a
_PZERO :: (Ord a, Floating a) => a
_RHOZERO :: (Ord a, Floating a) => a
_AZERO :: (Ord a, Floating a) => a
_BETAVISC :: (Ord a, Floating a) => a
_SUTH :: (Ord a, Floating a) => a


_FT2METERS = 0.3048              -- mult. ft. to get meters (exact)
_KELVIN2RANKINE = 1.8            -- mult deg K to get deg R
_PSF2NSM = 47.880258             -- mult lb/sq.ft to get sq.m
_SCF2KCM = 515.379               -- mult slugs/cu.ft to get kg/cu.m
_TZERO   = 288.15                -- sea-level temperature, kelvins
_PZERO   = 101325.0              -- sea-level pressure, N/sq.m
_RHOZERO = 1.225                 -- sea-level density, kg/cu.m
_AZERO   = 340.294               -- speed of sound at S.L.  m/sec
_BETAVISC = 1.458E-6             -- viscosity constant
_SUTH    = 110.4                 -- Sutherland's constant, kelvins


{-
  input: 
altitude in ft

  outputs:
temperature in Rankine
pressure in lb/ft^2
density in slugs/ft^3
speed of sound in ft/sec
viscosity in slugs/(ft-sec)
kinematic viscosity in ft^2/s
-}
usAtmosphere :: (Floating a, Ord a) => a -> (a,a,a,a,a,a)
usAtmosphere alt_ft = (temp, pressure, density, asound, viscosity, kinematicViscosity)
  where
    alt_km = _FT2METERS*0.001*alt_ft
    (sigma, delta, theta) = simpleAtmosphere alt_km
    temp = _KELVIN2RANKINE*_TZERO*theta
    pressure = _PZERO*delta/47.88
    density = _RHOZERO*sigma/515.379
    asound = (_AZERO/_FT2METERS)*sqrt(theta)
    viscosity=(1.0/_PSF2NSM)*metricViscosity(theta)
    kinematicViscosity = viscosity/density

siAtmosphere :: (Floating a, Ord a) => a -> (a,a,a,a,a,a)
siAtmosphere alt_m = (temp, pressure, density, asound, viscosity, kinematicViscosity)
  where
    alt_km = 0.001*alt_m
    (sigma, delta, theta) = simpleAtmosphere alt_km
    temp = _TZERO * theta
    pressure = _PZERO * delta
    density = _RHOZERO * sigma
    asound = _AZERO * sqrt(theta)
    viscosity = metricViscosity theta
    kinematicViscosity = viscosity/density

simpleAtmosphere :: (Floating a, Ord a) => a -> (a,a,a)
simpleAtmosphere alt = (sigma, delta, theta)
  where
{-  Compute temperature, density, and pressure in simplified
    standard atmosphere.

    Correct to 20 km.  Only approximate thereafter.

    Input:
        alt     geometric altitude, km.
    Return: (sigma, delta, theta)
        sigma   density/sea-level standard density
        delta   pressure/sea-level standard pressure
        theta   temperature/sea-level std. temperature
-}
    _REARTH = 6369.0             -- radius of the Earth (km)
    _GMR = 34.163195             -- gas constant

    h = alt*_REARTH/(alt+_REARTH) -- geometric to geopotential altitude

    (theta, delta)
      -- troposphere
      | h < 11.0 = ( (288.15 - 6.5*h)/288.15, theta**(_GMR/6.5) )
      -- stratosphere
      | h < 20.0 = (216.65/288.15, 0.2233611*exp(-_GMR*(h-11.0)/216.65))
      | otherwise = error "simpleAtmosphere invalid higher than 20 km"
    sigma = delta/theta

metricViscosity :: (Floating a, Ord a) => a -> a
metricViscosity theta = _BETAVISC*sqrt(t*t*t)/(t+_SUTH)
  where
    t = theta * _TZERO
