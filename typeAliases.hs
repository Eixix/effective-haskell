module TypeAliases where

type Meters = Double

type Seconds = Double

type MetersPerSecond = Double

velocity :: Meters -> Seconds -> MetersPerSecond
velocity meters seconds = meters / seconds

speedLimit :: Double
speedLimit =
  let meters = 299792458 :: Double
      seconds = 1 :: Double
   in velocity meters seconds