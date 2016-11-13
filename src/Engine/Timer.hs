module Engine.Timer where
-- zaimportuj to tak, żeby widoczny byl jeden modul, tzn, zeby
-- hierarchia nie byla zbyt wysoka

import Data.Time.Clock

toMiliseconds :: Double -> Double
toMiliseconds s = floor $ s * 1000

getTime :: IO UTCTime
getTime = getCurrentTime >>= return utcDayTime

-- | Calculate difference between two clocks
-- and convert it to double; end == currentTime
elapsedTime :: UTCTime -> UTCTime -> Double --treated as seconds
elapsedTime start end = timeStep
  where timeStep = fromRational $ toRational $ diffUTCTime end start

regulateTimestep :: Double -> Double
regulateTimestep dt = case dt of
  | dt > targetElapsedTime = msTargetElapsedTime
  | otherwise = toMiliseconds dt
