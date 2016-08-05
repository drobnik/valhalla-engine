module Timer where

import Data.Time.Clock
import FRP.Elerea.Simple
import Control.Monad
import Control.Concurrent

startTime :: Double
startTime = 0.0

-- | The timestep in seconds.
-- If the computer is too slow - we need to call
-- additional updates until we catch up
-- If it is not possible - panic mode or sth
targetElapsedTime :: Double
targetElapsedTime = 0.01666

-- | Maintaining the main game Signal
-- if it runs too fast -> delay the thread
-- how about passing the parameter to quit
-- the timer?
-- heavilyinspired by the elerea example
-- TODO: maybe add MaybeT for some problems
-- with performance?
driveNetwork :: IO (IO () ) -- ^ Signal IO ()
             -> IO ()
driveNetwork signal = {--runMaybeT--}forever $ do
  startTime <- getCurrentTime
  _         <- join signal
  endTime   <- getCurrentTime
  if elapsedTime startTime endTime < 0
    then putStrLn "Running slow.."
    else threadDelay $ truncate $ 1e6 * targetElapsedTime

-- | Calculate difference between two clocks
-- and convert it to double
elapsedTime :: UTCTime -> UTCTime -> Double
elapsedTime start end = timeStep - targetElapsedTime
  where timeStep = undefined
