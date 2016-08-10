module GameMain where

import Timer
import FRP.Elerea.Simple

-- | A sample game made with Valhalla engine
--

gameMain :: IO ()
gameMain = do
  gameSignal <- start $ do -- ^ initiate the game signal
    timer <- stateful startTime $ updateTimer
    return $ printTimer <$> timer

  driveNetwork gameSignal
