module Engine.Consts where

-- plenty of useful, engine specific constants

startTime :: Double
startTime = 0.0

-- | A fixed timestep. Used to indicate if some
-- additional updates are needed or frames need to be dropped; in milisec
targetElapsedTime :: Double
targetElapsedTime = 0.01666 -- !! do not use `==` for god's sake

msTargetElapsedTime :: Double
msTargetElapsedTime = targetElapsedTime * 1000
