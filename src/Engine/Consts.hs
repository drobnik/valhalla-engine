module Engine.Consts where

import Graphics.UI.GLUT
-- plenty of useful, engine specific constants

startTime :: Double
startTime = 0.0

-- | A fixed timestep. Used to indicate if some
-- additional updates are needed or frames need to be dropped; in milisec
targetElapsedTime :: Double
targetElapsedTime = 0.01666 -- !! do not use `==` for god's sake

msTargetElapsedTime :: Double
msTargetElapsedTime = targetElapsedTime * 1000

--TEMP
tileDim :: (Float, Float)
tileDim = (25.0, 25.0)

pos1 :: (Float, Float)
pos1 = (50.0, 40.0)

pos2 :: (Float, Float)
pos2 = (10.0, 20.0)

pos3 :: (Float, Float)
pos3 = (100.0, 100.0)

-- move to render!
col1 :: Color4 Float
col1 = Color4 1.0 1.0 1.0 1.0

col2 :: Color4 Float
col2 = Color4 1.0 0.0 0.5 1.0

col3 :: Color4 Float
col3 = Color4 1.0 1.0 1.0 1.0
