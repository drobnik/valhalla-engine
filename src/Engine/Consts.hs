module Engine.Consts where

import Graphics.UI.GLUT
import Data.Int

-- plenty of useful, engine specific constants

viewHeight :: Int32
viewHeight = 400


--viewHeight = 400

--viewWidth =

viewWidth :: Int32
viewWidth = 600

posit :: Int32
posit = 100

startTime :: Double
startTime = 0.0

-- | A fixed timestep. Used to indicate if some
-- additional updates are needed or frames need to be dropped; in milisec
targetElapsedTime :: Double
targetElapsedTime = 0.01666

msTargetElapsedTime :: Double
msTargetElapsedTime = targetElapsedTime * 1000

--TEMP
tileDim :: (Float, Float)
tileDim = (50.0, 50.0)--(0.05, 0.05)

pos1 :: (Float, Float)
pos1 = (10.0, 10.0)--(0.20, 0.10)

pos2 :: (Float, Float)
pos2 = (30.0, 10.0)--(0.10, 0.40)

pos3 :: (Float, Float)
pos3 = (4.0, 20.0)--(0.0, 0.0)

-- move to render!
col1 :: Color4 Float
col1 = Color4 1.0 1.0 1.0 1.0 --bial

col2 :: Color4 Float
col2 = Color4 1.0 1.0 0.0 1.0 --zolty

col3 :: Color4 Float
col3 = Color4 1.0 0.0 1.0 1.0 --roz
