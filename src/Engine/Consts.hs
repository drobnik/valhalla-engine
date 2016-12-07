module Engine.Consts where

import Graphics.Rendering.OpenGL
import Data.Int
import Data.Word
import SDL.Vect
import Foreign.C.Types

-- plenty of useful, engine specific constants

viewHeight ::CInt
viewHeight = 400

viewWidth :: CInt
viewWidth = 600

posit :: CInt
posit = 100

startTime :: Double
startTime = 0.0

--TEMP
heroKey :: Int
heroKey = 3

tileSize :: Int32
tileSize = 32

-- | A fixed timestep. Used to indicate if some
-- additional updates are needed or frames need to be dropped; in milisec
targetElapsedTime :: Double
targetElapsedTime = 0.01666

msTargetElapsedTime :: Double
msTargetElapsedTime = targetElapsedTime * 1000

--TEMP
tileDim :: (CInt, CInt)
tileDim = (50, 50)

pos1 :: (CInt, CInt)
pos1 = (10, 10)

pos2 :: (CInt, CInt)
pos2 = (30, 340)

pos3 :: (CInt, CInt)
pos3 = (100, 80)

--TEMP! for square movement
un :: CInt
un = 10
-- move to render!
col1 :: V4 Word8
col1 = V4 255 255 255 255 --bial

col2 :: V4 Word8
col2 = V4 255 255 0 255 --zolty

col3 :: V4 Word8
col3 = V4 255 0 255 255 --roz
