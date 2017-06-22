-- | Module which defines sample constants for rendering,
-- tile maps and game logic. It is quite mixed for now, should
-- be cleaned up in the future.
module Engine.Consts where

import Data.Int
import Data.Word
import Foreign.C.Types

import SDL.Vect


viewHeight ::CInt
viewHeight = 480

viewWidth :: CInt
viewWidth = 640

-- | Window initial position
posit :: CInt
posit = 100

startTime :: Double
startTime = 0.0

-- | A fixed timestep.
targetElapsedTime :: Double
targetElapsedTime = 0.01666

msTargetElapsedTime :: Double
msTargetElapsedTime = targetElapsedTime * 1000

---------------------------------------------
--
-- Game specific constants
--
---------------------------------------------
tileDim :: (CInt, CInt)
tileDim = (32, 32)

tileSize :: Int32
tileSize = 32

tileSInt :: Int
tileSInt = fromIntegral tileSize

pVelo :: Double
pVelo = 1040

---------------------------------------------
--
-- Collision module constants
--
---------------------------------------------
maxObj :: Int
maxObj = 10

maxLvl :: Int
maxLvl = 5
