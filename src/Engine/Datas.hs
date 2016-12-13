module Engine.Datas where

import qualified SDL
import SDL (Point(P), V2(..))
import Data.IORef
import Data.Set
import Data.Int
import Render.Model
import Foreign.C.Types
import Engine.Consts
-- datas and types used in this engine

type ActiveKeys = Set SDL.Keycode
type PixOff = (CInt, CInt) -- x ; y

data EngineState = EngineState
                 { keys :: ActiveKeys
                 , dt :: Double -- last delta time
                 , over :: Bool
                 , camera :: Camera
                 }

getKeys :: EngineState -> ActiveKeys
getKeys (EngineState keys _ _ _) = keys

getCamera :: EngineState -> Camera
getCamera (EngineState _ _ _ cam) = cam

closeGame :: IORef EngineState -> IO ()
closeGame es = do
  eState <- readIORef es
  writeIORef es (eState{ over = True })

--TEMP
isOver :: EngineState -> Bool
isOver (EngineState keys _ over _) = over || (SDL.KeycodeEscape `elem` keys) ||
                              (SDL.KeycodeQ `elem` keys)

sampleState :: EngineState
sampleState = EngineState
              { keys = empty :: Set SDL.Keycode
              , dt = 0.0
              , over = False
              , camera = SDL.Rectangle (P $ V2 0 0) (V2 viewWidth viewHeight)
              }
