module Engine.Datas where

import qualified SDL
import SDL (V2(..), Point(P))
import Data.IORef
import Data.Set
import Render.Model
import Foreign.C.Types
import Engine.Consts

-- datas and types used in this engine

type ActiveKeys = Set SDL.Keycode
type PixOff = (CInt, CInt) -- x ; y

data EngineState = EngineState
                 { keys :: ActiveKeys
                 , camera :: Camera
                 , dt :: Double -- last delta time
                 , over :: Bool
                 }

getKeys :: EngineState -> ActiveKeys
getKeys (EngineState keys _ _ _) = keys

getCamera :: EngineState -> Camera
getCamera (EngineState _ cam _ _) = cam

closeGame :: IORef EngineState -> IO ()
closeGame es = do
  eState <- readIORef es
  writeIORef es (eState{ over = True })

--TEMP
isOver :: EngineState -> Bool
isOver (EngineState keys _ _ over) = over || (SDL.KeycodeEscape `elem` keys) ||
                              (SDL.KeycodeQ `elem` keys)
setCamera :: Camera
setCamera = SDL.Rectangle (P $ V2 0 0) (V2 viewWidth viewHeight)

sampleState :: EngineState
sampleState = EngineState
              { keys = empty :: Set SDL.Keycode
              , camera = setCamera
              , dt = 0.0
              , over = False
              }
