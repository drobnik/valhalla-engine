module Engine.Datas where

import qualified SDL
import Data.IORef
import Data.Set
import Render.Model
import Foreign.C.Types
-- datas and types used in this engine

type ActiveKeys = Set SDL.Keycode
type PixOff = (CInt, CInt) -- x ; y

data EngineState = EngineState
                 { keys :: ActiveKeys
                 , dt :: Double -- last delta time
                 , over :: Bool
                 }

getKeys :: EngineState -> ActiveKeys
getKeys (EngineState keys _ _) = keys

closeGame :: IORef EngineState -> IO ()
closeGame es = do
  eState <- readIORef es
  writeIORef es (eState{ over = True })

--TEMP
isOver :: EngineState -> Bool
isOver (EngineState keys _ over) = over || (SDL.KeycodeEscape `elem` keys) ||
                              (SDL.KeycodeQ `elem` keys)

sampleState :: EngineState
sampleState = EngineState
              { keys = empty :: Set SDL.Keycode
              , dt = 0.0
              , over = False
              }
