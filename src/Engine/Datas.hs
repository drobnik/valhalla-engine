module Engine.Datas where

import Graphics.UI.GLUT
import Data.IORef
import Data.Set
import Render.Model
-- datas and types used in this engine

type ActiveKeys = Set Key
type PixOff = (Float, Float) -- x ; y

data EngineState = EngineState
                 { keys :: ActiveKeys
                 , dt :: Double -- last delta time
                 }

getKeys :: EngineState -> ActiveKeys
getKeys (EngineState keys _) = keys

sampleState :: EngineState
sampleState = EngineState
              { keys = empty :: Set Key
              , dt = 0.0
              }
