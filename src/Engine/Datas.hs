module Engine.Datas where

import Graphics.UI.GLUT
import Data.IORef
import Data.Set
-- datas and types used in this engine

type ActiveKeys = Set Key
type Direction = String

data EngineState = EngineState
                 { keys :: ActiveKeys
                 , dt :: Double -- last delta time
                 }

sampleState :: EngineState
sampleState = EngineState
              { keys = empty :: Set Key
              , dt = 0.0
              }
