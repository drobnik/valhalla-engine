module Engine.Datas where

import Data.IORef
import Data.Set (Set)
import Graphics.UI.GLUT

-- datas and types used in this engine

type ActiveKeys = IORef (Set Key)
type Direction = String

data EngineState = EngineState
                 { keys :: ActiveKeys
                 , dt :: Double -- last delta time
                 }
