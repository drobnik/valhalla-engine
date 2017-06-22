-- | Module defines basic datas and types used in the engine.
-- Provides predicate for ending the game and sample 'EngineState' definition
module Engine.Datas where

import Data.IORef
import Data.Set
import Data.Int

import qualified SDL
import SDL (Point(P), V2(..))

import Foreign.C.Types
import Engine.Consts

import Render.Model

-- | Set of pressed keys
type ActiveKeys = Set SDL.Keycode

-- | Size and position of the main game window
type WinSetup = ((Int32, Int32), (Int32, Int32))

-- | Data which tores variables time step, active input
-- (in this case, currently pressed keys) and a flag for indicating
-- the end of the game.
data EngineState = EngineState
                 { keys     :: ActiveKeys
                 , dt       :: Double    -- ^ Last delta time
                 , over     :: Bool
                 , camera   :: Camera
                 , winSetup :: WinSetup
                 }

-- | Close the game by setting 'over' flag on.
closeGame :: IORef EngineState -> IO ()
closeGame es = do
  eState <- readIORef es
  writeIORef es (eState{ over = True })

-- | Predicate for ending the game. Listen directly to keyboard events to decide
-- to finish.
isOver :: EngineState -> Bool
isOver (EngineState keys _ over _ _) = over
                                       || (SDL.KeycodeEscape `elem` keys)
                                       || (SDL.KeycodeQ `elem` keys)

-- | Default 'EngineState' configuration
sampleState :: EngineState
sampleState = EngineState
              { keys = empty :: Set SDL.Keycode
              , dt = 0.0
              , over = False
              , camera = SDL.Rectangle (P $ V2 0 0) (V2 viewWidth viewHeight)
              }
