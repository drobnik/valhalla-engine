-- | 'Engine' module provides two methods - 'runEngine' and 'engineLoop'.
-- First is responsible for initialization of a main loop of the game engine,
-- second provides definition for default configuration.
module Engine
  ( -- * Engine loop
    sampleEngine
  , runEngine
  ) where

import Data.IORef
import Control.Monad
import qualified Data.Map as Map

import qualified SDL

import Engine.Datas
import Engine.InputHandler
import Engine.Timer
import Engine.Loader
import GameState (GameState)
import Render.Utils
import Render.WindowManager
import GameState
import GameData

-- | Data which stores information about EngineState and 'Renderer' module
data Engine = Engine
              { windowManager :: WindowManager -- ^ Window manager for the game
              , engineS       :: IORef EngineState -- ^ Mutable reference for a state
              }

-- | Default configuration for the engine.
sampleEngine :: IORef EngineState -> Engine
sampleEngine es = Engine
                 { windowManager = sampleWinManager
                 , engineS = es
                 }

-- | The core function of valhalla engine. Trigger an initialization phase
-- of a window and SDL renderer context. Create timer and pass a GameState
-- reference to 'engineLoop'.
runEngine :: Engine -> IORef GameState -> IO ()
runEngine e@(Engine win eState) gs = do
  (window, renderer) <- initWin win
  eState' <- readIORef eState
  writeIORef eState (eState'{winSetup = winInfo win})
  loadGame renderer gs
  timer <- newIORef(initTimer)
  engineLoop gs eState window renderer timer
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

-- | True loop in this engine. Update the state of the engine between frames
-- in input handling, rendering and calculating delta time. A conditional execution
-- of expressions in 'engineLoop' is defined with 'unless' function.
-- It takes a predicate “return true if the engine state has noticed exit event from
-- input or game has ended” and a function which is executed until
-- the predicate evaluates to 'False'.
engineLoop :: IORef GameState
           -> IORef EngineState
           -> SDL.Window
           -> SDL.Renderer
           -> IORef Timer        -- ^ Mutable reference to store calculated ticks
           -> IO ()
engineLoop gs es win ren tim = do
  -- | Detect any input using SDL callbacks
  inputCallback es
  timer     <- readIORef tim   -- ^ Read old timer
  gameState <- readIORef gs
  eState    <- readIORef es
  renderPipeline ren gameState -- ^ Draw level in the window
  ticks     <- getTicks timer  -- ^ Get how many milisecs have passed
   -- | Update the game state and pass calculated delta time
  gameLoop es gs ((fromIntegral ticks) / 1000.0)
   -- | Retart timer for this frame
  restartTimer <- start timer
  writeIORef tim restartTimer
  -- | Continue the loop until the game is over
  unless (isOver eState) (engineLoop gs es win ren tim)
