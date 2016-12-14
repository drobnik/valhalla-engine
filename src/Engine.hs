module Engine where
-- TODO specify what to hide in these modules
import Render.Utils
import Render.WindowManager
import Engine.InputHandler
import Engine.Datas
import Engine.Timer
import Engine.Loader
import GameState --TEMP
import GameData
import Data.IORef
import Control.Monad
import GameState (GameState)
import qualified SDL
import qualified Data.Map as Map
-- http://hastebin.com/ezidakuhoq.hs == UPDATE!

-- moze zaleznosc engine do tefo?
data Engine = Engine
                { windowManager :: WindowManager
                , engineS :: IORef EngineState
                --, physics :: Physics
                --, update :: DeltaTime -> a -> a -- zmiana stanu gry
                }


sampleEngine :: IORef EngineState -> Engine
sampleEngine es = Engine {windowManager = sampleWinManager
                         , engineS = es
                         }

-- po callbacku podmieniaj stan silnika!
runEngine :: Engine -> IORef GameState -> IO ()
runEngine e@(Engine win eState) gs = do
  (window, renderer) <- initWin win
  loadGame renderer gs
  timer <- newIORef(initTimer)
  engineLoop gs eState window renderer timer
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

engineLoop ::IORef GameState -> IORef EngineState
           -> SDL.Window -> SDL.Renderer -> IORef Timer
           -> IO ()
engineLoop gs es win ren tim = do
  inputCallback es
  timer <- readIORef tim
  gameState <- readIORef gs
  eState <- readIORef es
  renderPipeline ren gameState
  ticks <- getTicks timer
  gameLoop es gs ((fromIntegral ticks) / 1000.0)

  restartTimer <- start timer
  writeIORef tim restartTimer
  unless (isOver eState) (engineLoop gs es win ren tim)
