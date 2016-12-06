module Engine where

-- TODO specify what to hide in these modules
import Render.Utils
import Render.WindowManager
import Engine.InputHandler
import Engine.Datas
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
                 --, loader :: Loader -- opcje ladowania swiata i assetow
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
  loadGame renderer gs Map.empty
  engineLoop gs eState window renderer
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

engineLoop ::IORef GameState -> IORef EngineState
           -> SDL.Window -> SDL.Renderer -> IO ()
engineLoop gs es win ren = do
  inputCallback es
  gameState <- readIORef gs
  renderPipeline ren gameState
  eState <- readIORef es
  gameLoop es gs
  unless (isOver eState) (engineLoop gs es win ren)
