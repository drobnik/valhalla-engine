module Engine where

-- TODO specify what to hide in these modules
import Render.Utils
import Render.WindowManager
import Engine.InputHandler
import Engine.Datas as D
import Render.Model --TEMP
import GameState --TEMP
import GameData
import Data.IORef
import GameState (GameState)
import Graphics.Rendering.OpenGL

-- | Definitions of engine specific classes and datas, f.e.
-- | GameEngine, GameState and Renderer

-- http://hastebin.com/ezidakuhoq.hs == UPDATE!

-- moze zaleznosc engine do tefo?
data Engine = Engine
                { windowManager :: WindowManager
--                , renderEngine :: ValRender
                , engineS :: IORef D.EngineState
                 --, loader :: Loader -- opcje ladowania swiata i assetow
                --, physics :: Physics
                --, update :: DeltaTime -> a -> a -- zmiana stanu gry
                }


sampleEngine :: IORef D.EngineState -> Engine
sampleEngine es = Engine {windowManager = sampleWinManager
                         --, renderEngine = initRender
                         , engineS = es
                         }

-- chyba engineState tez IORef, bo trzeba nalozyc zmiane dt
gameUpdate :: IORef EngineState -> IORef GameState -> IO ()
gameUpdate es gs = do
  engineState <- readIORef es
  gameState <- readIORef gs
  let keys = getKeys engineState
      models = getModelsSet gameState
      model = getModelKey 3 models --roz
      pos = modelPosition keys (renPos model) --fakap
      --model' = modifyModelPos model pos
      -- modyfikuj mape i nadpisz ja
  return ()

-- po callbacku podmieniaj stan silnika!
runEngine :: Engine -> IORef GameState -> IO ()
runEngine e@(Engine win eState) gs = do
  (window, renderer) <- initWin win
  --inputCallback eState
  gameState <- readIORef gs --tutaj jest pieklo
  return ()
