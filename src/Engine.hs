module Engine where

-- TODO specify what to hide in these modules
import Render.Utils
import Render.WindowManager
import Engine.InputHandler
import Engine.Datas as D
import Data.IORef
import GameState (GameState)

import Graphics.UI.GLUT

-- | Definitions of engine specific classes and datas, f.e.
-- | GameEngine, GameState and Renderer

-- http://hastebin.com/ezidakuhoq.hs == UPDATE!

-- moze zaleznosc engine do tefo?
data Engine = Engine
                { windowManager :: WindowManager -- bedzie miec opcje inita, nazwa, etc
                , renderEngine :: Renderer
                , engineS :: IORef D.EngineState -- przechowywanie i obliczanie dt
                 --, loader :: Loader -- opcje ladowania swiata i assetow
                --, physics :: Physics
                --, update :: DeltaTime -> a -> a -- zmiana stanu gry
                }


sampleEngine :: IORef D.EngineState -> Engine
sampleEngine es = Engine {windowManager = sampleWinManager
                      , renderEngine = initRender
                      , engineS = es
                      }

-- chyba engineState tez IORef, bo trzeba nalozyc zmiane dt
gameUpdate :: IORef EngineState -> IORef GameState -> IO ()
gameUpdate es gs = do
  engineState <- readIORef es
  gameState <- readIORef es
  return ()

-- po callbacku podmieniaj stan silnika!
runEngine :: Engine -> IORef GameState -> IO ()
runEngine e@(Engine win ren eState) gs = do
  initW win ren
  inputCallback eState
  gameState <- readIORef gs --tutaj jest pieklo
  displayCallback $= renderPipeline gameState
  reshapeCallback $= Just reshape
  gameUpdate eState gs
  mainLoop
