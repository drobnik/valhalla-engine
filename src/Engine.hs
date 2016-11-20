module Engine where

-- TODO specify what to hide in these modules
import Render.Utils
import Render.WindowManager
import Engine.InputHandler
import Engine.Datas as D
import Data.IORef
import GameState --TEMP

import Graphics.UI.GLUT

-- | Definitions of engine specific classes and datas, f.e.
-- | GameEngine, GameState and Renderer

-- | tutaj mozemy do tego przekazac stan gry -> idzie do inputa czy jak

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


sampleEngine :: Engine
sampleEngine = Engine {windowManager = sampleWinManager
                      , renderEngine = initRender
                      , engineS = undefined
                      }

-- po callbacku podmieniaj stan silnika!
runEngine :: Engine -> GameState -> IO () --(GState a) => Engine -> a -> IO ()
runEngine (Engine win ren eState) gameS = do
  enState <- newIORef (sampleState) --na razie enginestate jest useless
  initW win ren
  inputCallback enState
  displayCallback $= renderPipeline gameS
  mainLoop
-- data RenderPipeline -> jezeli bedziemy chcieli zrobic wiecej niz renderowanie tileso
-- class Updatable
-- class Drawable

-- + gameLoop for updating engine state and other stuff
