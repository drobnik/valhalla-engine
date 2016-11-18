module Engine where

-- TODO specify what to hide in these modules
import Render.Utils
import Render.WindowManager
import Engine.InputHandler
import Engine.Datas as D

import Graphics.UI.GLUT

-- | Definitions of engine specific classes and datas, f.e.
-- | GameEngine, GameState and Renderer

-- | tutaj mozemy do tego przekazac stan gry -> idzie do inputa czy jak

-- http://hastebin.com/ezidakuhoq.hs == UPDATE!

data Engine a = Engine
                { windowManager :: WindowManager -- bedzie miec opcje inita, nazwa, etc
                , render :: Renderer
                , engineS :: D.EngineState -- przechowywanie i obliczanie dt
              --  , inputHandler :: InputHandler a -- wszelakie wydarzenia z zewnatrz
                --, loader :: Loader -- opcje ladowania swiata i assetow
                --, physics :: Physics
                --, update :: DeltaTime -> a -> a -- zmiana stanu gry
                }


sampleEngine :: Engine a
sampleEngine = Engine {windowManager = sampleWinManager
                      , render = initRender
                      , engineS = sampleState}

-- po callbacku podmieniaj stan silnika!
runEngine :: Engine a -> IO ()
runEngine (Engine win render eState) = do
  initW win render
  -- inputCallback
  displayCallback $= sillyDisplay
  mainLoop
-- data InputHandler a
-- data RenderPipeline -> jezeli bedziemy chcieli zrobic wiecej niz renderowanie tileso
-- class Updatable
-- class Drawable

-- + gameLoop for updating engine state and other stuff
