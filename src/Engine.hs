module Engine where

import Render.Utils as R
import Render.WindowManager as W

import Graphics.UI.GLUT

-- | Definitions of engine specific classes and datas, f.e.
-- | GameEngine, GameState and Renderer

-- | tutaj mozemy do tego przekazac stan gry -> idzie do inputa czy jak

-- http://hastebin.com/ezidakuhoq.hs == UPDATE!

data Engine a = Engine
                { windowManager :: WindowManager -- bedzie miec opcje inita, nazwa, etc
                , render :: Renderer
                --, engineS :: EngineState -- przechowywanie i obliczanie dt
                --, inputHandler :: InputHandler a -- wszelakie wydarzenia z zewnatrz
                --, loader :: Loader -- opcje ladowania swiata i assetow
                --, physics :: Physics
                --, update :: DeltaTime -> a -> a -- zmiana stanu gry
                }
sampleEngine :: Engine a
sampleEngine = Engine {windowManager = W.sampleWinManager, render = R.initRender}

runEngine :: Engine a -> IO ()
runEngine (Engine win render) = do
  W.initW win render
  displayCallback $= R.sillyDisplay
  mainLoop
-- data InputHandler a
-- data RenderPipeline -> jezeli bedziemy chcieli zrobic wiecej niz renderowanie tileso
-- class Updatable
-- class Drawable

-- + gameLoop for updating engine state and other stuff
