module Render.WindowManager where

import Render.Utils
import Engine.Consts
import qualified SDL
import Data.Int

-- dodac potem reportowanie z gluta -> reportErrors
-- | supports only one window for now, sry

-- dla inicjalizacji okna
class Initializable a where
  initW :: (Graphic b) => a -> b -> IO ()

data WindowManager = WindowManager
                     { title :: !String
                   , winSize  :: !Size --TODO MVar Size -> GlSizei == Int32
                   , winPos :: !Position
                   , displayMode :: ![DisplayMode]
                   , winStatus :: !WindowStatus --TODO: no glut + IORef / Hid
                   } -- + info about cursor

instance Initializable WindowManager where
  initW (WindowManager t siz pos dis _) ren = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= dis
    initialWindowSize $= siz
    initialWindowPosition $= pos
    _ <- createWindow t -- chyba nie mozna tak no!
    renderInit ren


--resizecallback

initWinManager :: String -> Size -> Position -> WindowManager
initWinManager title' size pos = -- TODO \(mode, status) ->
  WindowManager { title = title'
                , winSize = size
                , winPos = pos
                , displayMode = [SingleBuffered]
                , winStatus = Shown
                }

sampleWinManager :: WindowManager
sampleWinManager = initWinManager "Testing.." (Size viewWidth viewHeight)
                   (Position posit posit)
