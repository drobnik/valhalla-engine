{-# LANGUAGE BangPatterns, OverloadedStrings #-}
module Render.WindowManager where

import Render.Utils
import Engine.Consts
import qualified SDL
import SDL (($=))
import SDL.Vect
import Data.Int
import Data.Text
import Foreign.C.Types

-- dodac potem reportowanie z gluta -> reportErrors
-- | supports only one window for now, sry

-- dla inicjalizacji okna
class Initializable a where
  initWin :: a -> IO (SDL.Window, SDL.Renderer)

data WindowManager = WindowManager
                     { title :: Text
                     , winSize :: !(CInt, CInt)
                     , winPos :: !(CInt, CInt)
                     }

instance Initializable WindowManager where
  initWin (WindowManager t (w,h) (x,y)) = do
    initSDL
    window <- SDL.createWindow t
              SDL.defaultWindow{ SDL.windowInitialSize = V2 w h
                               , SDL.windowPosition = SDL.Centered
                               , SDL.windowResizable = True}

    SDL.showWindow window
    renderer <- renderInit window
    return (window, renderer)

initWinManager :: Text -> (CInt, CInt) -> (CInt, CInt) -> WindowManager
initWinManager title' size pos =
  WindowManager { title = title'
                , winSize = size
                , winPos = pos
                }

sampleWinManager :: WindowManager
sampleWinManager = initWinManager "Testing.." (viewWidth, viewHeight)
                   (posit, posit)
