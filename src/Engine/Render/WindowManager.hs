{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- | This module provides context for window management in application.
-- Built with 'SDL.Window' module store basic information such as
-- window title, size and position. 'WindowManager' record supports only
-- one window for now.
module Render.WindowManager
  (
    -- * Window Management
    WindowManager(..)
  , sampleWinManager
  , initWin
  , winInfo
  )where

import Data.Int
import Data.Text
import Foreign.C.Types

import qualified SDL
import SDL (($=))
import SDL.Vect

import Render.Utils
import Engine.Consts


class Initializable a where
  initWin :: a -> IO (SDL.Window, SDL.Renderer)

-- | Record which represents system window - a separate viewport on a
-- screen display. 'WindowManager' stores only a configuration for
-- created window but it can be extended for multiple window management.
data WindowManager = WindowManager
                     { title   :: Text          -- ^ Window title
                     , winSize :: !(CInt, CInt) -- ^ Window dimensions
                     , winPos  :: !(CInt, CInt) -- ^ Upper-left point coordinates
                     }

-- | Instance of manager initialization. Invoke SDL procedures for
-- window creation with dimensions provided in 'WindowManager' reference.
instance Initializable WindowManager where
  initWin (WindowManager t (w,h) (x,y)) = do
    initSDL
    window <- SDL.createWindow t
              SDL.defaultWindow{ SDL.windowInitialSize = V2 w h
                               , SDL.windowPosition    = SDL.Centered
                               }

    SDL.showWindow window
    renderer <- renderInit window
    return (window, renderer)

-- | Create a 'WindowManager' with given title and dimensions.'
initWinManager :: Text -> (CInt, CInt) -> (CInt, CInt) -> WindowManager
initWinManager title' size pos =
  WindowManager { title = title'
                , winSize = size
                , winPos = pos
                }

-- | Return information about window dimensions and position.
winInfo :: WindowManager -> ((Int32, Int32), (Int32, Int32))
winInfo (WindowManager _ (w, h) (x, y)) = ((fromIntegral w
                                           , fromIntegral h)
                                           , (0, 0))

-- | Default configuration for sample game.
sampleWinManager :: WindowManager
sampleWinManager = initWinManager "Most exciting game ever" (viewWidth, viewHeight)
                   (posit, posit)
