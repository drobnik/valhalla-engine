module Render.Utils where

import Graphics.UI.GLUT
import Engine.Datas
import Render.Primitives
import GameState

data Renderer = Renderer
                { mMode :: MatrixMode
                , clearC :: Color4 Float
                , gameState :: GameState
                }

instance Graphic Renderer where

  renderInit (Renderer m c _) = do
    clearColor $= c
    matrixMode $= m
    loadIdentity
    ortho 0 1 0 1 (-1) 1

-- render pipeline
  render (Renderer _ _ gs) = do
    clear [ColorBuffer]
    -- renderPipeline
    -- iterate the actors and draw them
    flush

sillyDisplay :: IO ()
sillyDisplay = do
    clear [ColorBuffer]
    flush

class Graphic a where
  renderInit :: a -> IO ()
  render :: a -> IO () --gameState
  -- drawPrmitives + textures + strings + clear Colors
  --

initRender :: Renderer
initRender = Renderer { mMode = Projection
                      , clearC = Color4 0 0 0 0
                      , gameState = undefined}


{- class Drawable a where
  draw :: a -> IO () -- warto? -}
