module Render.Utils where

import Graphics.UI.GLUT
import Engine.Datas
import Render.Primitives
import GameState

data RenderPipeline = RenderPipeline

data Renderer = Renderer
                { mMode :: MatrixMode
                , clearC :: Color4 Float
                , gameState :: GameState
                , renderPipeline :: RenderPipeline
                }

instance Graphic Renderer where
  renderInit (Renderer m c _ _) = do
    clearColor $= c
    matrixMode $= m
    loadIdentity
    ortho 0 1 0 1 (-1) 1

  render (Renderer _ _ gs _) = do
    clear [ColorBuffer]
    -- renderPipeline
    -- iterate the actors and draw them
    flush

-- all graphics instructions in one place


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
                      , gameState = undefined
                      , renderPipeline = undefined}


{- class Drawable a where
  draw :: a -> IO () -- warto? -}
