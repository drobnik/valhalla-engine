module Render.Utils where
import Graphics.UI.GLUT as GL

data Renderer = Renderer
                { mMode :: MatrixMode
                , clearC :: Color
                }

instance Graphic Renderer where
  renderInit (Renderer m c) = do
    clearColor $= c
    matrixMode $= m
    loadIdentity
    ortho 0 1 0 1 (-1)1

  sillyDisplay = do
    clear [ColorBuffer]
    flush

class Graphic a where
  renderInit :: IO ()
  sillyDisplay :: IO ()
  -- draw :: gra -> IO ()
  -- drawPrmitives + textures + strings + clear Colors
  --

initRender :: Renderer
initRender = Renderer { mMode = Projection
                      , clearC = Color4 0 0 0 0 }
{- class Drawable a where
  draw :: a -> IO () -- warto? -}
