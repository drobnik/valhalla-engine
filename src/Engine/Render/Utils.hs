module Render.Utils where

import Graphics.UI.GLUT
import Engine.Datas
import Render.Primitives
import Engine.Consts
import Render.Model
import GameState


class Graphic a where
  renderInit :: a -> IO ()
--  renderPipeline :: a -> IO ()
-- viewport -> camera!
data Renderer = Renderer
                { mMode :: MatrixMode
                , clearC :: Color4 Float
                }

instance Graphic Renderer where
  renderInit (Renderer m c) = do
    clearColor $= c
--    viewport (Position 10 30) (Size winHeight winWidth)
    matrixMode $= m
    loadIdentity
    ortho 0 (realToFrac viewWidth) (realToFrac viewHeight) 0 (-1) 1


renderPipeline :: GameState -> IO ()
renderPipeline gs = do
    clear [ColorBuffer]
    renderModels $ listOfModels gs
    flush

-- renderPipeline = renderModels + renderMap + renderUI

renderModels :: [RenderModel] -> IO ()
renderModels (x:xs) = do
  interpretComs $ draw x
  renderModels xs
renderModels [] = return ()


interpretComs :: [RenderCom] -> IO ()
interpretComs (x:xs) = do
  interpretCommand x
  interpretComs xs
interpretComs [] = return ()

interpretCommand :: RenderCom -> IO ()
interpretCommand x = case x of
    RenderRectangle (w, h) (x1, y1) ->
      rect (Vertex2 x1 y1) (Vertex2 (w + x1) (h + y1))
    RenderColor colorF ->
      color colorF
    RenderRotate angle -> undefined
    RenderTranslate (x, y) -> undefined
    RenderScale factor -> undefined
    RenderText text -> undefined

-- later - add a position of player for viewport
reshape :: ReshapeCallback
reshape siz = do
  viewport $= ((Position 0 0), (Size viewWidth viewHeight))
  postRedisplay Nothing

 ------------------
initRender :: Renderer
initRender = Renderer { mMode = Projection
                      , clearC = Color4 0 0 0 0
                      }

sillyDisplay :: IO ()
sillyDisplay = do
    clear [ColorBuffer]
    flush
