module Render.Utils where

import Graphics.UI.GLUT
import Engine.Datas
import Render.Primitives
import Render.Model
import GameState


class Graphic a where
  renderInit :: a -> IO ()
--  renderPipeline :: a -> IO ()

data Renderer = Renderer
                { mMode :: MatrixMode
                , clearC :: Color4 Float
                }

instance Graphic Renderer where
  renderInit (Renderer m c) = do
    clearColor $= c
    matrixMode $= m
    loadIdentity
    ortho 0 1 0 1 (-1) 1


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
      rect (Vertex2 x1 y1) (Vertex2 (x1+w) (y1+h))
    RenderColor colorF ->
      color colorF
    RenderRotate angle -> undefined
    RenderTranslate (x, y) -> undefined
    RenderScale factor -> undefined
    RenderText text -> undefined

 ------------------
initRender :: Renderer
initRender = Renderer { mMode = Projection
                      , clearC = Color4 0 0 0 0
                      }

sillyDisplay :: IO ()
sillyDisplay = do
    clear [ColorBuffer]
    flush
