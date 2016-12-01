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
    -- make viewport centered TMP
--    viewport $= (0 0winWidth winHeight))
    matrixMode $= m
    loadIdentity
    ortho 0 (realToFrac winWidth) (realToFrac winHeight) 0 (-1) 1


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

{-reshape :: ReshapeCallback
reshape siz = do
  let pX' = (winWidth / 2)
  viewport $= (Position 0 0, siz)
  postRedisplay Nothing
-}
 ------------------
initRender :: Renderer
initRender = Renderer { mMode = Projection
                      , clearC = Color4 0 0 0 0
                      }

sillyDisplay :: IO ()
sillyDisplay = do
    clear [ColorBuffer]
    flush
