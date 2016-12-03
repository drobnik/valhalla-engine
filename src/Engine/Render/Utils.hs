module Render.Utils where

import Control.Monad.ST
import qualified SDL
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
    matrixMode $= m
    loadIdentity
    ortho 0 (realToFrac viewWidth) (realToFrac viewHeight) 0 (-1) 1


renderPipeline :: GameState -> IO ()
renderPipeline (GameState _ _ models)  = do
    clear [ColorBuffer]
    mapM_ renderModel (getModels models)
    flush

-- renderPipeline = (2)renderModels + (1)renderMap + (3)renderUI

renderModel :: RenderModel -> IO ()
renderModel x = interpretComs $ draw x

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


initRender :: Renderer
initRender = Renderer { mMode = Projection
                      , clearC = Color4 0 0 0 0
                      }
