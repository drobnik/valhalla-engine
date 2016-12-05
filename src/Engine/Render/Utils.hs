module Render.Utils where

import Control.Monad.ST
import Data.Map
import qualified SDL
import SDL.Vect
import SDL (($=))
import Engine.Datas
import Render.Primitives
import Engine.Consts
import Render.Model
import GameState
import Control.Concurrent

data ValRender = ValRender
                { renderer :: IO SDL.Renderer
                }

initSDL :: IO ()
initSDL = do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear

renderInit :: SDL.Window -> IO SDL.Renderer
renderInit win = do
    renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
    SDL.rendererDrawColor renderer $= V4 0 0 0 maxBound
    return renderer

renderPipeline :: SDL.Renderer -> GameState -> IO ()
renderPipeline ren gs = do
    SDL.clear ren
    -- SDL.copy ren texture Nothing Nothing
    mapM_ (renderModel ren) (getModelsSet gs)
    threadDelay 5000
    SDL.rendererDrawColor ren $= V4 10 10 10 255 --attention required
    SDL.present ren

renderModel :: SDL.Renderer -> RenderModel -> IO ()
renderModel render x = do
  interpretComs render $ draw x

interpretComs :: SDL.Renderer -> [RenderCom] -> IO ()
interpretComs ren (x:xs) = do
  interpretCommand ren x
  interpretComs ren xs
interpretComs ren [] = return ()

interpretCommand :: SDL.Renderer -> RenderCom -> IO ()
interpretCommand ren x = case x of
    RenderRectangle (w, h) (x1, y1) ->
      SDL.fillRect ren (Just $ SDL.Rectangle (P $ V2 x1 y1) (V2 w h))
    RenderColor colorF ->
      SDL.rendererDrawColor ren $= colorF
    RenderTexture texture ->
      SDL.copy ren texture Nothing Nothing

    RenderRotate angle -> undefined
    RenderTranslate (x, y) -> undefined
    RenderScale factor -> undefined
    RenderText text -> undefined
