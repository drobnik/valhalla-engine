module Render.Utils where

import Control.Monad.ST
import SDL (($=))
import qualified SDL
--import SDL.Vect
import Engine.Datas
import Render.Primitives
import Engine.Consts
import Render.Model
import GameState


data ValRender = ValRender
                { renderer :: IO SDL.Renderer
                }

renderInit :: SDL.Window -> SDL.Renderer
renderInit win = do
    SDL.initialize [SDL.InitVideo]
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear

    renderer <- SDL.createRenderer window (-1) SDL.defaultRendererConfig
    SDL.rendererDrawColor renderer $= V4 0 0 0 0
    return renderer

renderPipeline :: SDL.Renderer -> GameState -> IO ()
renderPipeline ren (GameState _ _ models)  = do
    SDL.clear ren
    -- SDL.copy ren texture Nothing Nothing
    mapM_ (renderModel ren) (getModels models)
    SDL.present renderer

renderModel :: SDL.Renderer -> RenderModel -> IO ()
renderModel render x@(RenderModel) = do
  interpretComs render $ draw x

interpretComs :: SDL.Renderer -> [RenderCom] -> IO ()
interpretComs ren (x:xs) = do
  interpretCommand ren x
  interpretComs ren xs
interpretComs [] = return ()

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
