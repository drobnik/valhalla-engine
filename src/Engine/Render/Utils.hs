{-# LANGUAGE BangPatterns #-}
module Render.Utils where

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
import qualified Debug.Trace as D
-- wczytywanie dla pierwszego jest przesuniete!
-- wczytalo tylko jedna linijke tekstur!

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
    -- add constraints on rendering unseen parts of lvl
    mapM_ (renderModel ren) (getTilesModels gs)
    mapM_ (renderModel ren) (getWorldModels gs)

    SDL.rendererDrawColor ren $= V4 10 10 10 255 --attention required
    SDL.present ren

renderModel :: SDL.Renderer -> RenderModel -> IO ()
renderModel render x = do
  interpretComs render $ (draw $! x)

interpretComs :: SDL.Renderer -> [RenderCom] -> IO ()
interpretComs ren (x:xs) = do
  interpretCommand ren x
  interpretComs ren xs
interpretComs ren [] = return $! ()

interpretCommand :: SDL.Renderer -> RenderCom -> IO ()
interpretCommand !ren !x = case x of
    RenderRectangle (w, h) (x1, y1) ->
      SDL.fillRect ren (Just $ SDL.Rectangle (P $ V2 x1 y1) (V2 w h))
    RenderColor colorF ->
      SDL.rendererDrawColor ren $= colorF
    RenderTexture (Texture texture (V2 w h)) (x, y) ->
      SDL.copy ren texture Nothing (Just (SDL.Rectangle (P $ V2 x y) (V2 w h)))
    RenderFrame (Texture texture _) sourceRec destiRect ->
      SDL.copy ren texture sourceRec destiRect

    RenderRotate angle -> undefined
    RenderTranslate (x, y) -> undefined
    RenderScale factor -> undefined
    RenderText text -> undefined
