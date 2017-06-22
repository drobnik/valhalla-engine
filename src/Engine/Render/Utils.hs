{-# LANGUAGE BangPatterns #-}

-- | This module provides rendering utilities such as presenting
-- rendering actions in the window, SDL and Renderer context initialization
-- and rendering commands interpretation.
module Render.Utils where

import qualified SDL
import SDL.Vect
import SDL (($=))

import Engine.Consts
import Engine.Datas
import GameState
import Render.Model
import Render.Primitives


-- | Initialize SDL context. Required.
initSDL :: IO ()
initSDL = do
    SDL.initialize [SDL.InitVideo]                -- ^ Initialize only video context
    SDL.HintRenderScaleQuality $= SDL.ScaleLinear -- ^ Linear filtering on

-- | Return a default SDL renderer for 'Renderer' module.
renderInit :: SDL.Window -> IO SDL.Renderer
renderInit win = do
    renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
    SDL.rendererDrawColor renderer $= V4 0 0 0 maxBound
    return $! renderer

-- | Execute the rendering commands stored in 'RenderModel' references
-- and present result in the window.
renderPipeline :: SDL.Renderer -> GameState -> IO ()
renderPipeline ren gs = do
    SDL.clear ren

    mapM_ (renderModel ren) (getTilesModels gs)  -- ^ Render background first
    mapM_ (renderModel ren) (getWorldModels gs)  -- ^ Render game entities

    SDL.rendererDrawColor ren $= V4 10 10 10 255 -- ^ Background color
    SDL.present $! ren                           -- ^ Show the results

-- | Interpret rendering commands stored in 'RenderModel' reference.
-- This function is mapped onto list of 'RenderModel's.
renderModel :: SDL.Renderer -> RenderModel -> IO ()
renderModel render !x = do
  interpretComs render $ (draw x)

-- | Pipeline a list of rendering commands.
interpretComs :: SDL.Renderer -> [RenderCom] -> IO ()
interpretComs ren !(x:xs) = do
  interpretCommand ren x
  interpretComs ren xs
interpretComs ren [] = return $! ()

-- | Inner function for commands interpretation. Translate every command
-- into corresponding SDL Rendering action.
interpretCommand :: SDL.Renderer -> RenderCom -> IO ()
interpretCommand !ren !x = case x of
                             RenderRectangle (w, h) (x1, y1) -> SDL.fillRect ren
                                                                (Just $
                                                                 SDL.Rectangle
                                                                 (P $ V2 x1 y1)
                                                                 (V2 w h)
                                                                )
                             RenderColor colorF              -> SDL.rendererDrawColor
                                                                ren $= colorF
                             RenderTexture (Texture texture
                                            (V2 w h)) (x, y) -> SDL.copy ren
                                                                texture Nothing
                                                                (Just (SDL.Rectangle
                                                                       (P $ V2 x y)
                                                                       (V2 w h)
                                                                      )
                                                                )
                             RenderFrame (Texture texture _)
                               sourceRec destiRect            -> SDL.copy ren texture
                                                                 sourceRec destiRect

                             RenderRotate angle               -> undefined
                             RenderTranslate (x, y)           -> undefined
                             RenderScale factor               -> undefined
                             RenderText text                  -> undefined
