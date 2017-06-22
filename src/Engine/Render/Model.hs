{-# LANGUAGE BangPatterns #-}

-- | Module provides Rendering actions which depend only on graphic information
-- stored in RenderModel reference and the list of commands, not on game objects.
-- Provided operations include position manipulation, camera calculation
-- and graphic entites definitions.
module Render.Model
  (
    -- * RenderModel Management
    RenderModel(..)
  , draw
    -- * Position Management
  , modifyModelPos
  , modifyPos
  , renPos
    -- * Camera Management
  , Camera
  , addCameraOffset
  , calcCameraPosition
  , checkOffset
  , hasOffsetChanged
  , dummyModel
  ) where

import Foreign.C.Types
import Data.Word

import qualified SDL (Texture, Rectangle(..))
import SDL.Vect

import Render.Primitives
import Engine.Consts


-- | Definition of a camera which is only an abstract entity.
-- It stores the camera dimensions (full height and some part of level width)
-- and relative position.
type Camera = SDL.Rectangle CInt

-- | Graphic representation of a game entity. Store information useful only
-- for rendering purposes such as a list of rendering instructions, texture
-- reference or color. This record allows to seperate 'Renderer' module
-- from game logic.
data RenderModel = RenderModel
                 { dim         :: Dimensions
                 , pos         :: LTPosition -- ^ Upper left point of the model
                 , path        :: FilePath   -- ^ Path of loaded texture
                 , texture     :: Texture
                 , modelColor  :: V4 Word8   -- ^ Base color of the model

                 -- | List of rendering commands for the model
                 , renderInstr :: [RenderCom]
                 } deriving (Eq, Ord)

-- | Return a list of rendering commands to be rendered in 'Renderer'.
draw :: RenderModel -> [RenderCom]
draw (RenderModel _ _ _ _ _ render) = render

-- | Return top-left point of the model as its position.
renPos :: RenderModel -> LTPosition
renPos (RenderModel _ pos _ _ _ _) = pos

-- | Change the position of game entity's graphic representation.
-- Before doing that, check if it would stay within the level size
modifyModelPos :: RenderModel -> LTPosition -> (Int, Int)
               -> Camera -> RenderModel
modifyModelPos (RenderModel d po path' tex col rend) (x', y')
  (width, height) (SDL.Rectangle (P(V2 camX camY)) _) =
  RenderModel{ dim = d
             , pos = pos'
             , path = path'
             , texture = tex
             , modelColor = col
             , renderInstr = modifyPos rend [] pos'
             }
  where pos' = (withinMap (fromIntegral width) x' (fst d) camX
               , withinMap (fromIntegral height) y' (snd d) camY)
        withinMap con u d c
          | (u + d) >= (con - c) = (con - c - d)
          | u < 0 = 0
          | otherwise = u

-- | Rewrite rendering commands with newly calculated position.
modifyPos :: [RenderCom] -> [RenderCom] -> LTPosition -> [RenderCom]
modifyPos !(x:xs) !renAcc pos'@(xp, yp) = case x of
  RenderRectangle dim pos -> renAcc ++ [(RenderRectangle dim pos')] ++ xs
  RenderTexture tex pos -> renAcc ++ [(RenderTexture tex pos')] ++ xs

  RenderFrame tex rect (Just (SDL.Rectangle (P(V2 fx fy)) dims)) ->
                   renAcc ++ [(RenderFrame tex rect
                               (Just (SDL.Rectangle (P(V2 xp yp)) dims)))] ++ xs
  _                       -> modifyPos xs (x:renAcc) pos'
modifyPos [] renAcc pos' = renAcc

------------------------------------------------
--
--   Camera management functions
--
------------------------------------------------

-- | Check if camera relative position between frames has changed
hasOffsetChanged :: SDL.Rectangle CInt -> Bool
hasOffsetChanged (SDL.Rectangle (P (V2 x y)) _) = x /= 0 || y /= 0

-- | Calculate the relative distance between camera from previous
-- frame and the current one.
checkOffset :: Camera -> Camera -> Camera
checkOffset (SDL.Rectangle (P(V2 camX camY)) (V2 cW cH))
  (SDL.Rectangle (P(V2 camX' camY')) _) = SDL.Rectangle
                                          (P(V2 (camX' - camX)
                                             (camY' - camY)))
                                          (V2 cW cH)

-- | Calculate the new camera position. If 'RenderModel' of 'Player'
-- has exceeded half of the viewport, calculate the offset.
calcCameraPosition :: Camera -> RenderModel -> (CInt, CInt) -> Camera
calcCameraPosition (SDL.Rectangle (P(V2 camX camY)) (V2 cW cH))
  (RenderModel (pWidth, pHeight) (pX, pY) _ _ _ _) (w, h) =
  let camX' = ((pX + (pWidth `div` 2)) - (viewWidth `div` 2))
      camY' = (pY + (pHeight `div`2)) - (viewHeight `div` 2)
      in SDL.Rectangle (P $ V2 (check camX' cW camX w)
                        (check camY' cH camY h)) (V2 cW cH)

-- | Minor function to eliminate camera flickering.
check :: CInt -> CInt -> CInt -> CInt -> CInt
check cam dim oldCam con
  | cam < 0 = 0
  | (cam + dim) > con = oldCam
  | otherwise = cam

-- | Apply the camera offset to the model.
addCameraOffset :: RenderModel -> Camera -> RenderModel
addCameraOffset (RenderModel d pos'@(rX, rY) path' tex col ren)
  (SDL.Rectangle (P(V2 camX camY)) _) = RenderModel
                                        { dim = d
                                        , pos = pos'
                                        , path = path'
                                        , texture = tex
                                        , modelColor = col
                                        , renderInstr = modifyPos ren []
                                                        ((rX - camX)
                                                        , (rY - camY))
                                        }

------------------------------------------------
--  Miscellaneous
------------------------------------------------
-- | Return dummy rendering command to colour a rectangle.
sampleInstr :: Dimensions -> LTPosition -> V4 Word8 -> [RenderCom]
sampleInstr dim pos color = [RenderColor color, (RenderRectangle dim pos)]

-- | Return dummy model with sample rendering command.
dummyModel :: RenderModel
dummyModel = RenderModel
            { dim = tileDim
            , pos = (100, 80)
            , path = undefined
            , texture = undefined
            , modelColor = V4 0 0 0 255
            , renderInstr = sampleInstr tileDim (100, 80) (V4 255 0 255 255)
            }
