{-# LANGUAGE BangPatterns #-}

-- | Module defines types for graphic primitives used in the main
-- Renderer module 'Utils'.
module Render.Primitives
  (
    -- * Render Primitives
    RenderCom(..)
  , Texture(..)
  , Dimensions
  , LTPosition
    -- * Miscellaneous
  , noTexture
  )where

import Foreign.C.Types
import Data.Word

import SDL (Rectangle (..))
import qualified SDL (Texture)
import SDL.Vect

-- | Vector for 'RenderModel' dimensions.
type Dimensions = (CInt, CInt)

-- | Upper left point of a rectangle describing game entity position
type LTPosition = (CInt, CInt)

-- | Data which stores engine representation of a texture.
data Texture = Texture
             { tex  :: !SDL.Texture
             , size :: V2 CInt
             } deriving (Show, Eq, Ord)

-- | Definition for rendering actions. These operations are matched while performing
-- 'interpretCommand' function called from 'interpretComs'. While a proper pattern is
-- found, method calls a defined SDL action, later stored in 'SDL.Renderer' reference.
-- Operations can be combined together freely, allowing to apply several changes in one
-- drawing call.
data RenderCom = RenderRectangle Dimensions LTPosition
               | RenderColor (V4 Word8)
               | RenderRotate Float                    -- ^ Rotate by an angle
               | RenderTranslate (Float, Float)
               | RenderScale Float
               | RenderText String
               | RenderTexture Texture LTPosition
               | RenderFrame Texture (Maybe (Rectangle CInt)) -- ^ For animation
                 (Maybe (Rectangle CInt))
              deriving (Eq, Ord)

-- hacks, do not look
instance Ord SDL.Texture where
  compare t1 t2 = LT

instance Show SDL.Texture where
  show t = "some texture"

-- | Synonym for a disaster
noTexture :: Texture
noTexture = undefined
