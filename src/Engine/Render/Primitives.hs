{-# LANGUAGE BangPatterns #-}
module Render.Primitives where


import SDL (Rectangle (..))
import qualified SDL (Texture)
import SDL.Vect
import Foreign.C.Types
import Data.Word

type Dimensions = (CInt, CInt)

-- | Upper left point of a rectangle describing game entity position
type LTPosition = (CInt, CInt)

data Texture = Texture
             { tex :: !SDL.Texture
             , size :: V2 CInt
             } deriving (Show, Eq, Ord)

noTexture :: Texture
noTexture = undefined

data RenderCom = RenderRectangle Dimensions LTPosition
               | RenderColor (V4 Word8)
               | RenderRotate Float -- angle
               | RenderTranslate (Float, Float)
               | RenderScale Float
               | RenderText String --TODO
               | RenderTexture Texture LTPosition
               | RenderFrame Texture (Maybe (Rectangle CInt)) --add info about frames!
                 (Maybe (Rectangle CInt))
              deriving (Eq, Ord, Show)
-- + renderWithCamera + alpha + texture

--TEMP hacks
instance Ord SDL.Texture where
  compare t1 t2 = LT

instance Show SDL.Texture where
  show t = "some texture"
