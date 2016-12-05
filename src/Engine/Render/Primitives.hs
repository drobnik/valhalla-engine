module Render.Primitives where


import SDL (Texture)
import SDL.Vect
import Foreign.C.Types
import Data.Word

type Dimensions = (CInt, CInt)
type CenterPosition = (CInt, CInt)

-- maybe operations for the module?

data RenderCom = RenderRectangle Dimensions CenterPosition
               | RenderColor (V4 Word8)
               | RenderRotate Float -- angle
               | RenderTranslate (Float, Float)
               | RenderScale Float
               | RenderText String --TODO
               | RenderTexture Texture
              deriving (Eq, Ord, Show)
-- + renderWithCamera + alpha + texture

--TEMP hacks
instance Ord Texture where
  compare t1 t2 = LT

instance Show Texture where
  show t = "some texture"
