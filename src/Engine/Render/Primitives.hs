module Render.Primitives where


import SDL (Texture)
import SDL.Vect
import Foreign.C.Types
import Data.Word

type Dimensions = (CInt, CInt)
type CenterPosition = (CInt, CInt)

-- rendering commnads for renderModel | trza do osobnego modulu przenisc jednak
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
--CLEANUP
modifyPos :: [RenderCom] -> [RenderCom] -> CenterPosition -> [RenderCom]
modifyPos (x:xs) renAcc pos' = case x of
  RenderRectangle dim pos -> renAcc ++ [(RenderRectangle dim pos')] ++ xs
  _                       -> modifyPos xs (x:renAcc) pos'
modifyPos [] renAcc pos' = renAcc
