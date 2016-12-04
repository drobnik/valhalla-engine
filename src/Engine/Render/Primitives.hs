module Render.Primitives where

import Foreign.C.Types
import SDL.Vect
import SDL (Texture)

import Data.Word

type Dimensions = (Float, Float)
type CenterPosition = (Float, Float)

-- rendering commnads for renderModel | trza do osobnego modulu przenisc jednak
data RenderCom = RenderRectangle Dimensions CenterPosition
              -- | RenderColor (V4 Word8)
               | RenderRotate Float -- angle
               | RenderTranslate (Float, Float)
               | RenderScale Float
               | RenderText String --TODO
               | RenderTexture Texture
              -- deriving (Eq, Ord)
-- + renderWithCamera + alpha + texture

--CLEANUP
modifyPos :: [RenderCom] -> [RenderCom] -> CenterPosition -> [RenderCom]
modifyPos (x:xs) renAcc pos' = case x of
  RenderRectangle dim pos -> renAcc ++ [(RenderRectangle dim pos')] ++ xs
  _                       -> modifyPos xs (x:renAcc) pos'
modifyPos [] renAcc pos' = renAcc
