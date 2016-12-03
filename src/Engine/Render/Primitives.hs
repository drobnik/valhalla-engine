module Render.Primitives where

import Graphics.Rendering.OpenGL

type Dimensions = (Float, Float)
type CenterPosition = (Float, Float)

-- rendering commnads for renderModel | trza do osobnego modulu przenisc jednak
data RenderCom = RenderRectangle Dimensions CenterPosition
               | RenderColor (Color4 Float)
               | RenderRotate Float -- angle
               | RenderTranslate (Float, Float)
               | RenderScale Float
               | RenderText String
               deriving (Show, Eq, Ord)
-- + renderWithCamera + alpha + texture

modifyPos :: [RenderCom] -> [RenderCom] -> CenterPosition -> [RenderCom]
modifyPos (x:xs) renAcc pos' = case x of
  RenderRectangle dim pos -> renAcc ++ [(RenderRectangle dim pos')] ++ xs
  _                       -> modifyPos xs (x:renAcc) pos'
modifyPos [] renAcc pos' = renAcc
