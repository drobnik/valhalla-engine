module Render.Primitives where

import Graphics.UI.GLUT

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
