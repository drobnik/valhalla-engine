module Render.Primitives where

import Graphics.UI.GLUT

data Primit = Rect | Circle

data RenderModel = RenderModel
                 { dimensions :: (Float, Float)
                 , position :: (Float, Float) --floor it; center
                 --, texture :: Texture
                 , primitive :: Primit
                 , color :: Color4 Float
                 }

-- for now: render with default font
renderModel :: RenderModel -> IO ()
renderModel (RenderModel (w, h) (x, y) prim color) =
  case prim of
    Rect -> renderRectangle (w, h) (x, y) color
    Circle -> undefined --TEMP

renderRectangle :: (Float, Float) -> (Float, Float) -> Color4 Float -> IO ()
renderRectangle (w, h) (x, y) color = undefined
