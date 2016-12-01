module Render.Model where

import Render.Primitives
import Engine.Consts
import Graphics.UI.GLUT
import Data.Map

-- wywalic rozowy z sampla
data RenderModel = RenderModel
                 { dim :: Dimensions
                 , pos :: CenterPosition --floor
                 --, texture :: Texture
                 , modelColor :: Color4 Float
                 , renderInstr :: [RenderCom]
                 } deriving (Show, Eq, Ord)

-- for now: render with default font
draw :: RenderModel -> [RenderCom]
draw (RenderModel _ _  _ render) = render

sampleSet :: Map Int RenderModel
sampleSet = insert 1 x $ insert 2 y $ insert 3 z $ sete
  where x = RenderModel
            { dim = tileDim
            , pos = pos1
            , modelColor = col1
            , renderInstr = sampleInstr tileDim pos1 col1
            }
        y = RenderModel
            { dim = tileDim
            , pos = pos2
            , modelColor = col2
            , renderInstr = sampleInstr tileDim pos2 col2
            }
        z = RenderModel
            { dim = tileDim
            , pos = pos3
            , modelColor = col3
            , renderInstr = sampleInstr tileDim pos3 col3
            }
        sete = empty

sampleInstr :: Dimensions -> CenterPosition -> Color4 Float -> [RenderCom]
sampleInstr dim pos color = [RenderColor color, (RenderRectangle dim pos)]
