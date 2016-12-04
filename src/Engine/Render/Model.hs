module Render.Model where

import Render.Primitives
import Engine.Consts
import Data.Word
import Graphics.Rendering.OpenGL
import SDL
--import SDL.Vect
import Data.Map

-- wywalic rozowy z sampla
data RenderModel = RenderModel
                 { dim :: Dimensions
                 , pos :: CenterPosition --lewy-gorny POPRAWIC
                 --, texture :: Texture
                 , modelColor :: V4 Word8
                 , renderInstr :: [RenderCom]
                 } deriving (Show, Eq, Ord)

-- for now: render with default font
draw :: RenderModel -> [RenderCom]
draw (RenderModel _ _  _ render) = render

renPos :: RenderModel -> CenterPosition
renPos (RenderModel _ pos _ _) = pos

modifyModelPos :: RenderModel -> CenterPosition -> RenderModel
modifyModelPos (RenderModel d po col rend) pos' = RenderModel
                                                   { dim = d
                                                   , pos = po
                                                   , modelColor = col
                                                   , renderInstr = modifyPos rend [] pos'
                                                   }

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

dummyModel :: RenderModel
dummyModel = RenderModel
            { dim = tileDim
            , pos = pos3
            , modelColor = Color4 0.2 0.2 0.2 1.0
            , renderInstr = sampleInstr tileDim pos3 col3
            }
