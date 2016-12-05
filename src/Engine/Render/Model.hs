module Render.Model where

import Render.Primitives
import Engine.Consts
import Data.Word
import SDL (Texture)
import SDL.Vect --constr
import Data.Map

-- wywalic rozowy z sampla
data RenderModel = RenderModel
                 { dim :: Dimensions
                 , pos :: CenterPosition --lewy-gorny POPRAWIC
                 , texture :: Texture
                 , modelColor :: V4 Word8
                 , renderInstr :: [RenderCom]
                 } deriving (Show, Eq, Ord)

-- for now: render with default font
draw :: RenderModel -> [RenderCom]
draw (RenderModel _ _ _  _ render) = render

renPos :: RenderModel -> CenterPosition
renPos (RenderModel _ pos _  _ _) = pos

modifyModelPos :: RenderModel -> CenterPosition -> RenderModel
modifyModelPos (RenderModel d po tex col rend) pos' = RenderModel
                                                   { dim = d
                                                   , pos = po
                                                   , texture = tex
                                                   , modelColor = col
                                                   , renderInstr = modifyPos rend [] pos'
                                                   }

sampleSet :: Map Int RenderModel
sampleSet = insert 1 x $ insert 2 y $ insert 3 z $ sete
  where x = RenderModel
            { dim = tileDim
            , pos = pos1
            , texture = undefined
            , modelColor = col1
            , renderInstr = sampleInstr tileDim pos1 col1
            }
        y = RenderModel
            { dim = tileDim
            , pos = pos2
            , texture = undefined
            , modelColor = col2
            , renderInstr = sampleInstr tileDim pos2 col2
            }
        z = RenderModel
            { dim = tileDim
            , pos = pos3
            , texture = undefined
            , modelColor = col3
            , renderInstr = sampleInstr tileDim pos3 col3
            }
        sete = empty

sampleInstr :: Dimensions -> CenterPosition -> V4 Word8 -> [RenderCom]
sampleInstr dim pos color = [RenderColor color, (RenderRectangle dim pos)]

dummyModel :: RenderModel
dummyModel = RenderModel
            { dim = tileDim
            , pos = pos3
            , texture = undefined
            , modelColor = V4 0 0 0 255
            , renderInstr = sampleInstr tileDim pos3 (V4 0 0 0 255)
            }
