module Render.Model where

import Render.Primitives
import Engine.Consts
import Foreign.C.Types
import Data.Word
import qualified SDL (Texture, Rectangle(..))
import SDL (V2(..), Point(P))
import SDL.Vect --constr
import Data.Map

type Camera = SDL.Rectangle CInt
-- wywalic rozowy z sampla
data RenderModel = RenderModel
                 { dim :: Dimensions
                 , pos :: CenterPosition --lewy-gorny POPRAWIC
                 , path :: FilePath
                 , texture :: Texture
                 , modelColor :: V4 Word8
                 , renderInstr :: [RenderCom]
                 } deriving (Eq, Ord)

instance Show RenderModel where
  show (RenderModel _ (x,y) _ _ _ renderI) = "\nmodel: x:" ++ show x ++ ", y:"
                                                 ++ show y ++ "render:" ++ show renderI
-- for now: render with default font
draw :: RenderModel -> [RenderCom]
draw (RenderModel _ _ _ _ _ render) = render

renPos :: RenderModel -> CenterPosition
renPos (RenderModel _ pos _ _ _ _) = pos

renDim :: RenderModel -> Dimensions
renDim (RenderModel dim _  _ _ _ _) = dim

modifyModelPos :: RenderModel -> CenterPosition -> Camera -> RenderModel
modifyModelPos (RenderModel d po path' tex col rend) pos'@(cX, cY)
  (SDL.Rectangle (P(V2 camX camY)) _ ) = RenderModel
                                         { dim = d
                                         , pos = pos'
                                         , path = path'
                                         , texture = tex
                                         , modelColor = col
                                         , renderInstr =
                                             modifyPos rend []
                                             (cX - camX, cY - camY )
                                         }

modifyPos :: [RenderCom] -> [RenderCom] -> CenterPosition -> [RenderCom]
modifyPos (x:xs) renAcc pos' = case x of
  RenderRectangle dim pos -> renAcc ++ [(RenderRectangle dim pos')] ++ xs
  _                       -> modifyPos xs (x:renAcc) pos'
modifyPos [] renAcc pos' = renAcc


sampleSet :: Map Int RenderModel
sampleSet = insert 1 x $ insert 2 y $ insert 3 z $ sete
  where x = RenderModel
            { dim = tileDim
            , pos = pos1
            , path = []
            , texture = undefined
            , modelColor = col1
            , renderInstr = sampleInstr tileDim pos1 col1
            }
        y = RenderModel
            { dim = tileDim
            , pos = pos2
            , path = []
            , texture = undefined
            , modelColor = col2
            , renderInstr = sampleInstr tileDim pos2 col2
            }
        z = RenderModel
            { dim = tileDim
            , pos = pos3
            , path = []
            , texture = undefined
            , modelColor = col3
            , renderInstr = sampleInstr tileDim pos3 (V4 0 10 100 255)
            }
        pi = RenderModel
             { dim = (128, 32)
             , pos = (200, 50)
             , path = "example_data/tiles.bmp"
             , texture = noTexture
             , modelColor = col2
             , renderInstr = []
             }
        sete = empty


-- TEMP
sampleInstr :: Dimensions -> CenterPosition -> V4 Word8 -> [RenderCom]
sampleInstr dim pos color = [RenderColor color, (RenderRectangle dim pos)]

dummyModel :: RenderModel
dummyModel = RenderModel
            { dim = tileDim
            , pos = pos3
            , path = undefined
            , texture = undefined
            , modelColor = V4 0 0 0 255
            , renderInstr = sampleInstr tileDim pos3 col3
            }
