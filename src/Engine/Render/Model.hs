module Render.Model where

import Render.Primitives
import Engine.Consts
import Foreign.C.Types
import Data.Word
import qualified SDL (Texture, Rectangle(..))
import SDL.Vect --constr
import Data.Map
import qualified Debug.Trace as D

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


modifyModelPos :: RenderModel -> CenterPosition -> (Int, Int)
               -> Camera -> RenderModel
modifyModelPos (RenderModel d po path' tex col rend) (x', y')
  (width, height) (SDL.Rectangle (P(V2 camX camY)) _) =
  RenderModel{ dim = d, pos = pos', path = path'
                    , texture = tex
                    , modelColor = col
                    , renderInstr = modifyPos rend [] pos'
                    }
  where pos' = (withinMap (fromIntegral width) x' (fst d) camX
               , withinMap (fromIntegral height) y' (snd d) camY)
        withinMap con u d c
          | (u + d) >= (con - c) = (con - c - d)
          | u < 0 = 0
          | otherwise = u

checkOffset :: Camera -> Camera -> Camera
checkOffset (SDL.Rectangle (P(V2 camX camY)) (V2 cW cH))
  (SDL.Rectangle (P(V2 camX' camY')) _) =
  (SDL.Rectangle (P(V2 (camX' - camX) (camY' - camY))) (V2 cW cH))

calcCameraPosition :: Camera -> RenderModel -> (CInt, CInt) -> Camera
calcCameraPosition (SDL.Rectangle (P(V2 camX camY)) (V2 cW cH))
  (RenderModel (pWidth, pHeight) (pX, pY) _ _ _ _) (w, h) =
  let camX' = ((pX + (pWidth `div` 2)) - (viewWidth `div` 2))
      camY' = (pY + (pHeight `div`2)) - (viewHeight `div` 2)
      in SDL.Rectangle (P $ V2 (check camX' cW camX w)
                        (check camY' cH camY h)) (V2 cW cH)

check :: CInt -> CInt -> CInt -> CInt -> CInt
check cam dim oldCam con
  | cam < 0 = 0
  | (cam + dim) > con = oldCam
  | otherwise = cam

addCameraOffset :: RenderModel -> Camera -> RenderModel
addCameraOffset (RenderModel d pos'@(rX, rY) path' tex col ren)
  (SDL.Rectangle (P(V2 camX camY)) _) = RenderModel
                                        { dim = d
                                        , pos = pos'
                                        , path = path'
                                        , texture = tex
                                        , modelColor = col
                                        , renderInstr =
                                          modifyPos ren [] --pos
                                                        ((rX - camX), (rY - camY))
                                        }

--- moze wybuchnac
modifyPos :: [RenderCom] -> [RenderCom] -> CenterPosition -> [RenderCom]
modifyPos (x:xs) renAcc pos'@(xp, yp) = case x of
  RenderRectangle dim pos -> renAcc ++ [(RenderRectangle dim pos')] ++ xs
  RenderTexture tex pos -> renAcc ++ [(RenderTexture tex pos')] ++ xs

  RenderFrame tex rect (Just (SDL.Rectangle (P(V2 fx fy)) dims)) ->
                   renAcc ++ [(RenderFrame tex rect
                               (Just (SDL.Rectangle (P(V2 xp yp)) dims)))] ++ xs
  _                       -> modifyPos xs (x:renAcc) pos'
modifyPos [] renAcc pos' = renAcc

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
