module Engine.Datas where

import qualified SDL
import SDL (Point(P), V2(..))
import Data.IORef
import Data.Set
import Data.Int
import Render.Model
import Foreign.C.Types
import Engine.Consts
-- datas and types used in this engine

type ActiveKeys = Set SDL.Keycode
type PixOff = (CInt, CInt) -- x ; y

data EngineState = EngineState
                 { keys :: ActiveKeys
                 , dt :: Double -- last delta time
                 , over :: Bool
                 , camera :: Camera
                 }

getKeys :: EngineState -> ActiveKeys
getKeys (EngineState keys _ _ _) = keys

getCamera :: EngineState -> Camera
getCamera (EngineState _ _ _ cam) = cam

calcCameraPosition :: Camera -> RenderModel -> (CInt, CInt) -> Camera
calcCameraPosition (SDL.Rectangle (P(V2 camX camY)) (V2 cW cH))
  (RenderModel (pWidth, pHeight) (pX, pY) _ _ _ _) (w, h) =
  let camX' = (pX + (pWidth `div` 2)) - (viewWidth `div` 2)
      camY' = (pY + (pHeight `div`2)) - (viewHeight `div` 2)
      in SDL.Rectangle (P $ V2 (check camX' w)
                        (check camY' h)) (V2 cW cH)

check :: CInt -> CInt -> CInt
check cam con
  | cam < 0 = 0
  | cam > con = con
  | otherwise = cam

closeGame :: IORef EngineState -> IO ()
closeGame es = do
  eState <- readIORef es
  writeIORef es (eState{ over = True })

--TEMP
isOver :: EngineState -> Bool
isOver (EngineState keys _ over _) = over || (SDL.KeycodeEscape `elem` keys) ||
                              (SDL.KeycodeQ `elem` keys)

sampleState :: EngineState
sampleState = EngineState
              { keys = empty :: Set SDL.Keycode
              , dt = 0.0
              , over = False
              , camera = SDL.Rectangle (P $ V2 0 0) (V2 viewWidth viewHeight)
              }
