module Engine.Loader where

import Control.Monad
import Foreign.C.Types
import Data.Map
import SDL.Vect
import SDL (($=))
import qualified SDL

import Paths_valhalla_engine (getDataFileName)

data Texture = Texture
             { tex :: SDL.Texture
             , size :: V2 CInt
             }

--bmp only for now
loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture ren path = do
  surface <- getDataFileName path >>= SDL.loadBMP
  size <- SDL.surfaceDimensions surface
  SDL.surfaceColorKey surface $= Just (V4 0 255 255 255)
  tex <- SDL.createTextureFromSurface ren surface
  SDL.freeSurface surface
  return (Texture tex size)
