module Engine.Loader where

import Control.Monad
import Data.Map (Map(..))
import qualified Data.Map as M
import Data.IORef
import Data.Maybe
import Render.Model
import Render.Primitives
import GameState
import SDL.Vect
import SDL (($=))
import qualified SDL

import Paths_valhalla_engine (getDataFileName)

--bmp only for now
loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture ren path = do
  surface <- getDataFileName path >>= SDL.loadBMP
  size <- SDL.surfaceDimensions surface
  SDL.surfaceColorKey surface $= Just (V4 0 255 255 255)
  tex <- SDL.createTextureFromSurface ren surface
  SDL.freeSurface surface
  return (Texture tex size)

--load textures for every rendermodel in the map + more
loadGame :: SDL.Renderer -> IORef GameState
         -> Map FilePath Texture -> IO ()
loadGame ren gs textures = do
  gameState <- readIORef gs
  let listModel = M.toList $ getModelsSet gameState
  loadedModels <- loadModels listModel M.empty M.empty ren
  --loadedModels <- mapM (loadModel ren textures) (getModelsSet gameState) --DEFINE
  writeIORef gs (gameState{ modelsSet = loadedModels })

loadModels :: [(Int,RenderModel)] -> Map Int RenderModel -> Map FilePath Texture
           -> SDL.Renderer -> IO (Map Int RenderModel)
loadModels (x:xs) modelsMap texMap ren = do
  (model', texMap') <- loadModel x texMap ren
  loadModels xs (M.insert (fst model') (snd model') modelsMap) texMap' ren
loadModels [] modelsMap _ _ = return modelsMap

loadModel :: (Int, RenderModel) -> Map FilePath Texture -> SDL.Renderer
          -> IO ((Int, RenderModel), (Map FilePath Texture))
loadModel (i, (RenderModel _ _ path' tex _ instruct)) texMap ren
  | path' `M.member` texMap = do
      let tex' = fromMaybe noTexture (M.lookup path' texMap)
      return ((i, RenderModel{ texture = tex'
                             , renderInstr = instruct --later: indicate animated RM
                                       ++ [RenderTexture tex']
                             }) , texMap)
  | otherwise = undefined-- dodaj do mapy, zaladuj teksture i daj ja do rendermodel
