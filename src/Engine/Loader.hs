module Engine.Loader where

import Control.Monad
import Data.Map (Map(..))
import qualified Data.Map as M
import Data.IORef
import Data.Maybe
import Render.Model
import Render.Primitives
import GameState
import GameData
import SDL.Vect
import SDL (($=))
import qualified SDL
import System.IO

import Paths_valhalla_engine (getDataFileName)

--later - move it to some file?
data LoadConfig = LoadConfig
                { mapsPath :: FilePath
                , contentsPath :: FilePath -- fe coin 200 35
                }

sampleConfig :: LoadConfig
sampleConfig = LoadConfig { mapsPath = "example_data/levels.txt"
                          , contentsPath = ""}

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
loadGame :: SDL.Renderer -> IORef GameState -> IO ()
loadGame ren gs = do
  gameState <- readIORef gs
  let listModel = M.toList $ getModelsSet gameState
  loadedModels <- loadModels listModel M.empty M.empty ren
--  ptr (loadMaps sampleConfig)
  --loadMaps sampleConfig
  -- loadedMaps <- loadMaps gameState sampleConfig
  -- loadedLevel <- loadLevelData
  writeIORef gs (gameState{ modelsSet = loadedModels })

loadModels :: [(Int,RenderModel)] -> Map Int RenderModel -> Map FilePath Texture
           -> SDL.Renderer -> IO (Map Int RenderModel)
loadModels (x:xs) modelsMap texMap ren = do
  (model', texMap') <- loadModel x texMap ren
  loadModels xs (M.insert (fst model') (snd model') modelsMap) texMap' ren
loadModels [] modelsMap _ _ = return modelsMap

loadModel :: (Int, RenderModel) -> Map FilePath Texture -> SDL.Renderer
          -> IO ((Int, RenderModel), (Map FilePath Texture))
loadModel (i, rm@(RenderModel _ pos path' tex _ instruct)) texMap ren
  | null path' = return ((i, rm), texMap)
  | path' `M.member` texMap = do
      let tex' = fromMaybe noTexture (M.lookup path' texMap)
      return ((i, rm{ texture = tex'
                             , renderInstr = instruct --later: indicate animated RM
                                       ++ [RenderTexture tex' pos]
                             }) , texMap)
  | otherwise = do
      tex' <- loadTexture ren path'
      let texMap' = M.insert path' tex' texMap
      return ((i, rm{ texture = tex'
                             , renderInstr = instruct
                                       ++ [RenderTexture tex' pos]
                             }) , texMap')

loadFile :: FilePath -> IO String
loadFile path = do
  initData <- readFile path
  return (filter (\x -> x /= '\t') initData)

ptr :: IO [String] -> IO ()
ptr x = do
  pl <- x
  mapM_ putStrLn pl
{-
-- potem fmap lines
loadMaps :: LoadConfig -> IO [String]--([TileMap TileKind])
loadMaps (LoadConfig mPath _)
  | not $ null mPath = do
      lData <- loadFile mPath
      let cleanD = fmap lines lData --fst for lvl, rest - tiles
      return cleanD
      --return (transformTiles [] cleanD)
  | otherwise = return ([])
-}
{-
transformTiles :: [TimeMap TileKind] -> [String]
               -> Bool -> [TileMap]
transformTiles (x:xs)
-}
