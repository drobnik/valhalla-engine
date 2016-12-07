{-# LANGUAGE BangPatterns #-}
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
import Data.Char

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
  --ptr (loadLines sampleConfig)
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

loadLines :: LoadConfig -> IO [String]
loadLines (LoadConfig mPath _)
  | not $ null mPath = do
      lData <- loadFile mPath
      let cleanD = lines lData --fst for lvl, rest - tiles
      return cleanD

  | otherwise = return ([])

--todo: load config from file
{-loadMaps :: [String] -> [TileMap TileKind] -> [TileMap TileKind]
loadMaps str tilesMap
  | null str = tilesMap
  | otherwise =
-}

loadMap :: [String] -> TileMap TileKind -> (TileMap TileKind, [String])
loadMap (x:xs) map'
  | null x = (map', xs)
  | length x /= 30 = loadMap xs (setWH x map')
  | otherwise = loadMap xs (transformTiles x 0 0 map')

-- trzeba tutaj miec dlugosc i szerokosc, naliczana przy kazdym
-- wywolaniu rekurencyjnym. Kontrolowac to z constami!
transformTiles :: String -> Int -> Int -> TileMap TileKind -> TileMap TileKind
transformTiles (x:xs) !wAcc !hAcc (TileMap w h tiles) = case x of
  '0' -> transformTiles xs w h (TileMap w h (tile:tiles)::TileMap TileKind)
    where (w, h, tile) = makeTile Sky wAcc hAcc
  {-'1' -> transformTiles xs (TileMap w h (Ground:tiles))
  '2' -> transformTiles xs (TileMap w h (Lava:tiles))
  '3' ->transformTiles xs (TileMap w h (Spikes:tiles))-}
transformTiles [] _ _ map' = map'

setWH :: String -> TileMap TileKind -> TileMap TileKind
setWH (x:y:[]) (TileMap w h tiles) = TileMap (digitToInt x) (digitToInt y) tiles
setWH [] map' = map'

makeTile :: TileKind -> Int -> Int -> (Int, Int, Tile TileKind)
makeTile = undefined

{-
makeTile :: TileKind -> Tile TileKind
makeTile kind' = Tile
                { dim = tileDim
                , pos = calcPos
                , kind = kind'
                , model = undefined --potem zaladowac przy ladowaniu rendermodeli
                }
-}
