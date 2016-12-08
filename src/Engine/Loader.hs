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
import Engine.Consts

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
  mapData <- loadLines sampleConfig
  let listModel = M.toList $ getModelsSet gameState
      loadMaps' = loadMaps mapData []
  loadedModels <- loadModels listModel M.empty M.empty ren
  -- [[TileMap]]loadMaps'' <- loadMapsTex ren loadMaps' []
  -- zaladowac teraz tekstury dla tilesow - moze mapa ma info o tym?
  --putStrLn (show loadMaps')
  -- loadedWorld <- loadWorldData
  writeIORef gs (gameState{ modelsSet = loadedModels{-,
                            maps = loadMaps''-}  })

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
      let cleanD = lines lData
      return cleanD
  | otherwise = return ([])

--todo: load config from file
loadMaps :: [String] -> [TileMap TileKind] -> [TileMap TileKind]
loadMaps str tilesMap
  | null str = tilesMap
  | otherwise = loadMaps str' (map':tilesMap)
    where (map', str') = loadMap str (TileMap 0 0 [] tilePath)

loadMap :: [String] -> TileMap TileKind -> (TileMap TileKind, [String])
loadMap (x:xs) map'
  | null x = (map', xs)
  | length x /= 30 = loadMap xs (setWH (words x) map')
  | otherwise = loadMap xs (transformTiles x 0 0 map')
loadMap [] map' = (map', [])

transformTiles :: String -> Int -> Int -> TileMap TileKind -> TileMap TileKind
transformTiles (x:xs) !wAcc !hAcc (TileMap w' h' tiles' path) = case x of
  '0' -> transformTiles xs w h TileMap { width = w'
                                       , height = h'
                                       , tiles = tile:tiles'
                                       , tilesPath = path
                                       }
    where (w, h, tile) = makeTile Sky wAcc hAcc w' h'

  '1' -> transformTiles xs w h TileMap { width = w'
                                       , height = h'
                                       , tiles = tile:tiles'
                                       , tilesPath = path
                                       }
    where (w, h, tile) = makeTile Ground wAcc hAcc w' h'

  '2' -> transformTiles xs w h TileMap { width = w'
                                       , height = h'
                                       , tiles = tile:tiles'
                                       , tilesPath = path
                                       }
    where (w, h, tile) = makeTile Lava wAcc hAcc w' h'

  '3' -> transformTiles xs w h TileMap { width = w'
                                       , height = h'
                                       , tiles = tile:tiles'
                                       , tilesPath = path
                                       }
    where (w, h, tile) = makeTile Sky wAcc hAcc w' h'
transformTiles [] _ _ map' = map'

setWH :: [String] -> TileMap TileKind -> TileMap TileKind
setWH (x:y:z:[]) (TileMap w h tiles path) = TileMap (read x) (read y) tiles path
setWH [] map' = map'

makeTile :: TileKind -> Int -> Int -> Int -> Int
         -> (Int, Int, Tile TileKind)
makeTile kind' width height mapW mapH
  | width > mapW && height < mapH = (0, height + (fromIntegral tileSize)
                                    , (Tile (tileSize, tileSize)
                                       (0, (fromIntegral height) + tileSize)
                                       kind' undefined))
  | height > mapH = (width, mapH
                    , (Tile (tileSize, tileSize)
                        ((fromIntegral width), fromIntegral mapH)
                       kind' undefined))
  | otherwise = (width + (fromIntegral tileSize), height
                , (Tile (tileSize, tileSize)
                   (((fromIntegral width) + tileSize), fromIntegral height)
                   kind' undefined))

loadMapsTex :: SDL.Renderer -> [TileMap TileKind]
           -> [TileMap TileKind] -> IO [TileMap TileKind]
loadMapsTex ren (x:xs) dist = do
  mapTes <- mapTex ren x
  loadMapsTex ren xs (mapTes:dist)
loadMapsTex ren [] dist = return dist

mapTex :: SDL.Renderer -> TileMap TileKind -> IO (TileMap TileKind)
mapTex ren t@(TileMap _ _ tiles tilesPath) = do
  mainTexture <- loadTexture ren tilesPath
  let tiles' = createModels mainTexture tiles []
  return (t{tiles = tiles'})

createModels :: Texture -> [Tile TileKind] -> [Tile TileKind]
             -> [Tile TileKind]
createModels t@(Texture tex (V2 w h)) (x:xs) tiles = createModels t xs
                                                     ((loadTile t x):tiles)
createModels _ [] tiles = tiles

loadTile :: Texture -> Tile TileKind -> Tile TileKind
loadTile (Texture tex (V2 w h)) t@(Tile (tw, th) (x, y) kin _) = undefined
  --t{model = (RenderModel)}
