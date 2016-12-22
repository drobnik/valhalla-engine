{-# LANGUAGE BangPatterns #-}
module Engine.Loader where

import Control.Monad
import Data.Map (Map(..))
import qualified Data.Map as M
import Data.IORef
import Data.Maybe
import Render.Model (RenderModel(..))
import qualified Render.Model as RM
import Render.Primitives
import GameState
import GameData
import Engine.Datas
import Engine.Collision (makeBox, BoundingBox(BoundingBox))
import World (Entity(..), EntityType(..))
import qualified World as W
import SDL.Vect
import SDL (($=))
import qualified SDL
import Foreign.C.Types
import Data.Char
import Engine.Consts
import Data.Int
import qualified Data.List.Split as S
import qualified Debug.Trace as Debug

import Paths_valhalla_engine (getDataFileName)

--later - move it to some file?
data LoadConfig = LoadConfig
                { mapsPath :: FilePath
                , contentsPath :: FilePath -- fe coin 200 35
                }

sampleConfig :: LoadConfig
sampleConfig = LoadConfig { mapsPath = "example_data/levels.txt"
                          , contentsPath = "example_data/level_1_unit.txt"
                          --TEMP for one level only!
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

--load textures for every rendermodel in the map + more
loadGame :: SDL.Renderer -> IORef GameState -> IO ()
loadGame ren gs = do
  let conf = sampleConfig
  gameState <- readIORef gs
  mapData <- loadMapLines conf
  unitData <- loadUnitLines conf
  unitTexs <- loadUnitTex ren
  let loadMaps' = loadMaps mapData []
      loadUnits' = loadUnits unitData ren unitTexs []
      player = loadPlayer ren unitTexs
  loadMaps'' <- loadMapsTex ren loadMaps' []

  writeIORef gs (gameState{ maps = loadMaps''
                          , world = W.setupWorld loadUnits' player
                          })

loadModel :: (Int, RenderModel) -> Map FilePath Texture -> SDL.Renderer
          -> IO ((Int, RenderModel), (Map FilePath Texture))
loadModel (i, rm@(RenderModel _ pos path' tex _ instruct)) texMap ren
  | null path' = return ((i, rm), texMap)
  | path' `M.member` texMap = do
      let tex' = fromMaybe noTexture (M.lookup path' texMap)
      return ((i, rm
                  { texture = tex'
                  , renderInstr = instruct --later: indicate animated RM
                                    ++ [RenderTexture tex' pos]
                  }) , texMap)
  | otherwise = do
      tex' <- loadTexture ren path'
      let texMap' = M.insert path' tex' texMap
      return ((i, rm
                  { texture = tex'
                  , renderInstr = instruct
                                    ++ [RenderTexture tex' pos]
                  }) , texMap')

loadFile :: FilePath -> IO String
loadFile path = do
  p <- getDataFileName path
  initData <- readFile p
  return (filter (\x -> x /= '\t') initData)

loadMapLines :: LoadConfig -> IO [String]
loadMapLines (LoadConfig mPath _)
  | not $ null mPath = do
      lData <- loadFile mPath
      let cleanD = lines lData
      return cleanD
  | otherwise = return ([])

--TODO one function
loadUnitLines :: LoadConfig -> IO [String]
loadUnitLines (LoadConfig _ cPath)
  | not $ null cPath = do
      p <- getDataFileName cPath
      initData <- readFile p
      let cleanD = S.splitOn "\r" (filter (\x -> x /= '\n') initData)
      return cleanD
  | otherwise = return ([])

--todo: load config from file
loadMaps :: [String] -> [TileMap] -> [TileMap]
loadMaps st@(x:xs) tilesMap = loadMaps str' (map':tilesMap)
  where (map', str') = loadMap st 0 0 (TileMap 0 0 [] tilePath)
loadMaps [] tilesMap = tilesMap

loadMap :: [String] -> Int -> Int -> TileMap
        -> (TileMap, [String])
loadMap (x:xs) wAcc hAcc map'
  | null x = (map', xs)
  | length x /= 30 = loadMap xs wAcc hAcc ((setWH (words x) map'))
  | otherwise = loadMap xs wAcc' hAcc' mapK
  where (wAcc', hAcc', mapK) = transformTiles x wAcc hAcc map'
loadMap [] _ _ map' = (map', [])

transformTiles :: String -> Int -> Int -> TileMap
               -> (Int, Int, TileMap)
transformTiles (x:xs) !wAcc !hAcc tm@(TileMap w' h' tiles' path) = case x of
  '0' -> transformTiles xs w h tm{tiles = tile:tiles'}
    where (w, h, tile) = (makeTile Sky wAcc hAcc w' h')

  '1' -> transformTiles xs w h tm{tiles = tile:tiles'}
    where (w, h, tile) = makeTile Ground wAcc hAcc w' h'

  '2' -> transformTiles xs w h tm{tiles = tile:tiles'}
    where (w, h, tile) = makeTile Lava wAcc hAcc w' h'

  '3' -> transformTiles xs w h tm{tiles = tile:tiles'}
    where (w, h, tile) = makeTile Spikes wAcc hAcc w' h'
transformTiles [] wAcc hAcc map' = (wAcc, hAcc, map')

setWH :: [String] -> TileMap -> TileMap
setWH (x:y:z:[]) (TileMap w h tiles path) = (TileMap (read x) (read y) tiles path)
setWH [] map' = map'

makeTile :: TileKind -> Int -> Int -> Int -> Int
         -> (Int, Int, Tile)
makeTile kind' width height mapW mapH
  | (width + tileSInt) >= mapW = (0, (height + tileSInt)
                                 , (Tile (tileSize, tileSize)
                                   ((fromIntegral width)
                                   , fromIntegral height) kind'
                                   undefined) (makeBox 0
                                               (height + tileSInt)
                                               tileSize tileSize))
  | otherwise = ((width + tileSInt), height, (Tile (tileSize, tileSize)
                                              ((fromIntegral width )
                                              ,(fromIntegral height)) kind'
                                              undefined) (makeBox width
                                                         height
                                                         tileSize tileSize))

loadMapsTex :: SDL.Renderer -> [TileMap] -> [TileMap] -> IO [TileMap]
loadMapsTex ren (x:xs) dist = do
  mapTes <- mapTex ren x
  loadMapsTex ren xs (mapTes:dist)
loadMapsTex ren [] dist = return dist

mapTex :: SDL.Renderer -> TileMap -> IO (TileMap)
mapTex ren t@(TileMap _ _ tiles tilesPath) = do
  mainTexture <- loadTexture ren tilesPath
  let tiles' = createModels mainTexture tiles []
  return (t{tiles = tiles'})

createModels :: Texture -> [Tile] -> [Tile]
             -> [Tile]
createModels t@(Texture tex (V2 w h)) (x:xs) tiles = createModels t xs
                                                     ((loadTile t x):tiles)
createModels _ [] tiles = tiles

loadTile :: Texture -> Tile -> Tile
loadTile tex t@(Tile (tw, _) (x, y) kin _ _) =
  t{GameData.model = RenderModel
            { RM.dim = (d, d)
            , RM.pos = (x', y')
            , path = ""
            , texture = tex
            , modelColor = V4 0 0 0 255
            , renderInstr = [RenderFrame tex (Just (tilesData kin)) (Just dest)]
            }}
  where d = CInt tw
        x' = CInt x
        y' = CInt y
        dest = SDL.Rectangle (P $ V2 x' y') (V2 d d)

loadUnits :: [String] -> SDL.Renderer -> Map UnitKind Texture
          -> [Entity] -> [Entity]
loadUnits (x:xs) ren texs mapEnt = if not $ null x
                                      then loadUnits xs ren texs (x':mapEnt)
                                           else loadUnits xs ren texs mapEnt
  where x' = loadUnit (S.splitOn "\t" x) texs
loadUnits [] _ _ mapEnt = mapEnt

loadUnitTex :: SDL.Renderer -> IO (Map UnitKind Texture)
loadUnitTex ren = do
  coinTex <- loadTexture ren (unitPath CoinU)
  gateTex <- loadTexture ren (unitPath GateU)
  healthTex <- loadTexture ren (unitPath HealthUpU)
  playerTex <- loadTexture ren (unitPath PlayerU)
  return ((M.insert CoinU coinTex) $ (M.insert GateU gateTex)
    $ (M.insert PlayerU playerTex) $ (M.insert HealthUpU healthTex)
    $ M.empty)

-- player0 390
-- load dimensions from texture
loadUnit :: [String] -> Map UnitKind Texture -> Entity
loadUnit (x:y:z:[]) textures = case x of
  "coin" -> createUnit texC Collect 10 ((read y), (read z))
    where texC = fromMaybe (Texture undefined (V2 0 0))
                 (getUnitTex textures CoinU)

  "gate" -> createUnit texG Gate 0 ((read y),(read z))
    where texG = fromMaybe (Texture undefined (V2 0 0))
                 (getUnitTex textures GateU)

  "healthUp" -> createUnit texH BonusH 0 ((read y), (read z))
    where texH = fromMaybe (Texture undefined (V2 0 0))
                 (getUnitTex textures HealthUpU)
loadUnit _ textures = createUnit (fromMaybe (Texture undefined (V2 0 0))
                 (getUnitTex textures CoinU)) Collect 0 (10, 10)

createUnit :: Texture -> EntityType -> Int -> (Int32, Int32)
           -> Entity
createUnit tex@(Texture _ (V2 w h)) kind value (x, y) = Entity (w', h') value
                                                        (x, y) kind bBox modE
  where
    w' = fromIntegral w
    h' = fromIntegral h
    modE = RenderModel (w,h) (CInt x, CInt y) undefined tex
          (V4 0 0 0 255) [RenderTexture tex (CInt x, CInt y)]
    bBox = makeBox (fromIntegral x) (fromIntegral y) w' h'

loadPlayer :: SDL.Renderer -> Map UnitKind Texture -> W.Player
loadPlayer ren textures = W.Player (w', h') 3 (10, 390) (makeBox 10 390 w' h')
                          modP False
  where tex@(Texture _ (V2 w h)) = fromMaybe (Texture undefined (V2 0 0))
                                   (getUnitTex textures PlayerU)
        modP = RenderModel (w, h) (CInt 10, CInt 390) undefined tex
               (V4 0 0 0 255) [RenderTexture tex (CInt 10, CInt 390)]
        w' = fromIntegral w
        h' = fromIntegral h

getUnitTex :: Map UnitKind Texture -> UnitKind -> Maybe Texture
getUnitTex units kind = M.lookup kind units
