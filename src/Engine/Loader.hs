{-# LANGUAGE BangPatterns #-}

-- | Module allows to load game resources with 'loadGame' function.
-- It involves (naive) tile map configuration parsing and creation
-- of game entities described in seperate file.
module Engine.Loader(loadGame) where

import Control.Monad
import Data.Map (Map(..))
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Maybe
import Foreign.C.Types
import Data.Char
import Data.Int
import qualified Data.List.Split as S

import SDL.Vect
import SDL (($=))
import qualified SDL

import qualified Render.Model as RM
import Render.Primitives
import GameState
import GameData
import Engine.Datas
import Engine.Collision (makeBox, BoundingBox(BoundingBox))
import World (Entity(..), EntityType(..))
import qualified World as W
import Engine.Consts
import Render.Model (RenderModel(..))
import Paths_valhalla_engine (getDataFileName) -- ^ To make relative paths happy

-- | Data which stores paths to the configuration files
data LoadConfig = LoadConfig
                { mapsPath     :: FilePath
                , contentsPath :: FilePath
                }

-- | Sample configuration. Warning -- defined for one level only!
sampleConfig :: LoadConfig
sampleConfig = LoadConfig { mapsPath     = "example_data/levels.txt"
                          , contentsPath = "example_data/level_1_unit.txt"
                          }

-- | Load texture from a file. Now only BMP files works, other
-- extensions in the future (I wish so).
loadTexture :: SDL.Renderer -> FilePath -> IO Texture
loadTexture ren path = do
  surface <- getDataFileName path >>= SDL.loadBMP
  size <- SDL.surfaceDimensions surface
  SDL.surfaceColorKey surface $= Just (V4 0 255 255 255)
  tex <- SDL.createTextureFromSurface ren surface
  SDL.freeSurface surface
  return (Texture tex size)

-- | Main function of this module. Load entities of the game and save
-- created 'World' and 'Level's to 'GameState' reference. It involves
-- loading textures for every 'RenderModel' in the map.
loadGame :: SDL.Renderer -> IORef GameState -> IO ()
loadGame ren gs = do
  let conf = sampleConfig
  gameState <- readIORef gs
  mapData   <- loadMapLines conf
  unitData  <- loadUnitLines conf
  unitTexs  <- loadUnitTex ren
  let loadMaps'  = loadMaps mapData []
      loadUnits' = loadUnits unitData ren unitTexs []
      player     = loadPlayer ren unitTexs
  loadMaps'' <- loadMapsTex ren 1 loadMaps' Map.empty

  writeIORef gs (gameState{ maps = loadMaps''
                          , world = W.setupWorld loadUnits' player
                          }
                )

-- |
loadModel :: (Int, RenderModel) -> Map FilePath Texture -> SDL.Renderer
          -> IO ((Int, RenderModel), (Map FilePath Texture))
loadModel (i, rm@(RenderModel _ pos path' tex _ instruct)) texMap ren
  | null path' = return ((i, rm), texMap)
  | path' `Map.member` texMap = do
      let tex' = fromMaybe noTexture (Map.lookup path' texMap)
      return ((i, rm
                  { texture = tex'
                  , renderInstr = instruct ++ [RenderTexture tex' pos]
                  })
             , texMap)
  | otherwise = do
      tex' <- loadTexture ren path'
      let texMap' = Map.insert path' tex' texMap
      return ((i, rm
                  { texture = tex'
                  , renderInstr = instruct ++ [RenderTexture tex' pos]
                  })
             , texMap')

-- | Load all text contents from provided file. Used for configuration loading.
loadFile :: FilePath -> IO String
loadFile path = do
  p <- getDataFileName path
  initData <- readFile p
  return (filter (\x -> x /= '\t') initData) -- ^ filter out the whitespaces

-- | Split loaded text file into lines for easier tile loading.
loadMapLines :: LoadConfig -> IO [String]
loadMapLines (LoadConfig mPath _)
  | not $ null mPath = do
      lData <- loadFile mPath
      let cleanD = lines lData
      return cleanD
  | otherwise = return ([])

-- | Split loaded text file into lines for easier unit loading.
-- Yea, it doubles 'loadMapLines' function, sorry.
loadUnitLines :: LoadConfig -> IO [String]
loadUnitLines (LoadConfig _ cPath)
  | not $ null cPath = do
      p <- getDataFileName cPath
      initData <- readFile p
      let cleanD = S.splitOn "\r" (filter (\x -> x /= '\n') initData)
      return cleanD
  | otherwise = return ([])

-- | Load all maps from the file. Warning - this function do not
-- check for proper formatting or so. It simply loads as much as it
-- can and returns incomplete maps on failure. Yes, this is dump.
loadMaps :: [String] -> [TileMap] -> [TileMap]
loadMaps st@(x:xs) tilesMap = loadMaps str' (map':tilesMap)
  where (map', str') = loadMap st 0 0 (TileMap 0 0 [] tilePath)
loadMaps [] tilesMap = tilesMap

-- | Load one map form the file. If there is a blank line, treat it
-- as an indication for the end of the map configuration and return.
-- If given line length is not 30 characters, treat this line as
-- level dimension description. Otherwise, treat it as tile list.
-- Yes, this is dump.
loadMap :: [String] -> Int -> Int -> TileMap
        -> (TileMap, [String])
loadMap (x:xs) wAcc hAcc map'
  | null x = (map', xs)
  | length x /= 30 = loadMap xs wAcc hAcc ((setWH (words x) map'))
  | otherwise = loadMap xs wAcc' hAcc' mapK
  where (wAcc', hAcc', mapK) = transformTiles x wAcc hAcc map'
loadMap [] _ _ map' = (map', [])

-- | Make a tile based on provided value for tile kind.
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

-- | Set dimensions for partially built tile map
setWH :: [String] -> TileMap -> TileMap
setWH (x:y:z:[]) (TileMap w h tiles path) = (TileMap (read x) (read y) tiles path)
setWH [] map' = map'

-- | Make a tile within provided level bounds.
makeTile :: TileKind -> Int -> Int -> Int -> Int
         -> (Int, Int, Tile)
makeTile kind' width height mapW mapH
  | (width + tileSInt) >= mapW = (0, (height + tileSInt)
                                 , (Tile (tileSize, tileSize)
                                   ((fromIntegral width)
                                   , fromIntegral height) kind'
                                   undefined) (makeBox 0 (height + tileSInt)
                                               tileSize tileSize)
                                 )
  | otherwise = ((width + tileSInt)
                , height
                , (Tile (tileSize, tileSize)
                    ((fromIntegral width )
                    ,(fromIntegral height)) kind'
                    undefined) (makeBox width height
                                tileSize tileSize)
                )

-- | Load textures for all maps already loaded.
loadMapsTex :: SDL.Renderer -> Int -> [TileMap] ->
               Map Int TileMap -> IO (Map Int TileMap)
loadMapsTex ren lvl (x:xs) partialMaps = do
  mapTes <- mapTex ren x
  loadMapsTex ren (lvl+1) xs (Map.insert lvl mapTes partialMaps)
loadMapsTex ren lvl [] dist = return dist

-- | Load texture for every tile in the map.
mapTex :: SDL.Renderer -> TileMap -> IO (TileMap)
mapTex ren t@(TileMap _ _ tiles tilesPath) = do
  mainTexture <- loadTexture ren tilesPath
  let tiles' = createModels mainTexture tiles []
  return (t{tiles = tiles'})

-- | Create graphic representation for every loaded tile.
createModels :: Texture -> [Tile] -> [Tile] -> [Tile]
createModels t@(Texture tex (V2 w h)) (x:xs) tiles = createModels t xs
                                                     ((loadTile t x):tiles)
createModels _ [] tiles = tiles

-- | Load a texture for particular tile.
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

-- | Load all units for the game.
loadUnits :: [String] -> SDL.Renderer -> Map UnitKind Texture
          -> [Entity] -> [Entity]
loadUnits (x:xs) ren texs mapEnt = if not $ null x
                                      then loadUnits xs ren texs (x':mapEnt)
                                           else loadUnits xs ren texs mapEnt
  where x' = loadUnit (S.splitOn "\t" x) texs
loadUnits [] _ _ mapEnt = mapEnt

-- | Load textures for level entities
loadUnitTex :: SDL.Renderer -> IO (Map UnitKind Texture)
loadUnitTex ren = do
  coinTex <- loadTexture ren (unitPath CoinU)
  gateTex <- loadTexture ren (unitPath GateU)
  healthTex <- loadTexture ren (unitPath HealthUpU)
  playerTex <- loadTexture ren (unitPath PlayerU)
  return ((Map.insert CoinU coinTex) $ (Map.insert GateU gateTex)
    $ (Map.insert PlayerU playerTex) $ (Map.insert HealthUpU healthTex)
    $ Map.empty)

-- | Load level entites.
loadUnit :: [String] -> Map UnitKind Texture -> Entity
loadUnit (x:y:z:[]) textures = case x of
                                 "coin"   -> createUnit texC Collect 10
                                           ((read y), (read z))
                                   where texC = fromMaybe
                                                (Texture undefined
                                                 (V2 0 0)
                                                ) (getUnitTex textures CoinU)
                                 "gate"   -> createUnit texG Gate 0
                                           ((read y),(read z))
                                   where texG = fromMaybe (Texture undefined
                                                           (V2 0 0)) (getUnitTex
                                                                      textures
                                                                      GateU)
                                 "healthUp" -> createUnit texH BonusH 0
                                               ((read y), (read z))
                                   where texH = fromMaybe (Texture undefined
                                                           (V2 0 0)) (getUnitTex
                                                                      textures
                                                                      HealthUpU)

loadUnit _ textures = createUnit (fromMaybe (Texture undefined (V2 0 0))
                                  (getUnitTex textures CoinU)) Collect 0 (10, 10)

-- | Load a game entity with all its details provided.
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

-- | Load player graphic representation
loadPlayer :: SDL.Renderer -> Map UnitKind Texture -> W.Player
loadPlayer ren textures = W.Player (w', h') 3 (10, 390) (makeBox 10 390 w' h')
                          modP False
  where tex@(Texture _ (V2 w h)) = fromMaybe (Texture undefined (V2 0 0))
                                   (getUnitTex textures PlayerU)
        modP = RenderModel (w, h) (CInt 10, CInt 390) undefined tex
               (V4 0 0 0 255) [RenderTexture tex (CInt 10, CInt 390)]
        w' = fromIntegral w
        h' = fromIntegral h

-- | Return texture for particular unit kind.
getUnitTex :: Map UnitKind Texture -> UnitKind -> Maybe Texture
getUnitTex units kind = Map.lookup kind units
