{-# LANGUAGE BangPatterns #-}

-- |
module GameState
  (GameState(..)
  , initStateG
  , gameLoop
  , getTilesModels
  , getWorldModels
  ) where

import qualified Data.Map.Strict as Map (lookup
                                        , insert
                                        , empty)
import Data.Map.Strict (Map(..))
import qualified Debug.Trace as D
import Foreign.C.Types (CInt(..))
import Data.Int(Int32(..))
import Data.Maybe
import Data.IORef
import Control.Concurrent

import qualified SDL (Rectangle(..))
import SDL (Point(P), V2(..))
import qualified SDL.Time (Timer(..))

import qualified World as W
import  Render.Model
        ( Camera
        , RenderModel(..)
        ,  modifyModelPos
        , renPos
        , dummyModel
        ,  addCameraOffset
        , calcCameraPosition
        , modifyPos
        , checkOffset
        , hasOffsetChanged
        )
import qualified Render.Model as RM (pos, dim)
import qualified GameData as GD(pos, dim)
import Engine.Datas
import Engine.Timer
import Engine.Collision
import Engine.Consts
import GameData


data GameState = GameState
    { level :: Int              -- ^ Currently played level
    , world :: W.World          -- ^ World state

    -- |  Map containing tilesets for every level.
    -- Key stores level number, beginning from 1.
    , maps  :: Map Int TileMap
    }

-- | Initialize a sample game state, starting at level 1.
initStateG :: GameState
initStateG = GameState
             { level = 1
             , world = undefined
             , maps = Map.empty
             }

-- | Retrieve graphic information about a tileset in current level.
--   When the information cannot be restored, an error is rised.
getTilesModels :: GameState      -- ^ Current game state
               -> [RenderModel]  -- ^ Tiles graphic representation
getTilesModels (GameState lvl _ maps) = case Map.lookup lvl maps of
                                           Just tileMap -> getModels
                                                          (tiles tileMap) []
                                           Nothing      -> error
                                                          "Level map not found!"

-- | Update a world depending on player status and movement.
changeWorld :: GameState -- ^ Current game state
            -> Camera    -- ^ Player camera
            -> W.Player  -- ^ Player entity
            -> W.World   -- ^ Updated world state
changeWorld (GameState lvl world _) cam player = W.updateWorld
                                                 world cam player lvl

-- | Update map if the tiles' position has changes.
updateMap :: GameState       -- ^ GameState with outdated tile maps
          -> TileMap         -- ^ Updated tile map
          -> Map Int TileMap -- ^ New map with updated tile map entry
updateMap !(GameState lvl _ maps) !tiles = if isEmpty tiles
                                           then maps
                                           else Map.insert lvl tiles maps

-- | Apply the cammera offset if there is any.
changeTilesLvl :: GameState -- ^ GameState with outdated tile maps
               -> Camera    -- ^ Player camera
               -> TileMap   -- ^ Tiles map with applied position offset
changeTilesLvl (GameState lvl _ maps) cam = if hasOffsetChanged cam
                                            then
                                              case Map.lookup lvl maps of
                                                Just tileMap -> changeTiles cam
                                                                tileMap
                                                Nothing      -> error
                                                                "Level map not\
                                                                \ found!"
                                            else empty

-- | Map camera offset on every tile in the map.
changeTiles :: Camera  -- ^ Player camera
            -> TileMap -- ^ Outdated map
            -> TileMap -- ^ Updated map with an offset
changeTiles !cam (TileMap w h tiles path) = TileMap w h (map (changeTile cam) tiles)
                                           path

-- | Define a new relative position for tile sprites in the scene.
-- This function is evoked only when the camera offset has been set.
changeTile :: Camera -> Tile -> Tile
changeTile  (SDL.Rectangle (P(V2 camX camY)) _)
  (Tile d p k !rm@(RenderModel _ (x, y) _ _ _ instr) !box) = Tile
                                                             { GD.dim = d
                                                             , GD.pos = p
                                                             , kind = k
                                                             , model = rm
                                                               { RM.pos =
                                                                 (CInt x', CInt y')
                                                               , renderInstr =
                                                                 modifyPos
                                                                 instr []
                                                                 (CInt x', CInt y')
                                                               }
                                                             , tBox = box
                                                             }
  where (x', y') = (fromIntegral (x - camX), fromIntegral (y - camY))

-- | Return 'RenderModel's for every entity present in the game.
getWorldModels :: GameState -> [RenderModel]
getWorldModels (GameState lvl wor _) = W.renderWorld wor lvl

-- | Return all 'BoundingBox' from current level.
getBoundingBoxes :: GameState -> [(BoundingBox, BoxKind)]
getBoundingBoxes (GameState lvl world' maps') = case Map.lookup lvl maps' of
                                                  Just tileMap -> W.getEntitiesBoxes
                                                                  world' lvl
                                                                  ++ getTilesBox
                                                                  tileMap
                                                  Nothing      -> error
                                                                  "Level map not\
                                                                  \ found!"

-- |
getPlayer :: GameState -> W.Player
getPlayer (GameState _ world _) = W.player world

-- | Return dimensions of currently played level
levelInfo :: GameState -> (Int, Int)
levelInfo (GameState lvl _ maps) = (w, h)
  where (TileMap w h _ _) = case Map.lookup lvl maps of
                              Just tileMap -> tileMap
                              Nothing      -> error "Level map not found!"

-- | Return 'RenderModel' value at a key
getModelKey :: Int -> Map Int RenderModel -> RenderModel
getModelKey n modMap = case Map.lookup n modMap of
                         Just m  -> m
                         Nothing -> error "Model not found!"

-- |
modifyGameState :: Map Int TileMap -> W.World -> GameState -> GameState
modifyGameState  correctedTiles wor' (GameState lvl _ _) = GameState
                                                           { level = lvl
                                                           , world = wor'
                                                           , maps  = correctedTiles
                                                           }

-- |
getLevelSize :: GameState -> (CInt, CInt)
getLevelSize (GameState lvl _ maps) =
  let (TileMap w h tiles' tilesP) = case Map.lookup lvl maps of
                                      Just tileMap -> tileMap
                                      Nothing      -> error "Level map \
                                                            \not found!"
  in (CInt(fromIntegral w), CInt (fromIntegral h))

-- | Convert camera offset for position calculations.
calcSum :: Camera -> (CInt, CInt) -> (Double, Double)
calcSum (SDL.Rectangle (P(V2 camX camY)) _) (x, y) = ( fromIntegral(camX + x)
                                                     , fromIntegral (camY + y))

-- | Return old 'World' and 'TileMap' for update.
getWorldAndTiles :: GameState -> (W.World, TileMap)
getWorldAndTiles (GameState lvl world tileMaps) = (world, tileMap)
  where tileMap = case Map.lookup (lvl-1) tileMaps of
                    Just tMap -> tMap
                    Nothing   -> error "Level map not found!"

-- | Get a list of tiles with the same bounding boxes. If  there are none
-- return a dummy Sky 'Tile'.
getTile :: [(BoundingBox, BoxKind)] -- ^ Bounding box with its tile kind
        -> TileMap                  -- ^ Tile map for current level
        -> [Tile]                   -- ^ Tiles with the same 'BundingBox'
getTile [] _ = [(Tile (0, 0) (0, 0) Sky undefined undefined)]
getTile ((box, _):xs) (TileMap _ _ tiles _) = filter (\y -> tBox y == box) tiles

-- | Convert an offset from 'Camera' to 'Double' value.
prepack :: (CInt, CInt)
        -> (Double, Double)
prepack (x, y) = (fromIntegral x, fromIntegral y)

-- | Simple vector substraction.
calc :: (Double, Double) -> (Double, Double) -> (Double, Double)
calc (x, y) (x2, y2) = (x2 - x, y2 - y)

-- | The core function. Defines
gameLoop :: IORef EngineState -- ^
         -> IORef GameState   -- ^
         -> Double            -- ^
         -> IO ()             -- ^
gameLoop es gs timeStep = do
                       engineState <- readIORef es
                       gameState <- readIORef gs
                       let
                         activeKeys = keys engineState
                         tree = insertElements
                                (getBoundingBoxes gameState)
                                (newQuadtree 1 (winSetup engineState))
                         (oldWorld, oldTiles) = getWorldAndTiles gameState
                         (cam'', tiles', world') = W.runWorld activeKeys
                                                 timeStep (camera engineState)
                                                 oldTiles oldWorld
                                                 (winSetup engineState)
                                                 (getBoundingBoxes gameState)

                         playerMod = W.heroM $ getPlayer gameState
                         levelDims = levelInfo gameState
                         position = modelPosition activeKeys (renPos playerMod)
                                    timeStep
                         model' = modifyModelPos playerMod position levelDims
                                  (camera engineState)
                         cam' = calcCameraPosition (camera engineState) model'
                                (getLevelSize gameState)
                         player = W.updatePlayer (getPlayer gameState) model'
                                  (prepack position)
                         collisions = checkCollisions (W.pBox player)
                                      (retrieve (W.playerBox world) tree)
                         correctCam = checkOffset (camera engineState) cam'
                         tileslvl = changeTilesLvl gameState correctCam
                         tileMaps = updateMap gameState tileslvl
                         world = changeWorld gameState correctCam player
                       threadDelay 8000

                       writeIORef gs (modifyGameState tileMaps world gameState)
                       writeIORef es engineState{ camera = cam' }
