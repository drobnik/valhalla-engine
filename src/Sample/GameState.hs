{-# LANGUAGE BangPatterns #-}

module GameState
  (GameState(..)
  , initStateG
  , gameLoop
  , getTilesModels
  , getWorldModels ) where

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
        , hasOffsetChanged )
import qualified Render.Model as RM (pos, dim)
import qualified GameData as GD(pos, dim)
import Engine.Datas
import Engine.Timer
import Engine.Collision
import Engine.Consts
import GameData





data GameState = GameState
                 { level :: Int
                 , world :: W.World
                 , maps :: Map Int TileMap -- lvl -> map
                 }

getTilesModels :: GameState -> [RenderModel]
getTilesModels (GameState lvl _ maps) = case Map.lookup lvl maps of
  Just tileMap -> getModels (tiles tileMap) []
  Nothing -> error "Level map not found!"

--change
changeWorld :: GameState -> Camera -> W.Player -> W.World
changeWorld (GameState lvl world _) cam player = W.updateWorld
                                                    world cam player lvl

updateMap :: GameState -> TileMap -> Map Int TileMap
updateMap !(GameState lvl _ maps) !tiles = if isEmpty tiles
                                           then maps
                                           else
                                             Map.insert lvl tiles maps

changeTilesLvl :: GameState -> Camera -> TileMap
changeTilesLvl (GameState lvl _ maps) cam = if hasOffsetChanged cam
                                            then
                                              case Map.lookup lvl maps of
                                                Just tileMap -> changeTiles cam tileMap
                                                Nothing -> error "Level map not found!"
                                            else empty

changeTiles :: Camera -> TileMap -> TileMap
changeTiles !cam (TileMap w h tiles path) = TileMap w h (map (changeTile cam) tiles)
                                           path

changeTile :: Camera -> Tile -> Tile
changeTile  (SDL.Rectangle (P(V2 camX camY)) _)
  (Tile d p k !rm@(RenderModel _ (x, y) _ _ _ instr) !box) =
   Tile { GD.dim = d, GD.pos = p, kind = k, model = rm
                                                    { RM.pos = (CInt x', CInt y')
                                                    , renderInstr = modifyPos
                                                      instr [] (CInt x', CInt y')
                                                    }, tBox = box}
  where (x', y') = (fromIntegral (x - camX), fromIntegral (y - camY))

getWorldModels :: GameState -> [RenderModel]
getWorldModels (GameState lvl wor _) = W.renderWorld wor lvl

getBoundingBoxes :: GameState -> [(BoundingBox, BoxKind)]
getBoundingBoxes (GameState lvl world' maps') = case Map.lookup lvl maps' of
  Just tileMap -> W.getEntitiesBoxes world' lvl ++ getTilesBox tileMap
  Nothing -> error "Level map not found!"

getPlayer :: GameState -> W.Player
getPlayer (GameState _ world _) = W.player world

levelInfo :: GameState -> (Int, Int)
levelInfo (GameState lvl _ maps) = (w, h)
  where (TileMap w h _ _) = case Map.lookup lvl maps of
          Just tileMap -> tileMap
          Nothing -> error "Level map not found!"

getModelKey :: Int -> Map Int RenderModel-> RenderModel
getModelKey n modMap = case Map.lookup n modMap of
  Just m -> m
  Nothing -> dummyModel

modifyGameState :: Map Int TileMap -> W.World -> GameState -> GameState
modifyGameState  correctedTiles wor' (GameState lvl _ _) =
  GameState{ level = lvl, world = wor', maps = correctedTiles}

getLevelSize :: GameState -> (CInt, CInt)
getLevelSize (GameState lvl _ maps) =
  let (TileMap w h tiles' tilesP) = case Map.lookup lvl maps of
        Just tileMap -> tileMap
        Nothing -> error "Level map not found!"
      in (CInt(fromIntegral w), CInt (fromIntegral h))

calcSum :: Camera -> (CInt, CInt) -> (Double, Double)
calcSum (SDL.Rectangle (P(V2 camX camY)) _) (x, y) =
  (fromIntegral(camX + x), fromIntegral (camY + y))

getWorldAndTiles :: GameState -> (W.World, TileMap)
getWorldAndTiles (GameState lvl world tileMaps) = (world, tileMap)
  where tileMap = case Map.lookup (lvl-1) tileMaps of
          Just tMap -> tMap
          Nothing -> error "Level map not found!"

getTile :: [(BoundingBox, BoxKind)] -> TileMap -> [Tile]
getTile [] _ = [(Tile (0, 0) (0, 0) Sky undefined undefined)]
getTile ((box, _):xs) (TileMap _ _ tiles _) = filter (\y -> tBox y == box) tiles

prepack :: (CInt, CInt) -> (Double, Double)
prepack (x, y) = (fromIntegral x, fromIntegral y)

calc :: (Double, Double) -> (Double, Double) -> (Double, Double)
calc (x, y) (x2, y2) = (x2 - x, y2 - y)

gameLoop :: IORef EngineState -> IORef GameState -> Double -> IO ()
gameLoop es gs timeStep = do
  engineState <- readIORef es
  gameState <- readIORef gs
  let
      activeKeys = keys engineState
      tree = insertElements (getBoundingBoxes gameState) (newQuadtree 1
                                                          (winSetup engineState))
      (oldWorld, oldTiles) = getWorldAndTiles gameState
      (cam'', tiles', world') = W.runWorld activeKeys timeStep (camera engineState)
                                oldTiles oldWorld (winSetup engineState)
                                (getBoundingBoxes gameState)

      playerMod = W.heroM $ getPlayer gameState
      levelDims = levelInfo gameState
      position = modelPosition activeKeys (renPos playerMod) timeStep
      model' = modifyModelPos playerMod position levelDims
               (camera engineState)
      cam' = calcCameraPosition (camera engineState) model'
             (getLevelSize gameState)
      player = W.updatePlayer (getPlayer gameState) model' (prepack position)
      --(calcSum cam' position)
      -- quadtree + collisions + react
      collisions = checkCollisions (W.pBox player)
                   (retrieve (W.playerBox world) tree)
      correctCam = checkOffset (camera engineState) cam'
      tileslvl = changeTilesLvl gameState correctCam
      tileMaps = updateMap gameState tileslvl
      world = changeWorld gameState correctCam player
  threadDelay 8000
--  D.traceIO(show $ hasOffsetChanged correctCam)
--  dist (W.pBox player) collisions
--  D.traceIO (show collisions)
--  D.traceIO (show (calc (W.pPos $ getPlayer gameState) (W.pPos player)))
--  D.traceIO (show (getTile collisions (tileslvl !! 0)))
  writeIORef gs (modifyGameState tileMaps world gameState)
  writeIORef es engineState{camera = cam'}

--later: read from json config file maybe?
initStateG :: GameState
initStateG = GameState
             { level = 1
             , world = undefined
             , maps = Map.empty
             }
