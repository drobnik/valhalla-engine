module GameState where

import qualified Data.Map as Map
import Data.Map (Map(..))
import Data.Int(Int32(..))
import Render.Model (modifyModelPos, renPos
                    , RenderModel(..), dummyModel
                    ,  addCameraOffset
                    , calcCameraPosition, Camera, modifyPos
                    , checkOffset)
import qualified Render.Model as RM (pos, dim)
import Engine.Datas
import Engine.Timer
import Engine.Collision
import Engine.Consts
import Data.IORef
import Foreign.C.Types (CInt(..))
import GameData
import qualified GameData as GD(dim, pos)
import qualified World as W
import qualified SDL (Rectangle(..))
import SDL (Point(P), V2(..))
import qualified SDL.Time (Timer(..))
import qualified Debug.Trace as D

data GameState = GameState
                 { level :: Int
                 , world :: W.World
                 , maps :: [TileMap] -- level maps
                 }

getTilesModels :: GameState -> [RenderModel]
getTilesModels (GameState lvl _ maps) = getModels (tiles (maps !! lvl)) []

--change
changeWorld :: GameState -> Camera -> W.Player -> W.World
changeWorld (GameState lvl world _) cam player = W.updateWorld
                                                    world cam player lvl

updateMap :: GameState -> [TileMap] -> [TileMap]
updateMap (GameState lvl _ maps) tiles = take lvl maps ++ tiles
                                           ++ drop (lvl+1) maps

changeTilesLvl :: GameState -> Camera -> [TileMap]
changeTilesLvl (GameState lvl _ maps) cam = [changeTiles cam (maps !! lvl)]

changeTiles :: Camera -> TileMap -> TileMap
changeTiles cam tileMap = map''
  where
        (TileMap w h tiles' pat) = tileMap
        map'' = TileMap w h (map (changeTile cam) tiles') pat

changeTile :: Camera -> Tile -> Tile
changeTile  (SDL.Rectangle (P(V2 camX camY)) _)
  (Tile d p k rm@(RenderModel _ (x, y) _ _ _ instr) box) =
   Tile { GD.dim = d, GD.pos = p, kind = k, model = rm
                                                    { RM.pos = (CInt x', CInt y')
                                                    , renderInstr = modifyPos
                                                      instr [] (CInt x', CInt y')
                                                    }, tBox = box}
  where (x', y') = (fromIntegral (x - camX), fromIntegral (y - camY))

getWorldModels :: GameState -> [RenderModel]
getWorldModels (GameState _ wor _) = W.renderWorld wor

getBoundingBoxes :: GameState -> [(BoundingBox, BoxKind)]
getBoundingBoxes (GameState lvl world' maps') = W.getEntitiesBoxes world' lvl
                                                ++ (getTilesBox (maps' !! lvl))

getPlayer :: GameState -> W.Player
getPlayer (GameState _ world _) = W.player world

levelInfo :: GameState -> (Int, Int)
levelInfo (GameState lvl _ maps) = (w, h)
  where (TileMap w h _ _) = maps !! lvl

getModelKey :: Int -> Map Int RenderModel-> RenderModel
getModelKey n modMap = case Map.lookup n modMap of
  Just m -> m
  Nothing -> dummyModel

modifyGameState :: [TileMap] -> W.World -> GameState -> GameState
modifyGameState  tileCam wor' (GameState lvl _ _) =
  GameState{ level = lvl, world = wor', maps = tileCam}

getLevelSize :: GameState -> (CInt, CInt)
getLevelSize (GameState lvl _ maps) =
  let (TileMap w h tiles' tilesP) = maps !! lvl
      in (CInt(fromIntegral w), CInt (fromIntegral h))

calcSum :: Camera -> (CInt, CInt) -> (Double, Double)
calcSum (SDL.Rectangle (P(V2 camX camY)) _) (x, y) =
  (fromIntegral(camX + x), fromIntegral (camY + y))

getWorldAndTiles :: GameState -> (W.World, TileMap)
getWorldAndTiles (GameState lvl world tilemaps) = (world, (tilemaps !! (lvl - 1)))

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
      tiles = updateMap gameState tileslvl
      world = changeWorld gameState correctCam player

--  dist (W.pBox player) collisions
  D.traceIO (show collisions)
--  D.traceIO (show (calc (W.pPos $ getPlayer gameState) (W.pPos player)))
--  D.traceIO (show (getTile collisions (tileslvl !! 0)))
  writeIORef gs (modifyGameState tiles world gameState)
  writeIORef es engineState{camera = cam'}

--later: read from json config file maybe?
initStateG :: GameState
initStateG = GameState
             { level = 1
             , world = undefined
             , maps = []
             }
