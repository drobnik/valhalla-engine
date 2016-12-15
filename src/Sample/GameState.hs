module GameState where

import qualified Data.Map as Map
import Data.Map (Map(..))
import Render.Model (modifyModelPos, renPos
                    , RenderModel(..), dummyModel
                    ,  addCameraOffset
                    , calcCameraPosition, Camera, modifyPos
                    , checkOffset)
import qualified Render.Model as RM (pos, dim)
import Engine.Datas
import Engine.Timer
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
                 , maps :: [TileMap TileKind] -- level maps
                 }

getTilesModels :: GameState -> [RenderModel]
getTilesModels (GameState lvl _ maps) = getModels (tiles (maps !! lvl)) []

--change
changeWorld :: GameState -> Camera -> RenderModel -> (CInt, CInt)
            -> W.World
changeWorld (GameState lvl world _) cam playerM pos  = W.updateWorld
                                                    world cam playerM pos lvl

updateMap :: GameState -> [TileMap TileKind] -> [TileMap TileKind]
updateMap (GameState lvl _ maps) tiles = take lvl maps ++ tiles
                                           ++ drop (lvl+1) maps

changeTilesLvl :: GameState -> Camera -> [TileMap TileKind]
changeTilesLvl (GameState lvl _ maps) cam = [changeTiles cam (maps !! lvl)]

changeTiles :: Camera -> TileMap TileKind -> TileMap TileKind
changeTiles cam tileMap = map''
  where
        (TileMap w h tiles' pat) = tileMap
        map'' = TileMap w h (map (changeTile cam) tiles') pat

changeTile :: Camera -> Tile TileKind -> Tile TileKind
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

getPlayer :: GameState -> W.Player
getPlayer (GameState _ world _) = W.player world

levelInfo :: GameState -> (Int, Int)
levelInfo (GameState lvl _ maps) = (w, h)
  where (TileMap w h _ _) = maps !! lvl

getModelKey :: Int -> Map Int RenderModel-> RenderModel
getModelKey n modMap = case Map.lookup n modMap of
  Just m -> m
  Nothing -> dummyModel

modifyGameState :: [TileMap TileKind] -> W.World
                -> GameState -> GameState
modifyGameState  tileCam wor' (GameState lvl _ _) =
  GameState{ level = lvl, world = wor', maps = tileCam}

getLevelSize :: GameState -> (CInt, CInt)
getLevelSize (GameState lvl _ maps) =
  let (TileMap w h tiles' tilesP) = maps !! lvl
      in (CInt(fromIntegral w), CInt (fromIntegral h))

--later: read from json config file maybe?
initStateG :: GameState
initStateG = GameState { level = 1
                       , world = undefined
                       , maps = []
                       }
calcSum :: Camera -> (CInt, CInt) -> (CInt, CInt)
calcSum (SDL.Rectangle (P(V2 camX camY)) _) (x, y) =
  ((camX + x),(camY + y))

gameLoop :: IORef EngineState -> IORef GameState -> Double -> IO ()
gameLoop es gs timeStep = do
  engineState <- readIORef es
  gameState <- readIORef gs
  let activeKeys = keys engineState
      playerMod = W.heroM $ getPlayer gameState
      levelDims = levelInfo gameState
      position = modelPosition activeKeys (renPos playerMod) timeStep --beirze z modela
 -- check for collisions? + react
      --(x, y) = modelPosition activeKeys (W.pPos (getPlayer gameState)) timeStep
      model' = modifyModelPos playerMod position levelDims
               (camera engineState)
      cam' = calcCameraPosition (camera engineState) model'
             (getLevelSize gameState)
      correctCam = checkOffset (camera engineState) cam'
      tileslvl = changeTilesLvl gameState correctCam
      tiles = updateMap gameState tileslvl
      world = changeWorld gameState correctCam model' position
  D.traceIO(show (calcSum cam' position))
  writeIORef gs (modifyGameState tiles world gameState)
  writeIORef es engineState{camera = cam'}
