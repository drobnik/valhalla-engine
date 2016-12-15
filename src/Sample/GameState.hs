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
getTilesModels (GameState lvl _ maps) = getModels (getTiles (maps !! lvl)) []

--change
changeWorld :: GameState -> Camera -> RenderModel -> W.World
changeWorld (GameState lvl world _) cam playerM = W.updateWorld
                                                    world cam playerM lvl

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
  (Tile d p k rm@(RenderModel _ (x, y) _ _ _ instr)) =
   Tile { GD.dim = d, GD.pos = p, kind = k, model = rm
                                                    { RM.pos = (CInt x', CInt y')
                                                    , renderInstr = modifyPos
                                                      instr [] (CInt x', CInt y')
                                                    }}
  where (x', y') = (fromIntegral (x - camX), fromIntegral (y - camY))

getWorldModels :: GameState -> [RenderModel]
getWorldModels (GameState _ wor _) = W.renderWorld wor

getPlayer :: GameState -> W.Player
getPlayer (GameState _ (W.World _ _ p _) _) = p

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

gameLoop :: IORef EngineState -> IORef GameState -> Double -> IO ()
gameLoop es gs timeStep = do
  engineState <- readIORef es
  gameState <- readIORef gs

  let activeKeys = getKeys engineState
      playerMod = W.getPlayerMod $ getPlayer gameState
      levelDims = levelInfo gameState
      position = modelPosition activeKeys (renPos playerMod) timeStep
      model' = modifyModelPos playerMod position levelDims
               (getCamera engineState)
      cam' = calcCameraPosition (getCamera engineState) model'
             (getLevelSize gameState)
      correctCam = checkOffset (getCamera engineState) cam'
      tileslvl = changeTilesLvl gameState correctCam
      tiles = updateMap gameState tileslvl
      world = changeWorld gameState correctCam model'
  writeIORef gs (modifyGameState tiles world gameState)
  writeIORef es engineState{camera = cam'}
