module GameState where

import qualified Data.Map as Map
import Data.Map (Map(..))
import Data.List((\\))
import Render.Model (modifyModelPos, renPos
                    , RenderModel(..), dummyModel
                    , sampleSet, addCameraOffset
                    , calcCameraPosition, Camera, modifyPos
                    , checkOffset)
import qualified Render.Model as RM (pos, dim)
import Engine.Datas
import Engine.Consts
import Data.IORef
import Foreign.C.Types (CInt(..))
import GameData
import qualified GameData as GD(dim, pos)
import qualified World as W
import qualified SDL (Rectangle(..))
import SDL (Point(P), V2(..))
import qualified Debug.Trace as D

data GameState = GameState
                 { level :: Int
                 , world :: W.World
                 , maps :: [TileMap TileKind] -- level maps
                 , modelsSet :: Map Int RenderModel
                 --isOver :: Bool
                 }

--rename
getModelsSet :: GameState -> Map Int RenderModel
getModelsSet (GameState _ _ _ mod) = mod

getTilesModels :: GameState -> [RenderModel]
getTilesModels (GameState lvl _ maps _ ) = getModels (getTiles (maps !! lvl)) []

updateMap :: GameState -> [TileMap TileKind] -> [TileMap TileKind]
updateMap (GameState lvl _ maps _) tiles = take lvl maps ++ tiles
                                           ++ drop (lvl+1) maps

changeLevel :: GameState -> Camera -> [TileMap TileKind]
changeLevel (GameState lvl _ maps _) cam = [changeTiles cam (maps !! lvl)]

changeTiles :: Camera -> TileMap TileKind -> TileMap TileKind
changeTiles cam tileMap = map''
  where
        (TileMap w h tiles' pat) = tileMap
        map'' = TileMap w h (map (changeTile cam) tiles') pat

--cleanup
changeTile :: Camera -> Tile TileKind -> Tile TileKind
changeTile  (SDL.Rectangle (P(V2 camX camY)) _)
  (Tile d p k rm@(RenderModel _ (x, y) _ _ _ instr)) =
   Tile { GD.dim = d, GD.pos = (x', y'), kind = k, model = rm
                                                           { RM.pos = (CInt x', CInt y')
                                                           , renderInstr = modifyPos
                                                             instr [] (CInt x', CInt y')
                                                           }}
  where (x', y') = (fromIntegral (x - camX), fromIntegral (y - camY))

getWorldModels :: GameState -> [RenderModel]
getWorldModels (GameState _ wor _ _) = W.renderWorld wor

getModelKey :: Int -> Map Int RenderModel-> RenderModel
getModelKey n modMap = case Map.lookup n modMap of
  Just m -> m
  Nothing -> dummyModel

modifyModelsSet :: Map Int RenderModel -> RenderModel
                -> Int -> [TileMap TileKind] -> GameState
                -> GameState
modifyModelsSet modMap rm n tileCam (GameState lvl wor maps' models) =
  GameState{ level = lvl, world = wor, maps = tileCam, modelsSet = modMap'}
  where modMap' = Map.insert n rm modMap

getLevelSize :: GameState -> (CInt, CInt)
getLevelSize (GameState lvl _ maps _) =
  let (TileMap w h tiles' tilesP) = maps !! lvl
      in (CInt(fromIntegral w), CInt (fromIntegral h))

--later: read from json config file maybe?
initStateG :: GameState
initStateG = GameState { level = 1
                       , world = undefined
                       , maps = []
                       , modelsSet = sampleSet
                       }
hasSomethingChanged :: GameState -> [TileMap TileKind] -> Bool
hasSomethingChanged (GameState lvl _ maps _) tils =
  let (TileMap _ _ tiles' _) = maps !! lvl
      (TileMap _ _ change' _) = head tils
      in not (null (change' \\ tiles'))

gameLoop :: IORef EngineState -> IORef GameState -> IO ()
gameLoop es gs = do
  engineState <- readIORef es
  gameState <- readIORef gs
  let
      activeKeys = getKeys engineState
      models = getModelsSet gameState
      model = getModelKey heroKey models
      position = modelPosition activeKeys (renPos model)
      model' = modifyModelPos model position
      cam' = calcCameraPosition (getCamera engineState) model' (getLevelSize gameState)
      model'' = addCameraOffset model' cam'
      level = changeLevel gameState (checkOffset (getCamera engineState) cam')
      tiles = updateMap gameState level

  writeIORef gs (modifyModelsSet models model' heroKey tiles gameState)
--  D.traceIO (show model'')
 -- D.traceIO ("to drugie:" ++ show model')
  writeIORef es engineState{camera = cam'}
