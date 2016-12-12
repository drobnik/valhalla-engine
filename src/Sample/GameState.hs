module GameState where

import Data.Map as Map
import Render.Model (modifyModelPos, renPos
                    , RenderModel(..), dummyModel
                    , sampleSet)
import Engine.Datas
import Engine.Consts
import Data.IORef
import Foreign.C.Types (CInt(..))
import GameData
import qualified World as W
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

getWorldModels :: GameState -> [RenderModel]
getWorldModels (GameState _ wor _ _) = W.renderWorld wor

getModelKey :: Int -> Map Int RenderModel-> RenderModel
getModelKey n modMap = case Map.lookup n modMap of
  Just m -> m
  Nothing -> dummyModel

modifyModelsSet :: Map Int RenderModel -> RenderModel
                -> Int -> GameState
                -> GameState
modifyModelsSet modMap rm n (GameState lvl wor maps' models) = GameState
                                                         { level = lvl
                                                         , world = wor
                                                         , maps = maps'
                                                         , modelsSet = modMap'
                                                         }
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

gameLoop :: IORef EngineState -> IORef GameState -> IO ()
gameLoop es gs = do
  engineState <- readIORef es
  gameState <- readIORef gs
  let (GameState lvl world maps mSets) = gameState
      activeKeys = getKeys engineState
      models = getModelsSet gameState
      model = getModelKey heroKey models
      position = modelPosition activeKeys (renPos model)
      model' = modifyModelPos model position
      cam' = calcCameraPosition (getCamera engineState) model' (getLevelSize gameState)
  writeIORef gs (modifyModelsSet models model' heroKey gameState)
  D.trace (show cam') (writeIORef es engineState{camera = cam'})
