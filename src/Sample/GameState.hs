module GameState where

import Data.Map as Map
import Render.Model (modifyModelPos, renPos
                    , RenderModel(..), dummyModel
                    , sampleSet)
import Engine.Datas
import Engine.Consts
import Data.IORef
import GameData

data GameState = GameState
                 { lives :: Int
                 , level :: Int
                 -- , world :: World
                 , maps :: [TileMap TileKind] -- level maps
                 , modelsSet :: Map Int RenderModel
                 }

--rename
getModelsSet :: GameState -> Map Int RenderModel
getModelsSet (GameState _ _ _ mod) = mod

getTilesModels :: GameState -> [RenderModel]
getTilesModels (GameState _ lvl maps _ ) = getModels (getTiles (maps !! lvl)) []

getModelKey :: Int -> Map Int RenderModel-> RenderModel
getModelKey n modMap = case Map.lookup n modMap of
  Just m -> m
  Nothing -> dummyModel

modifyModelsSet :: Map Int RenderModel -> RenderModel
                -> Int -> GameState
                -> GameState
modifyModelsSet modMap rm n (GameState liv lvl maps' models) = GameState
                                                         { lives = liv
                                                         , level = lvl
                                                         , maps = maps'
                                                         , modelsSet = modMap'
                                                         }
  where modMap' = Map.insert n rm modMap

--later: read from json config file maybe?
initStateG :: GameState
initStateG = GameState { lives = 1
                       , level = 1
                       , maps = []
                       , modelsSet = sampleSet
                       }
gameLoop :: IORef EngineState -> IORef GameState -> IO ()
gameLoop es gs = do
  engineState <- readIORef es
  gameState <- readIORef gs
  let activeKeys = getKeys engineState
      models = getModelsSet gameState
      model = getModelKey heroKey models
      position = modelPosition activeKeys (renPos model)
      model' = modifyModelPos model position
  writeIORef gs (modifyModelsSet models model' heroKey gameState)
