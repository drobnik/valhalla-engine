module GameState where

import Data.Map as Map
import Render.Model
import Engine.Datas
--import Engine.Loader (loadModels)

data GameState = GameState
               { lives :: Int
               , level :: Int
               -- , world :: World -- world entities with actors inluding renderModels
               -- , map :: Tiles -- tiles to render WTF to tutaj?
               , modelsSet :: Map Int RenderModel
               }

--rename
getModelsSet :: GameState -> Map Int RenderModel
getModelsSet (GameState _ _ mod) = mod

getModelKey :: Int -> Map Int RenderModel-> RenderModel
getModelKey n modMap = case Map.lookup n modMap of
  Just m -> m
  Nothing -> dummyModel

modifyModelsSet :: Map Int RenderModel -> RenderModel
                -> Int -> GameState
                -> GameState
modifyModelsSet modMap rm n (GameState liv lvl models) = GameState
                                                         { lives = liv
                                                         , level = lvl
                                                         , modelsSet = modMap'
                                                         }
  where modMap' = Map.insert n rm modMap

--later: read from json config file maybe?
initStateG :: GameState
initStateG = GameState { lives = 1
                       , level = 1
                       , modelsSet = sampleSet
                       }
