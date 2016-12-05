{-# LANGUAGE RankNTypes #-}

module GameState where

import Data.STRef
import Control.Monad.ST
import Data.Map as Map
import Graphics.Rendering.OpenGL
import Render.Model
import Engine.Datas

data GameState = GameState
               { lives :: Int
               , level :: Int
               -- , world :: World -- world entities with actors inluding renderModels
               -- , map :: Tiles -- tiles to render WTF to tutaj?
               , modelsSet :: forall s. ST s (Map Int RenderModel)
               }

--rename
getModelsSet :: GameState -> Map Int RenderModel
getModelsSet (GameState _ _ mod) = getModels mod

emptyModels :: ST s (Map Int RenderModel)
emptyModels = do
  renderMap <- newSTRef Map.empty
  readSTRef renderMap


getModelKey :: Int -> Map Int RenderModel-> RenderModel
getModelKey n modMap = case Map.lookup n modMap of
  Just m -> m
  Nothing -> dummyModel

getModels :: (forall s. ST s (Map Int RenderModel)) -> Map Int RenderModel
getModels models = runST models

modifyModelsSet :: Map Int RenderModel -> RenderModel
                -> Int -> GameState
                -> GameState
modifyModelsSet modMap rm n (GameState liv lvl models) = GameState
                                                         { lives = liv
                                                         , level = lvl
                                                         , modelsSet = modMap'
                                                         }
  where modMap' = do
          modsMap <- newSTRef (Map.insert n rm modMap) --bez sensu taki ST
          readSTRef modsMap


initStateG :: GameState
initStateG = GameState {lives = 1, level = 1, modelsSet = initModels}

initModels :: ST s (Map Int RenderModel)
initModels = do
  renderMap <- newSTRef sampleSet
  readSTRef renderMap
