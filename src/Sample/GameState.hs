{-# LANGUAGE RankNTypes #-}

module GameState where

import Data.STRef
import Control.Monad.ST
import Data.Map as Map
import Graphics.UI.GLUT
import Render.Model
import Engine.Datas

data GameState = GameState
               { lifes :: Int
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

getModels :: (forall s. ST s (Map Int RenderModel)) -> Map Int RenderModel
getModels models = runST models

initStateG :: GameState
initStateG = GameState {lifes = 1, level = 1, modelsSet = initModels}

initModels :: ST s (Map Int RenderModel)
initModels = do
  renderMap <- newSTRef sampleSet
  readSTRef renderMap
