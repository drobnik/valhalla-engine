module GameState where

import Data.Set
import Render.Model
import Engine.Datas

data GameState = GameState
               { lifes :: Int
               , level :: Int
               -- , world :: World -- world entities with actors inluding renderModels
               -- , map :: Tiles -- tiles to render
               , modelsSet :: Set RenderModel -- wyciagniety ze swiata
               -- musi ogarniac sam, co jest widoczne i dawac do zbioru
               }

instance GState GameState where
  listOfModels (GameState _ _ models) = toList models

initStateG :: GameState
initStateG = GameState {lifes = 1, level = 1, modelsSet = sampleSet}
