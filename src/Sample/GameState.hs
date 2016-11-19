module GameState where

import Data.Set
import Render.Primitives

data GameState = GameState
               { lifes :: Int
               , level :: Int
               -- , world :: World -- world entities with actors inluding renderModels
               -- , map :: Tiles -- tiles to render
               --  , renderModels :: Set RenderModel
               }

initStateG :: GameState
initStateG = GameState {lifes = 1, level = 1{-, renderModels = dummyModels-}}
