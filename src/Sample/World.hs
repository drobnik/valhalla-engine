module World where

import Data.Int
import Data.Map (Map(..), fromList, elems)
import Render.Model (RenderModel (..))
import Engine.Datas
import Engine.Consts
import GameData


data World = World
           { level :: [Level]
           , playerLives :: Int
           , player :: Player
           , wholeScore :: Int
           }

data EntityType = Collect | Gate

data Player = Player
            { pDim :: (Int32, Int32)
            , lives :: Int
            , pPos :: (Int32, Int32)
            , heroM :: RenderModel
            }

data Level = Level
           { collectables :: Map Int (Entity EntityType)
           , score :: Int
           , scoreToEnd :: Int
           , isGateOpen :: Bool
           }

data Entity a = Entity
              { dim :: (Int32, Int32)
              , value :: Int
              , pos :: (Int32, Int32)
              , kind :: a
              , model :: RenderModel
              }

setupWorld :: [Entity EntityType] -> Player -> World
setupWorld entities p = World
                      { level = [sampleLevel entities]
                      , playerLives = 3
                      , player = p
                      , wholeScore = 0
                      }

sampleLevel :: [Entity EntityType] -> Level
sampleLevel entities = Level
                       { collectables = fromList (zip [1..18] entities)
                       , score = 0
                       , scoreToEnd = 80
                       , isGateOpen = False
                       }

renderWorld :: World -> [RenderModel]
renderWorld (World levels' _ (Player _ _ _ hero) _) = renderLevels levels' [hero]

renderLevels :: [Level] -> [RenderModel] -> [RenderModel]
renderLevels (x:xs) models = renderLevels xs (mod ++ models)
  where mod = getLvlModels x
renderLevels [] models = models

getLvlModels :: Level -> [RenderModel]
getLvlModels (Level cosMap _ _ _) = map getRenders (elems cosMap)

getRenders :: Entity EntityType -> RenderModel
getRenders (Entity _ _ _ _ mod) = mod

runWorld = undefined
