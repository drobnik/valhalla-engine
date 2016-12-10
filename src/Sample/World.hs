module World where

import Data.Int
import Data.Map
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
              , value :: Int --what about the gate?
              , pos :: (Int32, Int32)
              , kind :: a
              , model :: RenderModel
              }

runWorld = undefined
