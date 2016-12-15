module World where

import Data.Int
import Data.Map (Map(..), fromList, elems)
import qualified Data.Map as M (map)
import Foreign.C.Types(CInt(..))
import SDL(V2(..), Point(P))
import qualified SDL (Rectangle(..))
import Render.Model (RenderModel (..), modifyPos, Camera(..))
import qualified Render.Model as RM (pos)
import Engine.Datas
import Engine.Consts
import GameData

import qualified Debug.Trace as D
data World = World
           { level :: [Level]
           , playerLives :: Int
           , player :: Player --play
           , wholeScore :: Int
           }

data EntityType = Collect | Gate
  deriving Show

data Player = Player
            { pDim :: (Int32, Int32)
            , lives :: Int
            , pPos :: (Double, Double)
--            , pBox :: BoundingBox
            , heroM :: RenderModel
            }

data Level = Level
             { collectables :: Map Int (Entity EntityType)
             , score :: Int
             , scoreToEnd :: Int
             , isGateOpen :: Bool
             }

instance Show Level where
  show (Level col _ _ _) = show (elems col)

data Entity a = Entity
                { dim :: (Int32, Int32)
                , value :: Int
                , pos :: (Int32, Int32)
                , kind :: a
                , model :: RenderModel
                } deriving Show

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
getLvlModels (Level cosMap _ _ _) = map World.model (elems cosMap)

updatePlayer :: Player -> RenderModel -> Player
updatePlayer old rm@(RenderModel _ (x, y) _  _ _ _) = old{pPos = po', heroM = rm}
  where po' = (fromIntegral x, fromIntegral y)

-- add update for player
updateWorld :: World -> Camera -> RenderModel -> Int -> World
updateWorld (World lvls liv p scr) cam pModel num = (World lvls' liv pi scr)
  where
    updated = changeLevel lvls cam num
    lvls' = updateLevels lvls updated num
    pi = updatePlayer p pModel

-- wina tego dziela
updateLevels :: [Level] -> [Level] -> Int -> [Level]
updateLevels lvlmaps upLvl num = take (num-1) lvlmaps ++ upLvl
                                 ++ drop (num+1) lvlmaps

-- tutaj sie wywala!
changeLevel :: [Level] -> Camera -> Int -> [Level]
changeLevel level cam lvl = [changeUnits cam (level !! (lvl - 1))]

changeUnits :: Camera -> Level -> Level
changeUnits cam (Level coll se' toEnd isOpen) = Level{ collectables =
                                                          (modifyEntities
                                                           cam coll)
                                                        , score = se'
                                                        , scoreToEnd = toEnd
                                                        , isGateOpen = isOpen
                                                        }

modifyEntities :: Camera -> Map Int (Entity EntityType)
               -> Map Int (Entity EntityType)
modifyEntities cam collectables = M.map (changeEnt cam) collectables

changeEnt :: Camera -> Entity EntityType -> Entity EntityType
changeEnt (SDL.Rectangle (P(V2 camX camY)) s) (Entity d val p k
                                             rm@(RenderModel _ (x, y) _
                                                 _ _ instr)) =
  Entity {World.dim = d, value = val, World.pos = p,
       World.kind = k, World.model = rm{ RM.pos = (CInt x', CInt y')
                             , renderInstr = modifyPos
                                             instr [] (CInt x', CInt y')
                             }}
  where (x', y') = (fromIntegral (x - camX), fromIntegral (y - camY))


runWorld = undefined
