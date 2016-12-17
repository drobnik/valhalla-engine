module World where

import Data.Int
import Data.Map (Map(..), fromList, elems)
import qualified Data.Map as M (map)
import Foreign.C.Types(CInt(..))
import SDL(V2(..), Point(P))
import qualified SDL (Rectangle(..))
import Render.Model (RenderModel (..), modifyPos, Camera(..))
import qualified Render.Model as RM (pos)
import Engine.Collision (Collidable(..), makeBox, BoundingBox(BoundingBox))
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
  deriving (Show, Eq, Ord)

data Player = Player
            { pDim :: (Int32, Int32)
            , lives :: Int
            , pPos :: (Double, Double)
            , pBox :: BoundingBox
            , heroM :: RenderModel
            } deriving (Eq, Ord)

instance Collidable Player where
  boundingBox = pBox

data Level = Level
             { collectables :: Map Int Entity
             , score :: Int
             , scoreToEnd :: Int
             , isGateOpen :: Bool
             }

instance Show Level where
  show (Level col _ _ _) = show (elems col)

data Entity = Entity
              { dim :: (Int32, Int32)
              , value :: Int
              , pos :: (Int32, Int32)
              , kind :: EntityType
              , eBox :: BoundingBox
              , model :: RenderModel
              } deriving (Eq, Ord, Show)

instance Collidable Entity where
  boundingBox = eBox

setupWorld :: [Entity] -> Player -> World
setupWorld entities p = World
                      { level = [sampleLevel entities]
                      , playerLives = 3
                      , player = p
                      , wholeScore = 0
                      }

sampleLevel :: [Entity] -> Level
sampleLevel entities = Level
                       { collectables = fromList (zip [1..18] entities)
                       , score = 0
                       , scoreToEnd = 80
                       , isGateOpen = False
                       }

renderWorld :: World -> [RenderModel]
renderWorld (World levels' _ player' _) = renderLevels levels'
                                                      [heroM player']

renderLevels :: [Level] -> [RenderModel] -> [RenderModel]
renderLevels (x:xs) models = renderLevels xs (mod ++ models)
  where mod = getLvlModels x
renderLevels [] models = models

getLvlModels :: Level -> [RenderModel]
getLvlModels (Level cosMap _ _ _) = map World.model (elems cosMap)

updatePlayer :: Player -> RenderModel -> (Double, Double) -> Player
updatePlayer old@(Player (w, h) _ _ _ _) rm po@(x, y) = old
                                                        {pPos = po
                                                        , heroM = rm
                                                        , pBox = makeBox
                                                          (floor x) (floor y) w h}

updateWorld :: World -> Camera -> Player -> Int -> World
updateWorld (World lvls liv p scr) cam play num = (World lvls' liv play scr)
  where
    updated = changeLevel lvls cam num
    lvls' = updateLevels lvls updated num

updateLevels :: [Level] -> [Level] -> Int -> [Level]
updateLevels lvlmaps upLvl num = take (num-1) lvlmaps ++ upLvl
                                 ++ drop (num+1) lvlmaps

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

modifyEntities :: Camera -> Map Int (Entity)
               -> Map Int (Entity)
modifyEntities cam collectables = M.map (changeEnt cam) collectables

changeEnt :: Camera -> Entity -> Entity
changeEnt (SDL.Rectangle (P(V2 camX camY)) s) (Entity d val p k bbox
                                             rm@(RenderModel _ (x, y) _
                                                 _ _ instr)) =
  Entity {World.dim = d, value = val, World.pos = p,
       World.kind = k, eBox = bbox, World.model = rm{ RM.pos = (CInt x', CInt y')
                                                    , renderInstr = modifyPos
                                                      instr [] (CInt x', CInt y')
                                                    }}
  where (x', y') = (fromIntegral (x - camX), fromIntegral (y - camY))

getCollidablesWorld ::  World -> Int -> [Entity]
getCollidablesWorld (World levels _ _ _) lvl = getCollidablesLvl (levels !! lvl)

getCollidablesLvl :: Level -> [Entity]
getCollidablesLvl (Level colls _ _ _) = elems colls

getSome :: (Collidable a) => Map Int Entity -> [a]
getSome colls = undefined

runWorld = undefined
