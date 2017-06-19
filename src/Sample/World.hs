module World where

import Data.Int
import Data.Map (Map(..), fromList, elems)
import qualified Data.Map.Strict as Map (map, Map(..), lookup,
                                         insert, empty, size)
import qualified Data.Set as S (null)
import Foreign.C.Types(CInt(..))
import SDL(V2(..), Point(P))
import qualified SDL (Rectangle(..))
import Render.Model (RenderModel (..), modifyPos, Camera(..))
import qualified Render.Model as RM (pos)
import Engine.Collision (Collidable(..), makeBox, BoundingBox(BoundingBox)
                        , BoxKind(..))
import Engine.Datas
import Engine.Consts
import Engine.Collision
import GameData

import qualified Debug.Trace as D
data World = World
           { level :: Map Int Level
           , playerLives :: Int
           , player :: Player --play
           , wholeScore :: Int
           }

data EntityType = Collect | Gate | BonusH
  deriving (Show, Eq, Ord)

data Player = Player
            { pDim :: (Int32, Int32)
            , lives :: Int
            , pPos :: (Double, Double)
            , pBox :: BoundingBox
            , heroM :: RenderModel
            , harm :: Bool
          --  , velocity :: (Float, Float) --velX, velY
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
                      { level = Map.insert 1 (sampleLevel entities) Map.empty
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
renderWorld (World levels' _ player' _) = renderLevels levels' 1
                                                      [heroM player']

renderLevels :: Map Int Level -> Int -> [RenderModel] -> [RenderModel]
renderLevels levels lvlInc models = if lvlInc < Map.size levels
                                    then
                                      renderLevels levels (lvlInc+1) (mod ++ models)
                                    else
                                      models
  where mod = getLvlModels lvlM
        lvlM = case Map.lookup lvlInc levels of
                 Just lvl' -> lvl'
                 Nothing -> error "Models for the level not found"

getLvlModels :: Level -> [RenderModel]
getLvlModels (Level cosMap _ _ _) = map World.model (elems cosMap)

-- add update for velocity
updatePlayer :: Player -> RenderModel -> (Double, Double) -> Player
updatePlayer old@(Player (w, h) _ _ _ _ _) rm po@(x, y) = old
                                                        {pPos = po
                                                        , heroM = rm
                                                        , pBox = makeBox
                                                          (floor x) (floor y) w h}

updateWorld :: World -> Camera -> Player -> Int -> World
updateWorld (World lvls liv p scr) cam play num = (World lvls' liv play scr)
  where
    updated = changeLevel lvls cam num
    lvls' = updateLevels lvls updated num

updateLevels :: Map Int Level -> Level -> Int -> Map Int Level
updateLevels lvlmaps upLvl num = Map.insert (num-1) upLvl lvlmaps --or num?

  {-take (num-1) lvlmaps ++ upLvl ++ drop (num+1) lvlmaps-}

changeLevel :: Map Int Level -> Camera -> Int -> Level
changeLevel level cam lvl = changeUnits cam lvlForUpdate
  where lvlForUpdate = case Map.lookup lvl level of
          Just lvl -> lvl
          Nothing -> error "Contents for this level not found!"

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
modifyEntities cam collectables = Map.map (changeEnt cam) collectables

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

getEntitiesBoxes ::  World -> Int -> [(BoundingBox, BoxKind)]
getEntitiesBoxes (World levels _ _ _) lvl = zip boxes kinds
  where entities' = elems (collectables level)
        level = case Map.lookup (lvl-1) levels of
          Just level' -> level'
          Nothing -> error "Level with such index not found"
        (boxes, kinds) = enBoxKind entities' ([], [])

playerBox :: World -> (BoundingBox, BoxKind)
playerBox (World _ _ player _) = (pBox player, CollPlayer)

enBoxKind :: [Entity] -> ([BoundingBox], [BoxKind])
          -> ([BoundingBox], [BoxKind])
enBoxKind (e:es) (box, kind) = enBoxKind es ((box':box), (kind':kind))
  where (box', kind') = mapBox e
enBoxKind [] boxes = boxes

heal :: Player -> Player
heal p@(Player _ l _ _ _ ha) = p{ lives = l + 1
                                , harm = False
                                }
loseLife :: Player -> Player
loseLife p@(Player _ lives' _ _ _ _) --later: check harm
  | lives' > 0 = p {lives = lives' - 1}
  | otherwise = p

mapBox :: Entity -> (BoundingBox, BoxKind)
mapBox (Entity _ _ _ kind box _) = (box, check kind)
  where check k = case k of
                    BonusH -> CollHealth
                    Collect -> CollCoin
                    Gate -> CollGate

runWorld :: ActiveKeys -> Double -> Camera -> TileMap -> World
         -> WinSetup -> [(BoundingBox, BoxKind)] -> (Camera, TileMap, World)
runWorld keys' dt oldCam oldTiles oldWorld winSet boundingBoxes
  | S.null keys' = (oldCam, oldTiles, oldWorld)
  | otherwise = let
      tree = insertElements boundingBoxes (newQuadtree 1 winSet)
      updatedCam = undefined
      updatedTileMap = undefined
      updatedWorld = undefined
      in (updatedCam, updatedTileMap, updatedWorld)
