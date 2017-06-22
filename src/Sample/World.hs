-- | World module provides definitions for basic game entities and supporting functions
-- for loading and updating purposes.
module World
  (
    -- * Game World Types
    World(..)
  , Player(..)
  , Entity(..)
  , EntityType(..)
    -- * Game World Management
  , updateWorld
  , renderWorld
  , setupWorld
  , runWorld
  , updatePlayer
    -- * Miscellaneous
  , getEntitiesBoxes
  , playerBox
  ) where

import Data.Int
import Foreign.C.Types (CInt(..))
import Data.Map.Strict ( Map(..)
                       , fromList
                       , elems)
import qualified Data.Map.Strict as Map ( map
                                        , lookup
                                        , insert
                                        , empty
                                        , size
                                        , singleton)
import qualified Data.Set as Set (null)

import SDL(V2(..)
          , Point(P))
import qualified SDL (Rectangle(..))

import Render.Model (RenderModel (..)
                    , modifyPos
                    , Camera(..))
import qualified Render.Model as RM (pos)
import Engine.Collision
import Engine.Datas
import Engine.Consts
import Engine.Collision
import GameData


-- | Data which stores a list of levels and player information such as life
-- counter and overall score in a game.
data World = World
           { level        :: Map Int Level
           , playerLives  :: Int
           , player       :: Player
           , wholeScore   :: Int
           }

data EntityType = Collect | Gate | BonusH
  deriving (Show, Eq, Ord)

-- | Data representing player entity. Stores information about life status
-- or harm done by game environment.
data Player = Player
            { pDim  :: (Int32, Int32)
            , lives :: Int
            , pPos  :: (Double, Double)
            , pBox  :: BoundingBox
            , heroM :: RenderModel
            , harm  :: Bool
            } deriving (Eq, Ord)

instance Collidable Player where
  boundingBox = pBox

-- |  Data which describes a state of a part of the game with
-- a map of level entities, how many points has player earned
-- and information about the level completion.
data Level = Level
             { collectables :: Map Int Entity
             , score        :: Int             -- ^ Score for current level
             , scoreToEnd   :: Int
             , isGateOpen   :: Bool            -- ^ Flag to indicate scoring 'scoreToEnd'
             }

-- | Data which stores information used for collision detection,
-- a BoundingBox reference calculated using its position and dimensions,
-- a value which is added to player's score on the first collision and graphic
-- representation of a type in 'RenderModel' reference.
data Entity = Entity
              { dim    :: (Int32, Int32)
              , value  :: Int
              , pos    :: (Int32, Int32)
              , kind   :: EntityType
              , eBox   :: BoundingBox
              , model  :: RenderModel
              } deriving (Eq, Ord, Show)

instance Collidable Entity where
  boundingBox = eBox

-- | Initialize game world with a list of level entities already created.
setupWorld :: [Entity] -> Player -> World
setupWorld entities p = World
                      { level = Map.singleton 1 (sampleLevel entities)
                      , playerLives = 3
                      , player = p
                      , wholeScore = 0
                      }

-- | Initialize dummy level with 18 entities, indexed from 1 and 80 points
-- to collect in order to finish it.
sampleLevel :: [Entity] -> Level
sampleLevel entities = Level
                       { collectables = fromList (zip [1..18] entities)
                       , score = 0
                       , scoreToEnd = 80
                       , isGateOpen = False
                       }

-- | Return a list of 'RenderModel' references for currently played level.
renderWorld :: World -> Int -> [RenderModel]
renderWorld (World levels' _ player' _) lvl = renderLevels levels' lvl
                                                      [heroM player']

-- | Combine all 'RenderModel's from level maps.
renderLevels :: Map Int Level -> Int -> [RenderModel] -> [RenderModel]
renderLevels levels lvlInc models = if lvlInc <= Map.size levels
                                    then
                                      renderLevels levels (lvlInc+1) (mod ++ models)
                                    else
                                      models
  where mod = getLvlModels lvlM
        lvlM = case Map.lookup lvlInc levels of
                 Just lvl' -> lvl'
                 Nothing   -> error "Models for the level not found"

-- | Return a list of all 'RenderModel' referneces of collectable entities.
getLvlModels :: Level -> [RenderModel]
getLvlModels (Level cosMap _ _ _) = map World.model (elems cosMap)

-- | Update player position and 'RenderModel' with relative offset
-- calculated ealier.
updatePlayer :: Player -> RenderModel -> (Double, Double) -> Player
updatePlayer old@(Player (w, h) _ _ _ _ _) rm po@(x, y) = old
                                                        {
                                                          pPos  = po
                                                        , heroM = rm
                                                        , pBox  = makeBox
                                                                  (floor x)
                                                                  (floor y) w h
                                                        }

-- | Apply update functions on levels in the game world.
updateWorld :: World  -- ^ Outdated 'World' reference
            -> Camera -- ^ Camera with an offset
            -> Player -- ^ Player entity to be updated
            -> Int    -- ^ Number of current level (from 'GameState')
            -> World  -- ^ World with updated levels
updateWorld (World lvls liv p scr) cam play num = (World lvls' liv play scr)
  where
    updated = changeLevel lvls cam num
    lvls' = updateLevels lvls updated num

-- | Insert an updated level.
updateLevels :: Map Int Level -> Level -> Int -> Map Int Level
updateLevels lvlmaps upLvl num = Map.insert num upLvl lvlmaps

-- | Apply 'changeUnits' to the level.
changeLevel :: Map Int Level -> Camera -> Int -> Level
changeLevel level cam lvl = changeUnits cam lvlForUpdate
  where lvlForUpdate = case Map.lookup lvl level of
                         Just lvl' -> lvl'
                         Nothing   -> error "Contents for this level not found!"

-- | Change a relative position of entities in current level.
changeUnits :: Camera -> Level -> Level
changeUnits cam (Level coll se' toEnd isOpen) = Level
                                                {collectables =
                                                    (modifyEntities cam coll)
                                                , score = se'
                                                , scoreToEnd = toEnd
                                                , isGateOpen = isOpen
                                                }

-- | Map 'changeEnt' funtion to every collectable entity.
modifyEntities :: Camera -> Map Int (Entity) -> Map Int (Entity)
modifyEntities cam collectables = Map.map (changeEnt cam) collectables

-- | Constuct a new entity based on 'Camera' offset.
changeEnt :: Camera -> Entity -> Entity
changeEnt (SDL.Rectangle (P(V2 camX camY)) s)
  (Entity d val p k bbox rm@(RenderModel _ (x, y) _ _ _ instr)) =
  Entity
  { World.dim = d
  , value = val
  , World.pos = p
  , World.kind = k
  , eBox = bbox
  , World.model = rm{ RM.pos = (CInt x', CInt y')
                    , renderInstr = modifyPos instr [] (CInt x', CInt y')
                    }
  }
  where (x', y') = (fromIntegral (x - camX), fromIntegral (y - camY))

-- | Return 'BoundingBox'es of entites in current level.
getEntitiesBoxes ::  World -> Int -> [(BoundingBox, BoxKind)]
getEntitiesBoxes (World levels _ _ _) lvl = zip boxes kinds
  where entities' = elems (collectables level)
        level = case Map.lookup (lvl-1) levels of
                  Just level' -> level'
                  Nothing     -> error "Level with such index not found"
        (boxes, kinds) = enBoxKind entities' ([], [])

-- | Return 'BoundingBox' of a player.
playerBox :: World -> (BoundingBox, BoxKind)
playerBox (World _ _ player _) = (pBox player, CollPlayer)

-- | Recurisively apply function for returing 'BoundingBox'es.
enBoxKind :: [Entity] -> ([BoundingBox], [BoxKind])
          -> ([BoundingBox], [BoxKind])
enBoxKind (e:es) (box, kind) = enBoxKind es ((box':box), (kind':kind))
  where (box', kind') = mapBox e
enBoxKind [] boxes = boxes

-- | Increment life counter and return a new 'Player' reference.
heal :: Player -> Player
heal p@(Player _ l _ _ _ ha) = p{ lives = l + 1
                                , harm = False
                                }

-- | Increment life counter and return a new 'Player' reference.
loseLife :: Player -> Player
loseLife p@(Player _ lives' _ _ _ _)
  | lives' > 0 && harm p = p {lives = lives' - 1}
  | otherwise = p

-- | Create a tuple to insert an entity in 'Quadtree'.
mapBox :: Entity -> (BoundingBox, BoxKind)
mapBox (Entity _ _ _ kind box _) = (box, check kind)
  where check k = case k of
                    BonusH -> CollHealth
                    Collect -> CollCoin
                    Gate -> CollGate

-- | Core function in 'World' definition. Apply updates on 'Camera',
-- current 'TileMap' and 'World' when any input is recieved.
runWorld :: ActiveKeys               -- ^ Set of pressed keys
         -> Double                   -- ^ Delta time
         -> Camera                   -- ^ Camera from previous frame
         -> TileMap                  -- ^ Tile map for current level
         -> World                    -- ^ Old world
         -> WinSetup                 -- ^ Window for any 'WindowManager' updates
         -> [(BoundingBox, BoxKind)] -- ^ Current Quad tree
         -> (Camera, TileMap, World) -- ^ Updated game objects
runWorld keys' dt oldCam oldTiles oldWorld winSet boundingBoxes
  | Set.null keys' = (oldCam, oldTiles, oldWorld) -- ^ Nothing changes
  | otherwise = let
      tree = insertElements boundingBoxes (newQuadtree 1 winSet)
      updatedCam = undefined
      updatedTileMap = undefined
      updatedWorld = undefined
      in (updatedCam, updatedTileMap, updatedWorld)
