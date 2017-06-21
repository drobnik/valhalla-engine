{-# LANGUAGE BangPatterns #-}

-- | Say something nice about this module
--
module GameData
  (
    -- * Tile-specific Types
    TileMap(..)
  , Tile(..)
  , TileKind(..)
  , UnitKind(..)
    -- * 'RenderModel' Management
  , modelPosition
  , getModels
    -- * Resources
  , unitPath
  , tilesData
  , tilePath
  -- * Miscellaneous
  , getTilesBox
  , isEmpty
  , empty
  ) where

import Data.Int(Int32)
import Foreign.C.Types(CInt(..))
import qualified Data.Set as Set (map, elems)

import qualified SDL
import SDL (V2 (..)
           , Point(..)
           , Rectangle(..)
           )

import Engine.Collision
import Engine.Consts
import Engine.Datas
import Render.Model (modifyModelPos
                    , renPos
                    , RenderModel(..)
                    )
import Render.Primitives

-- | Game specific directions data
data Direction = LeftDir
               | RightDir
               | UpDir
               | DownDir
               | End
               | Unknown
               deriving (Show, Eq, Ord)

-- | Tile sprite kinds enum
data TileKind = Sky
              | Ground
              | Lava
              | Spikes
              deriving (Show, Eq, Ord)

-- | Tile entity data.
data Tile = Tile
            { dim   :: (Int32, Int32) -- ^ Dimensions of the tile
            , pos   :: (Int32, Int32) -- ^ Position of the tile
            , kind  :: TileKind       -- ^ Kind of the sprite
            , model :: RenderModel    -- ^ Graphic representation
            , tBox  :: BoundingBox    -- ^ Bounding box for collision detection
            } deriving (Ord)

instance Collidable Tile where
  boundingBox = tBox

instance Eq Tile where
  x == y = GameData.pos x == GameData.pos y

-- | Data of a tile set for particular level
data TileMap = TileMap
               { width     :: Int      -- ^ Width of a tile level
               , height    :: Int      -- ^ Height of a tile level
               , tiles     :: [Tile]   -- ^ List of tiles
               , tilesPath :: FilePath -- ^ Path to tile map configuration
               } deriving(Eq)

-- | Data of entities in the game world
data UnitKind = CoinU     -- ^ Collectable
              | GateU     -- ^ LevelExit
              | HealthUpU -- ^ Health enhanment
              | PlayerU   -- ^  Enemy
              deriving (Show, Eq, Ord)

type TileSize = V2 Int32
type TileTexture = (TileSize
                   , [((Rectangle Int32) , TileKind)]
                   )

-- |Temporary function. Return a path for tile sprites
tilePath :: FilePath
tilePath = "example_data/tiles.bmp"

-- |Temporary function. Return paths for game resources, based
-- on the UnitKind enum
unitPath :: UnitKind
         -> FilePath
unitPath CoinU = "example_data/coin.bmp"
unitPath HealthUpU = "example_data/health.bmp"
unitPath GateU = "example_data/gate.bmp"
unitPath PlayerU = "example_data/dot.bmp"

-- | ..used in GameState:80
isEmpty :: TileMap -> Bool
isEmpty s = s == empty

-- |
empty :: TileMap
empty = TileMap
        { width = 0
        , height = 0
        , tiles = []
        , tilesPath = ""
        }

-- |
getModels :: [Tile]
          -> [RenderModel]
          -> [RenderModel]
getModels (x:xs) rm = getModels xs ((renderModel x):rm)
getModels [] rm = rm

-- |
renderModel :: Tile
            -> RenderModel
renderModel (Tile _ _ _ mod _) = mod

-- |
getTilesBox :: TileMap
            -> [(BoundingBox, BoxKind)]
getTilesBox t = zip boxes kinds
  where tiles' = filter(\x -> kind x /= Sky) (tiles t)
        (boxes, kinds) = mapTiles tiles' ([], [])

-- |
mapTiles :: [Tile]                     -- ^ List of tiles in the level
         -> ([BoundingBox], [BoxKind]) -- ^ Partial list of bounding boxes
         -> ([BoundingBox], [BoxKind]) -- ^ List of boundig boxes in the level
mapTiles (t:ts) (boxes, kinds) = mapTiles ts ((b:boxes),(k:kinds))
  where (b, k) = tileBoxKind t
mapTiles [] boxes = boxes

-- |
tileBoxKind :: Tile -> (BoundingBox, BoxKind)
tileBoxKind (Tile _ _ kind _ tbox) = (tbox, check kind)
  where check kind = case kind of
                       Ground -> TileGround
                       Lava   -> TileLava
                       Spikes -> TileSpikes

-- | Setting a sprite frame for tiles texture file.
-- Values are file specific and need to be configurated manually.
tilesData :: TileKind -> Rectangle CInt
tilesData k = case k of
                Sky    -> (Rectangle (P $ V2 0 0)) tile
                Ground -> (Rectangle (P $ V2 32 0)) tile
                Lava   -> (Rectangle (P $ V2 64 0)) tile
                Spikes -> (Rectangle (P $ V2 96 0)) tile
  where d = CInt tileSize
        tile = V2 d d

-- | Translates a set of pressed keys to in-game player directions
transformSet :: ActiveKeys                 -- ^ Pressed keys set
             -> (SDL.Keycode -> Direction) -- ^ Custom mapping function
             -> [Direction]                -- ^ List of directions
transformSet keys f = Set.elems $ Set.map f keys

-- | Simple mapping function from SDL.Keycode to in-game directions
-- which understands arrow keys and escape presses.
transformKeys :: SDL.Keycode -- ^ Key represented in SDL library
              -> Direction   -- ^ In-game direction
transformKeys (SDL.KeycodeUp) = UpDir
transformKeys (SDL.KeycodeLeft)  = LeftDir
transformKeys (SDL.KeycodeRight) = RightDir
transformKeys (SDL.KeycodeDown)  = DownDir
transformKeys (SDL.KeycodeEscape) = End
transformKeys _ = Unknown

-- |
transDirection :: (Double, Double) -- ^
               -> [Direction]      -- ^
               -> Double           -- ^
               -> CenterPosition   -- ^
transDirection (x',y') (x:xs) dt  = case x of
                                      LeftDir  -> transDirection
                                                  ((x' - pVelo*dt), y')
                                                  xs dt
                                      RightDir -> transDirection
                                                  ((x' + pVelo*dt), y')
                                                  xs dt
                                      UpDir    -> transDirection
                                                  (x', (y' - pVelo*dt))
                                                  xs dt
                                      DownDir  -> transDirection
                                                  (x', (y' + pVelo*dt))
                                                  xs dt
                                      _ ->  transDirection (x',y') xs dt
transDirection (x, y) [] _ = (CInt(floor x), CInt (floor y))

-- | Temporary function. Calculates the new position based on pressed keys
modelPosition :: ActiveKeys     -- ^ Set of pressed keys
              -> CenterPosition -- ^ Center position in the last frame
              -> Double         -- ^ Delta time
              -> CenterPosition -- ^ New center position
modelPosition keys !(u,i) dt = calcPos (u, i) (transDirection (0.0, 0.0) dirs dt)
  where
    dirs = transformSet keys transformKeys
    calcPos !(xp, yp) !(x', y') = ((xp + x'), (yp + y'))
