module GameData where

import SDL (V2 (..), Point(..), Rectangle(..))
import qualified SDL
import Engine.Datas
import Engine.Consts
import Data.Set as Set
import Render.Model (modifyModelPos, renPos, RenderModel(..))
import Render.Primitives
import Data.Int
import Foreign.C.Types

--game specific directions + commands
data Direction = LeftDir | RightDir | UpDir | DownDir | Unknown | End
  deriving (Show, Eq, Ord)

data TileKind = Sky | Ground | Lava | Spikes
  deriving (Show, Eq)

data Tile a = Tile
              { dim :: (Int32, Int32)
              , pos :: (Int32, Int32)
              , kind :: a
              , model :: RenderModel
              } deriving (Eq)

instance (Show a) => Show (Tile a) where
  show (Tile dim pos kind _) = "Tile| dimens:" ++ show dim ++ ", pos:"
                             ++ show pos ++ ", kind:" ++ show kind ++ "\t"
data TileMap a = TileMap
                 { width :: Int
                 , height :: Int
                 , tiles :: [Tile a]
                 , tilesPath :: FilePath
                 }

instance Show a => Show (TileMap a) where
  show (TileMap _ _ tiles _) = show tiles

getTiles :: TileMap TileKind -> [Tile TileKind]
getTiles (TileMap _ _ t _) = t

getModels :: [Tile TileKind]-> [RenderModel] -> [RenderModel]
getModels (x:xs) rm = getModels xs ((renderModel x):rm)
getModels [] rm = rm

renderModel :: Tile TileKind -> RenderModel
renderModel (Tile _ _ _ mod) = mod

type TileSize = V2 Int32
type TileTexture = (TileSize, [((Rectangle Int32), TileKind)])

-- temp!!
tilePath :: FilePath
tilePath = "example_data/tiles.bmp"

data UnitKind = CoinU | GateU | HealthUpU | PlayerU --  Enemy
  deriving (Show, Eq, Ord)
--lame
unitPath :: UnitKind -> FilePath
unitPath CoinU = "example_data/coin.bmp"
unitPath HealthUpU = "example_data/health.bmp"
unitPath GateU = "example_data/gate.bmp"
unitPath PlayerU = "example_data/dot.bmp"

tilesData :: TileKind -> Rectangle CInt
tilesData k = case k of
  Sky -> (Rectangle (P $ V2 0 0)) tile
  Ground -> (Rectangle (P $ V2 32 0)) tile
  Lava -> (Rectangle (P $ V2 64 0)) tile
  Spikes -> (Rectangle (P $ V2 96 0)) tile
  where d = CInt tileSize
        tile = V2 d d

transformSet :: ActiveKeys -> (SDL.Keycode -> Direction) -> [Direction]
transformSet keys f = Set.elems $ Set.map f keys

transformKeys :: SDL.Keycode -> Direction
transformKeys (SDL.KeycodeUp) = UpDir
transformKeys (SDL.KeycodeLeft)  = LeftDir
transformKeys (SDL.KeycodeRight) = RightDir
transformKeys (SDL.KeycodeDown)  = DownDir
transformKeys (SDL.KeycodeEscape) = End
transformKeys _ = Unknown

transDirection :: (Double, Double) -> [Direction] -> Double -> (CInt, CInt)
transDirection (x',y') (x:xs) dt = case x of
  LeftDir -> transDirection ((x' - pVelo*dt), y') xs dt
  RightDir -> transDirection ((x' + pVelo*dt), y') xs dt
  UpDir -> transDirection (x', (y' - pVelo*dt)) xs dt
  DownDir -> transDirection (x', (y' + pVelo*dt)) xs dt
  _ ->  transDirection (x',y') xs dt
transDirection (x, y) [] _ = (CInt(floor x), CInt (floor y))

-- TEMP SECTION
modelPosition :: ActiveKeys -> CenterPosition -> Double -> (Int, Int)
              -> CenterPosition
modelPosition keys pos dt (lw, lh) = calcPos pos (transDirection (0.0, 0.0) dirs dt)
  where
    dirs = transformSet keys transformKeys
    check po con
      | po > con = con
      | otherwise = po
    calcPos (xp, yp) (x', y') = ((check (xp + x') (fromIntegral lw)) --to transDirection
                                ,(check (yp + y') (fromIntegral lh)))
