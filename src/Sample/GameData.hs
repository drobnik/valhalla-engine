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
  deriving Show

data Tile a = Tile
              { dim :: (Int32, Int32)
              , pos :: (Int32, Int32)
              , kind :: a
              , model :: RenderModel
              }

instance (Show a) => Show (Tile a) where
  show (Tile dim pos kind _) = "Tile| dimens:" ++ show dim ++ ", pos:"
                             ++ show pos ++ ", kind:" ++ show kind ++ "\t"
data TileMap a = TileMap
                 { width :: Int --rename to x
                 , height :: Int -- y
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

tilesData :: TileKind -> Rectangle CInt
tilesData k = case k of
  Sky -> (Rectangle (P $ V2 0 0)) tile
  Ground -> (Rectangle (P $ V2 32 0)) tile
  Lava -> (Rectangle (P $ V2 64 0)) tile
  Spikes -> (Rectangle (P $ V2 64 0)) tile
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

transDirection :: PixOff -> [Direction] -> PixOff
transDirection (x',y') (x:xs) = case x of
  LeftDir -> transDirection ((x' - un),y') xs
  RightDir -> transDirection ((x' + un),y') xs
  UpDir -> transDirection (x',(y' - un)) xs
  DownDir -> transDirection (x',(y' + un)) xs
  _ ->  transDirection (x',y') xs
transDirection off [] = off

-- TEMP SECTION
modelPosition :: ActiveKeys -> CenterPosition -> CenterPosition
modelPosition keys pos = calcPos pos (transDirection (0, 0)  dirs)
  where
    dirs = transformSet keys transformKeys
    calcPos (xp, yp) (x', y') = ((xp + x'), (yp + y'))
