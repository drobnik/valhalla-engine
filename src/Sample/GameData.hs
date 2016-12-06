module GameData where

import qualified SDL
import Engine.Datas
import Engine.Consts
import Data.Set as Set
import Render.Model (modifyModelPos, renPos, RenderModel(..))
import Render.Primitives
import Data.Int

--game specific directions + commands
data Direction = LeftDir | RightDir | UpDir | DownDir | Unknown | End
  deriving (Show, Eq, Ord)

data TileKind = Sky | Ground | Lava | Spikes

data Tile a = Tile
              { dim :: (Int32, Int32)
              , pos :: (Int32, Int32)
              , kind :: a
              , model :: RenderModel
              }


data TileMap a = TileMap
                 { width :: Int
                 , height :: Int
                 , tiles :: [Tile a]
                 }



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

-- TEMP
modelPosition :: ActiveKeys -> CenterPosition -> CenterPosition
modelPosition keys pos = calcPos pos (transDirection (0, 0)  dirs)
  where
    dirs = transformSet keys transformKeys
    calcPos (xp, yp) (x', y') = ((xp + x'), (yp + y'))
