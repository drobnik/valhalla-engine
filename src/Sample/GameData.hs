module GameData where

import qualified SDL
import Engine.Datas
import Engine.Consts
import Render.Primitives
import Data.Set as Set
--temporary module

--game specific directions + commands
data Direction = LeftDir | RightDir | UpDir | DownDir | Unknown | End
  deriving (Show, Eq, Ord)

transformSet :: ActiveKeys -> (Key -> Direction) -> [Direction]
transformSet keys f = Set.elems $ Set.map f keys

transformKeys :: Key -> Direction
transformKeys (Char ' ') = UpDir
transformKeys (SpecialKey KeyLeft)  = LeftDir
transformKeys (SpecialKey KeyRight) = RightDir
transformKeys (SpecialKey KeyUp)    = UpDir
transformKeys (SpecialKey KeyDown)  = DownDir
transformKeys (Char '\ESC') = End
transformKeys (Char _ ) = Unknown
transformKeys (SpecialKey _) = Unknown

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
