module GameData where

import Graphics.UI.GLUT
import Engine.Datas
import Data.Set as Set
--temporary module

data Offset = Offset
              { left :: Int
              , right :: Int
              , up :: Int
              , down :: Int
              }

--game specific directions + commands
data Direction = LeftDir | RightDir | UpDir | DownDir | Unknown | End
  deriving (Show, Eq, Ord)

--TEMP
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
