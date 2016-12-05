module GameData where

import qualified SDL
import Engine.Datas
import Engine.Consts
import Data.Set as Set
import Data.IORef
import GameState
import Render.Model (modifyModelPos, renPos)
import Render.Primitives
--temporary module

--game specific directions + commands
data Direction = LeftDir | RightDir | UpDir | DownDir | Unknown | End
  deriving (Show, Eq, Ord)

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


gameLoop :: IORef EngineState -> IORef GameState -> IO ()
gameLoop es gs = do
  engineState <- readIORef es
  gameState <- readIORef gs
  let activeKeys = getKeys engineState
      models = getModelsSet gameState
      model = getModelKey heroKey models
      position = modelPosition activeKeys (renPos model)
      model' = modifyModelPos model position
  writeIORef gs (modifyModelsSet models model' heroKey gameState)
