module GameState where

import Data.Map as Map
import Render.Model (modifyModelPos, renPos
                    , RenderModel(..), dummyModel
                    , sampleSet, Camera, renDim)
import Engine.Datas
import Engine.Consts
import Data.IORef
import GameData
import Render.Primitives
import qualified World as W
import SDL (Point(P), V2(..))
import qualified SDL (Rectangle(..))

data GameState = GameState
                 { level :: Int
                 , world :: W.World
                 , maps :: [TileMap TileKind] -- level maps
                 , modelsSet :: Map Int RenderModel
                 --isOver :: Bool
                 }

--rename
getModelsSet :: GameState -> Map Int RenderModel
getModelsSet (GameState _ _ _ mod) = mod

getTilesModels :: GameState -> [RenderModel]
getTilesModels (GameState lvl _ maps _ ) = getModels (getTiles (maps !! lvl)) []

getWorldModels :: GameState -> [RenderModel]
getWorldModels (GameState _ wor _ _) = W.renderWorld wor

setCameraPosition :: Camera -> CenterPosition -> Dimensions
                  -> Camera
setCameraPosition (SDL.Rectangle (P (V2 camX camY)) (V2 width height))
                 (playerX, playerY) (playerW, playerH) = SDL.Rectangle
                                                         (P (V2 camX'' camY''))
                                                         (V2 width height)
  where camX' = (playerX + playerW `div` 2) - (width `div` 2)
        camY' = (playerY + playerH `div` 2) - (height `div` 2)
        camX'' = if camX' < 0
                    then 0
                 else if camX' > width
                         then width
                      else camX'
        camY'' = if camY' < 0
                    then 0
                 else if camY' > height
                         then height
                      else camY'

getModelKey :: Int -> Map Int RenderModel-> RenderModel
getModelKey n modMap = case Map.lookup n modMap of
  Just m -> m
  Nothing -> dummyModel

modifyModelsSet :: Map Int RenderModel -> RenderModel
                -> Int -> GameState
                -> GameState
modifyModelsSet modMap rm n (GameState lvl wor maps' models) = GameState
                                                         { level = lvl
                                                         , world = wor
                                                         , maps = maps'
                                                         , modelsSet = modMap'
                                                         }
  where modMap' = Map.insert n rm modMap

--later: read from json config file maybe?
initStateG :: GameState
initStateG = GameState { level = 1
                       , world = undefined
                       , maps = []
                       , modelsSet = sampleSet
                       }
gameLoop :: IORef EngineState -> IORef GameState -> IO ()
gameLoop es gs = do
  engineState <- readIORef es
  gameState <- readIORef gs
  let activeKeys = getKeys engineState
      models = getModelsSet gameState
      model = getModelKey heroKey models
      position = modelPosition activeKeys (renPos model) --przesun
      cam' = setCameraPosition (getCamera engineState) position (renDim model)
      model' = modifyModelPos model position cam'
  writeIORef gs (modifyModelsSet models model' heroKey gameState)
  writeIORef es (engineState{camera = cam'})
