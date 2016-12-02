module GameMain where

import Data.IORef
import Engine
import Engine.Datas
import GameState

gameMain :: IO ()
gameMain = do
  engineState <- newIORef (sampleState)
  gameState <- newIORef (initStateG)
  let engine = sampleEngine engineState

  runEngine engine gameState
