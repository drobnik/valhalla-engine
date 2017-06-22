-- | Simple module for triggering game loop.
module GameMain
  (gameMain) where

import Data.IORef

import Engine
import Engine.Datas
import GameState

-- | Initialize 'GameState' and 'EngineState' references before
-- entering the main engine loop
gameMain :: IO ()
gameMain = do
  engineState <- newIORef (sampleState)
  gameState <- newIORef (initStateG)
  let engine = sampleEngine engineState

  runEngine engine gameState
