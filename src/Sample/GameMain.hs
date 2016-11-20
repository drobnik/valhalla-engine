module GameMain where

import Engine
import GameState

gameMain :: IO ()
gameMain = do
  let engine = sampleEngine
      gameState = initStateG
  runEngine engine gameState
