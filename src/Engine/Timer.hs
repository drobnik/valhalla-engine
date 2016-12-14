module Engine.Timer where

import Engine.Consts
import qualified SDL.Time as SDL
import Data.Word

-- for Word32 -> fromIntegral
data Timer = Timer
           { paused :: Bool
           , started :: Bool
           , ticksFromStart :: Word32
           , ticksFromPause :: Word32
           }

initTimer :: Timer
initTimer = Timer False False 0 0

start :: Timer -> IO Timer
start tim = do
  ticksStart <- SDL.ticks
  return (Timer False True ticksStart 0)

stop :: Timer
stop = Timer False False 0 0

pause :: Timer -> IO Timer
pause tim@(Timer paused started startT _)
  | started && not paused = do
      fromPause <- SDL.ticks
      return (tim{paused = True, ticksFromStart = 0
                 , ticksFromPause = fromPause - startT})
  | otherwise = return tim

unpause :: Timer -> IO Timer
unpause tim@(Timer paused started _ pauseT)
  | started && paused = do
      fromStart <- SDL.ticks
      return (tim{paused = False, ticksFromStart = fromStart - pauseT
                 , ticksFromPause = 0})
  | otherwise = return tim

getTicks :: Timer -> IO Word32
getTicks (Timer paused' started' ticksStart ticksPause)
  | started' =
      if paused'
      then return ticksPause
      else do
        time <- SDL.ticks
        return (time - ticksStart)
  | otherwise = return 0
