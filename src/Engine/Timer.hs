-- | A time step estimation is done in 'Timer' module. It delivers an advanced timer
-- utility which  allows to restart, pause and resume time calculation.
-- It provides a method to get time ticks, depending on the timer state,
-- stored flags for pause and timer initialisation and references
-- of the milliseconds passed since the possible pause or the beginning of the game.
module Engine.Timer
  (
    -- * Timer type
    Timer(Timer)
    -- * Miscellaneous
  , initTimer
  , start
  , getTicks
  ) where

import qualified SDL.Time as SDL
import Data.Word
import Engine.Consts


-- | Data which simultaneously stores time ticks since the start
-- or possible pause in the game. Flags in this record indicate
-- if the game has started or there is a pause.
data Timer = Timer
           { paused         :: Bool
           , started        :: Bool
           , ticksFromStart :: Word32
           , ticksFromPause :: Word32
           }

-- | Create a timer which has not started.
initTimer :: Timer
initTimer = Timer False False 0 0

-- | Take the first time ticks using 'SDL.Time' utility. Mark
-- the start of measuring time with turning 'started' flag on.
start :: Timer -> IO Timer
start tim = do
  ticksStart <- SDL.ticks
  return (Timer False True ticksStart 0)

-- | Stop the timer. An alias for 'initTimer' because they do the same.
stop :: Timer
stop = initTimer

-- | Pause the game. Turn 'paused' flag on and start counting ticks
-- in 'ticksFromPause' subtimer.
pause :: Timer -> IO Timer
pause tim@(Timer paused started startT _)
  | started && not paused = do
      fromPause <- SDL.ticks
      return ( tim{ paused = True
                  , ticksFromStart = 0
                  , ticksFromPause = fromPause - startT
                 }
             )
  | otherwise = return tim

-- | Unpause the game. Turn 'paused' flag off, recalculate 'ticksFromStart'
-- and zero 'ticksFromPause' subtimer.
unpause :: Timer -> IO Timer
unpause tim@(Timer paused started _ pauseT)
  | started && paused = do
      fromStart <- SDL.ticks
      return (tim{ paused = False
                 , ticksFromStart = fromStart - pauseT
                 , ticksFromPause = 0
                 }
             )
  | otherwise = return tim

-- | Calculate delta time inn every step of the engine loop. Returns a number of
-- ticks if a timer has been created with 'started' flag on.
-- Depending on the state of a timer, it yields paused ticks which is a number
-- of ticks before pausing or ticks since the beginning.
getTicks :: Timer -> IO Word32
getTicks (Timer paused' started' ticksStart ticksPause)
  | started' =
      if paused'
      then return ticksPause
      else do
        time <- SDL.ticks
        return (time - ticksStart)
  | otherwise = return 0
