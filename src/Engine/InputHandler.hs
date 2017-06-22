-- | Input handling module listens for particular events
-- from external devices such as keyboard, mouse and joystick.
-- SDL library exports an interface to access its event model
-- which stores all the events waiting to be handled in a queue.
-- Function 'pollEvent' is used for obtaining all currently pending events.
-- Input handling system filters out other types of events, leaving only
-- those associated with a user input.
module Engine.InputHandler (inputCallback) where

import qualified Data.Set as Set
import Data.IORef
import Control.Monad

import qualified SDL

import Engine.Datas

-- | Filter out events which are not associated with user input and
-- let define handling for chosen events.
inputCallback :: IORef EngineState -> IO ()
inputCallback e = do
  events <- map SDL.eventPayload <$> SDL.pollEvents
  handleEvents e events

-- | Define behaviour for keyboard input processing and window
-- closing action. It can be easily extended for other payloads.
-- Ignores everything apart closing window and keyboard events.
handleEvents :: IORef EngineState -> [SDL.EventPayload] -> IO ()
handleEvents es events = foldMap (\ev -> inputUpdate es ev) events
  where
    inputUpdate :: IORef EngineState -> SDL.EventPayload -> IO ()
    inputUpdate estate e = case e of
                             SDL.QuitEvent -> closeGame estate
                             SDL.KeyboardEvent es -> keyboardEvents es estate
                             SDL.MouseMotionEvent es -> return ()
                             SDL.MouseButtonEvent es -> return ()
                             _ -> return ()

-- | Match an action for a key spotted in event callback. If a particular
-- key is pressed, it is added to 'ActiveKey' set (but ignored on continous press).
-- On release, the key is deleted.
keyboardEvents :: SDL.KeyboardEventData -> IORef EngineState -> IO ()
keyboardEvents e estate
  | SDL.keyboardEventKeyMotion e == SDL.Pressed  = keyUpdate (getKey e) Set.insert
  | SDL.keyboardEventKeyMotion e == SDL.Released = keyUpdate (getKey e) Set.delete
  | SDL.keyboardEventRepeat e = return ()
  | otherwise = return () -- ^ Ignore everything else
  where
    getKey e = SDL.keysymKeycode (SDL.keyboardEventKeysym e) -- ^ Unpack the keycode
    keyUpdate key f = do -- ^ Apply an update on 'EngineState' reference
      (EngineState keys' dt' ov cam' w') <- readIORef estate
      let keysUpd = f key keys'
          engine = EngineState{ keys = keysUpd
                              , dt = dt'
                              , over = ov
                              ,camera = cam'
                              , winSetup = w'
                              }
      writeIORef estate engine
