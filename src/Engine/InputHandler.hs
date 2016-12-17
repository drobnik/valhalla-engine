module Engine.InputHandler where

import qualified SDL
import qualified Data.Set as S
import Data.IORef
import Engine.Datas
import Control.Monad

inputCallback :: IORef EngineState -> IO ()
inputCallback e = do
  events <- map SDL.eventPayload <$> SDL.pollEvents
  handleEvents e events

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

keyboardEvents :: SDL.KeyboardEventData -> IORef EngineState -> IO ()
keyboardEvents e estate
  | SDL.keyboardEventKeyMotion e == SDL.Pressed  = keyUpdate (getKey e) S.insert
  | SDL.keyboardEventKeyMotion e == SDL.Released = keyUpdate (getKey e) S.delete
  | SDL.keyboardEventRepeat e = return () --do not update
  | otherwise = return ()
  where
    getKey e = SDL.keysymKeycode (SDL.keyboardEventKeysym e)
    keyUpdate key f = do
      (EngineState keys' dt' ov cam' w') <- readIORef estate
      let keysUpd = f key keys'
          engine = EngineState{keys = keysUpd, dt = dt', over = ov
                              ,camera = cam', winSetup = w'}
      writeIORef estate engine
