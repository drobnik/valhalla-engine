module Engine.InputHandler where

import qualified SDL
import Data.Set as S
import Data.IORef
import Engine.Datas
import Control.Monad


keyboardMouse :: IORef EngineState ->  Key -> KeyState
                -> Modifiers -> Position -> IO () --later: take care of modifires
keyboardMouse e k kState mod pos =
  case (k, kState) of
    (Char c, Down)        -> keyUpdate (Char c) S.insert
    (Char c, Up)          -> keyUpdate (Char c) S.delete
    (SpecialKey c, Down)  -> keyUpdate (SpecialKey c) S.insert
    (SpecialKey c, Up)    -> keyUpdate (SpecialKey c) S.delete
    (MouseButton c, Up)   -> keyUpdate (MouseButton c) S.delete
    (MouseButton c, Down) -> keyUpdate (MouseButton c) S.insert
    _ -> return () --overlapped

  where
    keyUpdate key f = do
      (EngineState keys' dt') <- readIORef e
      let keysUpd = f key keys' --klawisze tlumaczone dopiero w estate
          engine = EngineState{keys = keysUpd, dt = dt'}
      writeIORef e engine
              -- potem w idle patrzymy co sie tutaj stalo i nakladamy to w grze.
          -- patrz engineState -> zmieniaj wedlug tego stan gry i dalej

inputCallback :: IORef EngineState -> IO ()
inputCallback e = do
  keyboardMouseCallback $= (Just (keyboardMouse e))
  -- motionCallback $=
  -- passiveMotionCallback $=
