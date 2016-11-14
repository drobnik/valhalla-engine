module Engine.InputHandler where

import Graphics.UI.GLUT
import Data.Set as S
import Data.IORef
import Engine.Datas

--data Direction = Left | Right | Up | Down | Jump --TEMP

-- gs - gameState, e - engineState
data InputHandler e gs = InputHandler --mozna zdefiniowac klase, ktora okresla gs
                         { keyboardMouseIn :: EngineState
                                           -> Key -> KeyState
                                           -> Modifiers -> Position -> IO ()
                         , activeMouseMotion :: EngineState -> Position -> IO ()
                         , passiveMouseMotion :: EngineState -> Position -> IO ()
                         }
initKeys :: IO ActiveKeys
initKeys = newIORef (S.empty)

transformKeys :: Key -> Direction
transformKeys (Char c) = show c
transformKeys (Char ' ') = "Jump!"
transformKeys (SpecialKey KeyLeft)  = "Left"
transformKeys (SpecialKey KeyRight) = "Right"
transformKeys (SpecialKey KeyUp)    = "Up"
transformKeys (SpecialKey KeyDown)  = "Down"
transformKeys _ = "Random_event"

-- for silly debuging
whatIsActive :: ActiveKeys -> (Key -> Direction) -> IO ()
whatIsActive keys f = do
  keys' <- readIORef keys
  let iterated = S.elems $ S.map f keys'
  showMe iterated

showMe :: [String] -> IO ()
showMe (x:xs) = do
  putStrLn x
  showMe xs
showMe [] = return ()

-- for keyboardMouseIn!!
keyboardMouse :: EngineState -> Key -> KeyState
                -> Modifiers -> Position -> IO () --later: take care of modifires +gamestate
keyboardMouse (EngineState keysSet _) k kState mod pos =
  case (k, kState) of
    (Char c, Down)        -> keyUpdate (Char c) S.insert
    (Char c, Up)          -> keyUpdate (Char c) S.delete
    (SpecialKey c, Down)  -> keyUpdate (SpecialKey c) S.insert
    (SpecialKey c, Up)    -> keyUpdate (SpecialKey c) S.delete
    (MouseButton c, Up)   -> keyUpdate (MouseButton c) S.delete
    (MouseButton c, Down) -> keyUpdate (MouseButton c) S.insert
    _ -> return ()

  where keyUpdate key f = do
          keys <- readIORef keysSet
          let keysUpd = f key keys
          writeIORef keysSet keysUpd
          --dodaj do stanu silniczka! On sam musi byc jakis mutowalny kurde
          -- potem w idle patrzymy co sie tutaj stalo i nakladamy to w grze.
          -- patrz engineState -> zmieniaj wedlug tego stan gry i dalej
