module GameMain where
import Engine

--import Timer
--import FRP.Elerea.Simple

-- gameDemo
{-gameMain :: IO ()
gameMain = do
  initialize
  openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  windowTitle $= "Elerea Bounce"

  (mousePositionGen,mousePositionSink) <- external vnull
  (mousePressGen,mousePressSink) <- external (False,False)

  closed <- newIORef False
  windowSizeCallback $= resizeGLScene
  windowCloseCallback $= (writeIORef closed True >> return True)
  initGL 800 800

  unitCircle <- defineNewList Compile $ renderPrimitive TriangleStrip $ forM_ [0..20] $ \i -> do
    let a = 2*pi*i/20
    vertex $ Vertex3 (0.5*sin a) (0.5*cos a) (0 :: GLfloat)
    vertex $ Vertex3 0 0 (0 :: GLfloat)

  demo <- start $ do
    mousePosition <- mousePositionGen
    mousePress <- mousePressGen
    bounceDemo (render unitCircle) mousePosition mousePress
  time $= 0
  driveNetwork demo (readInput mousePositionSink mousePressSink closed)

  closeWindow-}

gameMain = do
  let engine = sampleEngine
  runEngine engine
