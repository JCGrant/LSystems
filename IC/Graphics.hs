module IC.Graphics
  ( Colour
  , black
  , blue
  , green
  , cyan
  , red
  , magenta
  , yellow
  , white
  , drawLines
  , Vertex
  , ColouredLine
  ) where

import           Control.Monad
import           Data.Char
import           Data.IORef
import           Data.List
import           Graphics.Rendering.OpenGL hiding (Vertex)
import           Graphics.UI.GLUT          hiding (Vertex)

type Colour = (Float, Float, Float)

black, blue, green, cyan, red, magenta, yellow, white :: Colour
black = (0, 0, 0)

blue = (0, 0, 1.0)

green = (0, 1.0, 0)

cyan = (0, 1.0, 1.0)

red = (1.0, 0, 0)

magenta = (1.0, 0, 1.0)

yellow = (1.0, 1.0, 0)

white = (1.0, 1.0, 1.0)

type Vertex = (Double, Double, Double)

type ColouredLine = (Vertex, Vertex, Colour)

drawLines :: [ColouredLine] -> IO ()
drawLines ls = do
  () <- ls `seq` return ()
  _ <- getArgsAndInitialize
  initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
  createWindow "LSystems"
  depthFunc $= Just Less
  angle <- newIORef 0
  delta <- newIORef 1.0
  pos <- newIORef (0, 0)
  actionOnWindowClose $= ContinueExecution
  let vertices = concatMap (\(from, to, _) -> [from, to]) ls
      ((minX, minY, minZ), (maxX, maxY, maxZ)) = computeScale vertices
      top = minY
      bottom = maxY
      right = maximum [abs minX, abs minZ, maxX, maxZ] * 1.1
      left = -right
  displayCallback $= display ls top bottom left right angle pos
  keyboardMouseCallback $= Just (keyboardMouse delta pos)
  idleCallback $= Just (idle angle delta)
  reshapeCallback $= Just reshape
  mainLoop

display ::
     [ColouredLine]
  -> GLdouble
  -> GLdouble
  -> GLdouble
  -> GLdouble
  -> IORef GLfloat
  -> IORef (GLfloat, GLfloat)
  -> DisplayCallback
display ls top bottom left right angle pos = do
  clear [ColorBuffer, DepthBuffer]
  (x', y') <- get pos
  do a <- get angle
     loadIdentity
     translate $ Vector3 x' y' 0
     rotate a $ Vector3 0 1 0
     ortho left right top bottom left right
     renderPrimitive Lines $ mapM_ lineVertices ls
  swapBuffers

lineVertices :: ColouredLine -> IO ()
lineVertices ((fromX, fromY, fromZ), (toX, toY, toZ), (r, g, b)) = do
  color $ Color3 r g b
  vertex $ Vertex3 fromX fromY fromZ
  vertex $ Vertex3 toX toY toZ

keyboardMouse ::
     IORef GLfloat -> IORef (GLfloat, GLfloat) -> KeyboardMouseCallback
keyboardMouse _ _ (Char '\ESC') Down _ _ = leaveMainLoop
keyboardMouse _ _ (MouseButton _) Down _ p = print p
keyboardMouse a p key Down _ _ =
  case key of
    (Char ' ')            -> a $~! negate
    (Char '=')            -> a $~! (* 2)
    (Char '-')            -> a $~! (/ 2)
    (SpecialKey KeyLeft)  -> p $~! \(x, y) -> (x - 0.1, y)
    (SpecialKey KeyRight) -> p $~! \(x, y) -> (x + 0.1, y)
    (SpecialKey KeyUp)    -> p $~! \(x, y) -> (x, y + 0.1)
    (SpecialKey KeyDown)  -> p $~! \(x, y) -> (x, y - 0.1)
    _                     -> return ()
keyboardMouse _ _ _ _ _ _ = return ()

idle :: IORef GLfloat -> IORef GLfloat -> IdleCallback
idle angle delta = do
  d <- get delta
  angle $~! (+ d)
  postRedisplay Nothing

reshape :: Size -> IO ()
reshape s = do
  viewport $= (Position 0 0, s)
  postRedisplay Nothing

computeScale :: [Vertex] -> (Vertex, Vertex)
computeScale =
  foldl' f ((infinity, infinity, infinity), (-infinity, -infinity, -infinity))
  where
    f ((minX, minY, minZ), (maxX, maxY, maxZ)) (x, y, z) =
      ( (min minX x, min minY y, min minZ z)
      , (max maxX x, max maxY y, max maxZ z))
    infinity = 1000000000
