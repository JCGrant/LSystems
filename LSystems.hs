module LSystems where

import           Control.Monad.State (State, evalState, state)
import           Data.Maybe          (fromMaybe)

import           IC.Graphics

type Rule = (Char, String)

type Rules = [Rule]

type Angle = Float

type Axiom = String

data LSystem = LSystem
  { angle :: Angle
  , axiom :: Axiom
  , rules :: Rules
  } deriving (Show)

type TurtleState = (Vertex, (Angle, Angle))

type Command = Char

type Commands = [Command]

type Stack = [TurtleState]

lookupChar :: Char -> Rules -> String
-- Pre: the character has a binding in the Rules list
lookupChar c rules = fromMaybe (c : "") $ lookup c rules

expandOne :: String -> Rules -> String
expandOne s rules = concatMap (`lookupChar` rules) s

expand :: String -> Int -> Rules -> String
expand s n rules = applyNTimes (`expandOne` rules) s n
  where
    applyNTimes :: (a -> a) -> a -> Int -> a
    applyNTimes f x n = iterate f x !! n

move :: Command -> Angle -> TurtleState -> TurtleState
move command deltaAngle state@(pos@(x, y, z), angles@(elevation, azimuth)) =
  case command of
    'L' -> (pos, (elevation + deltaAngle, azimuth))
    'R' -> (pos, (elevation - deltaAngle, azimuth))
    '>' -> (pos, (elevation, azimuth + deltaAngle))
    '<' -> (pos, (elevation, azimuth - deltaAngle))
    'F' ->
      ( ( x + realToFrac (xyProjectionDistance * cos elevationRads)
        , y + realToFrac (xyProjectionDistance * sin elevationRads)
        , z + realToFrac (turtleSpeed * sin azimuthRads))
      , angles)
  where
    elevationRads = degreesToRads elevation
    azimuthRads = degreesToRads azimuth
    degreesToRads d = d * pi / 180
    xyProjectionDistance = turtleSpeed * cos azimuthRads

initialTurtleState :: TurtleState
initialTurtleState = ((0, 0, 0), (90, 0))

turtleSpeed :: Float
turtleSpeed = 1.0

pop :: State Stack TurtleState
pop = state $ \(x:xs) -> (x, xs)

push :: TurtleState -> State Stack ()
push x = state $ \xs -> ((), x : xs)

trace :: Commands -> Angle -> Colour -> [ColouredLine]
trace cs deltaAngle color = evalState (trace' initialTurtleState cs) []
  where
    trace' :: TurtleState -> Commands -> State Stack [ColouredLine]
    trace' _ [] = return []
    trace' state@(pos, _) (c:cs) =
      case c of
        'F' -> do
          lines <- nextTraceState
          return $ (pos, pos', color) : lines
        '[' -> do
          push state
          trace' state cs
        ']' -> do
          prevState <- pop
          trace' prevState cs
        _ -> nextTraceState
      where
        state'@(pos', _) = move c deltaAngle state
        nextTraceState = trace' state' cs

expandLSystem :: LSystem -> Int -> String
expandLSystem (LSystem _ axiom rs) n = expandOne (expand axiom n rs) commandMap

drawLSystem :: LSystem -> Int -> Colour -> IO ()
drawLSystem system n colour =
  drawLines (trace (expandLSystem system n) (angle system) colour)

----------------------------------------------------------
-- Some test systems.
cross, triangle, arrowHead, peanoGosper, dragon, snowflake, tree, bush ::
     LSystem
cross = LSystem 90 "M-M-M-M" [('M', "M-M+M+MM-M-M+M")]

triangle = LSystem 90 "-M" [('M', "M+M-M-M+M")]

arrowHead = LSystem 60 "N" [('M', "N+M+N"), ('N', "M-N-M")]

peanoGosper =
  LSystem 60 "M" [('M', "M+N++N-M--MM-N+"), ('N', "-M+NN++N+M--M-N")]

dragon = LSystem 45 "MX" [('M', "A"), ('X', "+MX--MY+"), ('Y', "-MX++MY-")]

snowflake = LSystem 60 "M--M--M" [('M', "M+M--M+M")]

tree = LSystem 45 "M" [('M', "N[-M][+M][NM]"), ('N', "NN")]

bush = LSystem 22.5 "X" [('X', "M-[[X>]+X]+>M[+<M<X]-X"), ('M', "MM")]

other = LSystem 22 "F" [('F', "FF-[-F+F+F]+[+F-F-F]")]

commandMap :: Rules
commandMap =
  [ ('M', "F")
  , ('N', "F")
  , ('X', "")
  , ('Y', "")
  , ('A', "")
  , ('[', "[")
  , (']', "]")
  , ('+', "L")
  , ('-', "R")
  ]

main = drawLSystem bush 6 white
