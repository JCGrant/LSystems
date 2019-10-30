module Tests where

import           IC.Graphics
import           IC.TestSuite
import           LSystems     hiding (main)

import           Data.List    (sort)

angleTestCases =
  [cross ==> 90, LSystem 1 "" [] ==> 1, triangle ==> 90, arrowHead ==> 60]

axiomTestCases =
  [ LSystem 0 "+" [] ==> "+"
  , cross ==> "M-M-M-M"
  , triangle ==> "-M"
  , arrowHead ==> "N"
  ]

rulesTestCases =
  [ cross ==> [('M', "M-M+M+MM-M-M+M")]
  , LSystem 0 "" [('M', "N")] ==> [('M', "N")]
  ]

{- Note: these test cases use angle/axiom/rules, and will fail the test
 - suite with Argument exceptions until those functions are correctly
 - implemented.
 -}
lookupCharTestCases =
  [ ('X', [('X', "Yes"), ('Y', "No")]) ==> "Yes"
  , ('X', [('Y', "No"), ('X', "Yes")]) ==> "Yes"
  , ('M', rules peanoGosper) ==> "M+N++N-M--MM-N+"
  , ('+', rules triangle) ==> "+"
  ]

expandOneTestCases =
  [ (axiom triangle, rules triangle) ==> "-M+M-M-M+M"
  , ("A", [('A', "B")]) ==> "B"
  ]

expandTestCases =
  [ (axiom arrowHead, 2, rules arrowHead) ==> "N+M+N-M-N-M-N+M+N"
  , (axiom dragon, 0, rules dragon) ==> "MX"
  , (axiom dragon, 1, rules dragon) ==> "A+MX--MY+"
  , (axiom dragon, 5, rules dragon) ==>
    concat
      [ "A+A+A+A+A+MX--MY+--A-MX++MY-+--A-A+MX--MY+++A-MX++MY--+"
      , "--A-A+A+MX--MY+--A-MX++MY-+++A-A+MX--MY+++A-MX++MY---+-"
      , "-A-A+A+A+MX--MY+--A-MX++MY-+--A-A+MX--MY+++A-MX++MY--++"
      , "+A-A+A+MX--MY+--A-MX++MY-+++A-A+MX--MY+++A-MX++MY----+"
      ]
  ]

moveTestCases =
  [ ('L', 90, ((100, 100, 0), (90, 0))) ==> ((100.0, 100.0, 0), (180.0, 0))
  , ('F', 60, ((50, 50, 0), (60, 0))) ==>
    ((50.49999997019768, 50.86602544784546, 0.0), (60.0, 0.0))
  , ('F', 45, ((-25, 180, 0), (180, 0))) ==>
    ((-26.0, 179.9999999125772, 0.0), (180.0, 0.0))
  ]

traceTestCases =
  [ ( expandOne (expand (axiom triangle) 1 (rules triangle)) commandMap
    , angle triangle
    , blue) ==>
    sort
      [ ((0.0, 0.0, 0.0), (1.0, 0.0, 0.0), (0.0, 0.0, 1.0))
      , ( (0.9999999562886117, 1.0, 0.0)
        , (1.9999999562886117, 1.0, 0.0)
        , (0.0, 0.0, 1.0))
      , ((1.0, 0.0, 0.0), (0.9999999562886117, 1.0, 0.0), (0.0, 0.0, 1.0))
      , ( (1.9999999125772234, 0.0, 0.0)
        , (2.9999999125772234, 0.0, 0.0)
        , (0.0, 0.0, 1.0))
      , ( (1.9999999562886117, 1.0, 0.0)
        , (1.9999999125772234, 0.0, 0.0)
        , (0.0, 0.0, 1.0))
      ]
  , (expandOne (expand (axiom tree) 1 (rules tree)) commandMap, angle tree, red) ==>
    sort
      [ ( (-8.742277657347586e-8, 2.0, 0.0)
        , (-1.3113416486021379e-7, 3.0, 0.0)
        , (1.0, 0.0, 0.0))
      , ( (-4.371138828673793e-8, 1.0, 0.0)
        , (-0.7071068127963187, 1.7071067690849304, 0.0)
        , (1.0, 0.0, 0.0))
      , ( (-4.371138828673793e-8, 1.0, 0.0)
        , (-8.742277657347586e-8, 2.0, 0.0)
        , (1.0, 0.0, 0.0))
      , ( (-4.371138828673793e-8, 1.0, 0.0)
        , (0.7071067253735421, 1.7071067690849304, 0.0)
        , (1.0, 0.0, 0.0))
      , ((0.0, 0.0, 0.0), (-4.371138828673793e-8, 1.0, 0.0), (1.0, 0.0, 0.0))
      ]
  ]

allTestCases =
  [ TestCase "angle" (angle . unId) (map mkId angleTestCases)
  , TestCase "axiom" (axiom . unId) (map mkId axiomTestCases)
  , TestCase "rules" (rules . unId) (map mkId rulesTestCases)
  , TestCase "lookupChar" (uncurry lookupChar) lookupCharTestCases
  , TestCase "expandOne" (uncurry expandOne) expandOneTestCases
  , TestCase "expand" (uncurry3 expand) expandTestCases
  , TestCase "move" (uncurry3 move) moveTestCases
  , TestCase "trace" (sort . uncurry3 trace) traceTestCases
  ]

runTests = mapM_ goTest allTestCases

main = runTests
