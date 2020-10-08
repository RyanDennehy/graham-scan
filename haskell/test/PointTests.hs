import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import PointData
import Utility

-- Data
startPoint :: Point2D
startPoint = Point 0.0 0.0

-- dist Tests
distError :: String
distError = "Distance was not calculated correctly"

distTest1 :: Assertion
distTest2 :: Assertion

distTest1 = assertEqual distError 0.0 (dist (Point 1.0 1.0) (Point 1.0 1.0))
distTest2 = assertEqual distError 5.0 (dist (Point 3.0 0.0) (Point 0.0 4.0))

-- startingPoint Tests
startError :: String
startError = "Did not find the correct start point"

startTest1 :: Assertion
startTest2 :: Assertion

startTest1 = assertEqual startError startPoint (startingPoint [startPoint])
startTest2 = assertEqual startError (Point 1.0 1.0) (startingPoint [(Point 2.0 2.0), (Point 2.0 1.0), (Point 1.0 1.0), (Point 1.0 2.0)])

-- angleBetween Tests
polarError :: String
polarError = "Polar angle was not calculated correctly"

polarTest :: Point2D -> Double -> Assertion
polarTest p expected = assertBool polarError (approxEqual expected (angleBetween startPoint p))

polarNotEqual :: String
polarNotEqual = "Angles not equal"

haveSameAngle :: Point2D -> Point2D -> Assertion
haveSameAngle a b = assertBool polarNotEqual (approxEqual (angleBetween startPoint a) (angleBetween startPoint b))

polarTest0Deg   :: Assertion
polarTest30Deg  :: Assertion
polarTest60Deg  :: Assertion
polarTest90Deg  :: Assertion
polarTest120Deg :: Assertion
polarTest150Deg :: Assertion
polarTest180Deg :: Assertion

polarTest0Deg   = polarTest (Point 1.0 0.0) 0.0
polarTest30Deg  = polarTest (Point ((sqrt 3) / 2.0) 0.5) (pi / 6.0)
polarTest60Deg  = polarTest (Point 0.5 ((sqrt 3) / 2.0)) (pi / 3.0)
polarTest90Deg  = polarTest (Point 0.0 1.0) (pi / 2.0)
polarTest120Deg = polarTest (Point (negate 0.5) ((sqrt 3) / 2.0)) (4.0 * pi / 6.0)
polarTest150Deg = polarTest (Point (negate ((sqrt 3) / 2.0)) 0.5) (5.0 * pi / 6.0)
polarTest180Deg = polarTest (Point (negate 1.0) 0.0) pi

polarTestLine = haveSameAngle (Point 1.0 2.0) (Point 2.0 4.0)

-- dedupByEq Tests
dedupError :: String
dedupError = "Did not deduplicate list correctly"

dedupByEqTest :: (Eq a, Show a) => [a] -> [a] -> Assertion
dedupByEqTest ps expected = assertEqual dedupError expected (dedupByEq ps)

dedupByEqNone   :: Assertion
dedupByEqOne    :: Assertion
dedupByEqTriple :: Assertion
dedupByEqTwo    :: Assertion

dedupByEqNone   = dedupByEqTest [(Point 1.0 2.0), (Point 1.0 1.0)]
                                [(Point 1.0 2.0), (Point 1.0 1.0)]
dedupByEqOne    = dedupByEqTest [(Point 1.0 1.0), (Point 1.0 1.0)]
                                [(Point 1.0 1.0)]
dedupByEqTriple = dedupByEqTest [(Point 1.0 1.0), (Point 1.0 1.0), (Point 1.0 1.0)]
                                [(Point 1.0 1.0)]
dedupByEqTwo    = dedupByEqTest [(Point 1.0 1.0), (Point 2.0 3.0), (Point 2.0 3.0), (Point 1.0 1.0)]
                                [(Point 1.0 1.0), (Point 2.0 3.0)]

-- dedupByAngle Tests
dedupByAngleTest :: [Point2D] -> [Point2D] -> Assertion
dedupByAngleTest ps expected = assertEqual dedupError expected (dedupByAngle startPoint ps)

dedupByAngleNone    :: Assertion
dedupByAngleOne     :: Assertion
dedupByAngleTriple  :: Assertion
dedupByAngleTwoA    :: Assertion
dedupByAngleTwoB    :: Assertion

dedupByAngleNone    = dedupByAngleTest  [(Point 1.0 2.0), (Point 1.0 1.0)]
                                        [(Point 1.0 2.0), (Point 1.0 1.0)]
dedupByAngleOne     = dedupByAngleTest  [(Point 1.0 2.0), (Point 10.0 20.0)]
                                        [(Point 10.0 20.0)]
dedupByAngleTriple  = dedupByAngleTest  [(Point 1.0 2.0), (Point 10.0 20.0), (Point 100.0 200)]
                                        [(Point 100.0 200.0)]
dedupByAngleTwoA    = dedupByAngleTest  [(Point 3.0 2.0), (Point 1.0 2.0), (Point 2.0 4.0), (Point 1.5 1.0)]
                                        [(Point 3.0 2.0), (Point 2.0 4.0)]
dedupByAngleTwoB    = dedupByAngleTest  [(Point 1.0 2.0), (Point 2.0 4.0), (Point 3.0 2.0), (Point 1.5 1.0)]
                                        [(Point 2.0 4.0), (Point 3.0 2.0)]

-- Sorting tests
sortTest1 :: Assertion
sortTest1 = assertEqual "Empty points list did not result in an empty list" [] (sortPoints startPoint [])

sortError :: String
sortError = "Points were not sorted correctly"

t2In = [(Point 10.0 5.0), (Point 0.0 10.0), (Point 10.0 10.0), (Point 10.0 0.0), (Point 5.0 10.0)]
t2Out = [(Point 10.0 0.0), (Point 10.0 5.0), (Point 10.0 10.0), (Point 5.0 10.0), (Point 0.0 10.0)]
sortTest2 :: Assertion
sortTest2 = assertEqual sortError t2Out (sortPoints startPoint t2In)

t3In = [(Point 5.0 1.0), (Point 3.0 3.0), (Point 1.0 4.0), (Point 2.0 6.0), (Point 8.0 7.0), (Point 6.0 6.0), (Point 4.0 2.0)]
t3Out = [(Point 5.0 1.0), (Point 4.0 2.0), (Point 8.0 7.0), (Point 6.0 6.0), (Point 2.0 6.0), (Point 1.0 4.0)]
sortTest3 = assertEqual sortError t3Out (sortPoints startPoint t3In)

main :: IO ()
main = defaultMainWithOpts
       [testCase "dist1" distTest1
       , testCase "dist2" distTest2
       , testCase "start1" startTest1
       , testCase "start2" startTest2
       , testCase "polar0" polarTest0Deg
       , testCase "polar30" polarTest30Deg
       , testCase "polar60" polarTest60Deg
       , testCase "polar90" polarTest90Deg
       , testCase "polar120" polarTest120Deg
       , testCase "polar150" polarTest150Deg
       , testCase "polar180" polarTest180Deg
       , testCase "polarLine" polarTestLine
       , testCase "dedupByEqNone" dedupByEqNone
       , testCase "dedupByEqOne" dedupByEqOne
       , testCase "dedupByEqTriple" dedupByEqTriple
       , testCase "dedupByEqTwo" dedupByEqTwo
       , testCase "dedupByAngleNone" dedupByAngleNone
       , testCase "dedupByAngleOne" dedupByAngleOne
       , testCase "dedupByAngleTriple" dedupByAngleTriple
       , testCase "dedupByAngleTwoA" dedupByAngleTwoA
       , testCase "dedupByAngleTwoB" dedupByAngleTwoB
       , testCase "sort1" sortTest1
       , testCase "sort2" sortTest2
       , testCase "sort3" sortTest3]
       mempty
       --, testCase "" 
