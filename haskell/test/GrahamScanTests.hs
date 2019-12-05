import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import GrahamScan
import PointData

-- grahamScan Tests
scanError :: String
scanError = "Convex hull was not successfully computed"

scanTest1 :: Assertion
scanTest2 :: Assertion
scanTest3 :: Assertion
scanTest4 :: Assertion
scanTest5 :: Assertion

unitCirclePoint a = Point (cos a) (sin a)

t1In  = [(Point 0.0 0.0),
         (Point 2.0 0.0),
         (Point 2.0 2.0),
         (Point 0.0 2.0)]

t1Out = [(Point 0.0 0.0),
         (Point 2.0 0.0),
         (Point 2.0 2.0),
         (Point 0.0 2.0)]

t2In  = [(Point 0.0 0.0),
         (Point 1.0 4.0),
         (Point 5.0 3.0),
         (Point 3.0 1.0),
         (Point 2.0 2.0),
         (Point 4.0 4.5)]

t2Out = [(Point 0.0 0.0),
         (Point 3.0 1.0),
         (Point 5.0 3.0),
         (Point 4.0 4.5),
         (Point 1.0 4.0)]

t3In  = [(unitCirclePoint 0),
         (unitCirclePoint (pi / 6.0)),
         (unitCirclePoint (pi / 3.0)),
         (unitCirclePoint (pi / 2.0)),
         (unitCirclePoint ((2.0 * pi) / 3.0))]

t3Out = [(unitCirclePoint 0),
         (unitCirclePoint (pi / 6.0)),
         (unitCirclePoint (pi / 3.0)),
         (unitCirclePoint (pi / 2.0)),
         (unitCirclePoint ((2.0 * pi) / 3.0))]

t4In  = [(Point 0.0 0.0),
         (Point 1.0 2.0),
         (Point 5.0 3.0),
         (Point 2.0 4.0),
         (Point 1.0 4.0)]

t4Out = [(Point 0.0 0.0),
         (Point 5.0 3.0),
         (Point 2.0 4.0),
         (Point 1.0 4.0)]

t5In = [(Point 1.0 1.0),
        (Point 5.0 1.0),
        (Point 4.0 7.0),
        (Point 13.0 5.0),
        (Point 1.0 9.0),
        (Point (-2.0) 4.0)]

t5Out = [(Point 1.0 1.0),
         (Point 5.0 1.0),
         (Point 13.0 5.0),
         (Point 1.0 9.0),
         (Point (-2.0) 4.0)]

scanTest1 = assertEqual scanError t1Out (grahamScan t1In)
scanTest2 = assertEqual scanError t2Out (grahamScan t2In)
scanTest3 = assertEqual scanError t3Out (grahamScan t3In)
scanTest4 = assertEqual scanError t4Out (grahamScan t4In)
scanTest5 = assertEqual scanError t5Out (grahamScan t5In)

main :: IO ()
main = defaultMainWithOpts
       [testCase "scanTest1" scanTest1
       , testCase "scanTest2" scanTest2
       , testCase "scanTest3" scanTest3
       , testCase "scanTest4" scanTest4
       , testCase "scanTest5" scanTest5
       ]
       mempty
