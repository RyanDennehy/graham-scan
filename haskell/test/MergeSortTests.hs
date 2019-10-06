import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import MergeSort

-- Utility
correctHalves :: Eq a => [a] -> [a] -> [a] -> Bool
correctHalves input out1 out2 =
    let (a, b) = halve input
    in (a == out1) && (b == out2)

-- halve Tests
halveError :: String
halveError = "List was not halved correctly"

havleOne    :: Assertion
halveTwo    :: Assertion
halveEven   :: Assertion
halveOdd    :: Assertion

havleOne    = assertBool halveError (correctHalves [1] [] [1])
halveTwo    = assertBool halveError (correctHalves [1,2] [1] [2])
halveEven   = assertBool halveError (correctHalves [1,2,3,4,5,6] [1,2,3] [4,5,6])
halveOdd    = assertBool halveError (correctHalves [1,2,3,4,5,6,7] [1,2,3] [4,5,6,7])

-- mergeWrapper Tests
sortError :: String
sortError = "List was not sorted correctly"

sort2   :: Assertion
sort4A  :: Assertion
sort4B  :: Assertion
sort4C  :: Assertion
sort16  :: Assertion

sort2   = assertEqual sortError [1,2] (mergeWrapper [2,1])
sort4A  = assertEqual sortError [1,2,3,4] (mergeWrapper [2,1,3,4])
sort4B  = assertEqual sortError [1,2,3,4] (mergeWrapper [2,1,4,3])
sort4C  = assertEqual sortError [1,2,3,4] (mergeWrapper [4,3,2,1])
sort16  = assertEqual sortError [1..16] (mergeWrapper [11,1,4,9,16,15,12,3,6,2,13,7,8,5,10,14])

main :: IO ()
main = defaultMainWithOpts
       [testCase "havleOne" havleOne
       , testCase "halveTwo" halveTwo
       , testCase "halveEven" halveEven
       , testCase "halveOdd" halveOdd
       , testCase "sort2" sort2
       , testCase "sort4A" sort4A
       , testCase "sort4B" sort4B
       , testCase "sort4C" sort4C
       , testCase "sort16" sort16]
       mempty
