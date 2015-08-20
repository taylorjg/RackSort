import           Control.Monad (when)
import           RackSortLib
import           System.Exit   (exitFailure)
import           Test.HUnit

--     a            k
--    b c          l g
--   d e f        m h d
--  g h i j      n i e b
-- k l m n o    o j f c a
testRotateRackCw :: Test
testRotateRackCw = TestCase $
    assertEqual "rotateRackCw works correctly" expected actual
    where
        actual = rotateRackCw "abcdefghijklmno"
        expected = "klgmhdniebojfca"

--     a            o
--    b c          j n
--   d e f        f i m
--  g h i j      c e h l
-- k l m n o    a b d g k
testRotateRackCcw :: Test
testRotateRackCcw = TestCase $
    assertEqual "rotateRackCcw works correctly" expected actual
    where
        actual = rotateRackCcw "abcdefghijklmno"
        expected = "ojnfimcehlabdgk"

testRotateRackCwThreeTimes :: Test
testRotateRackCwThreeTimes = TestCase $
    assertEqual "rotateRackCw three times gets back to original" expected actual
    where
        actual = rotateRackCw $ rotateRackCw $ rotateRackCw "abcdefghijklmno"
        expected = "abcdefghijklmno"

testRotateRackCcwThreeTimes :: Test
testRotateRackCcwThreeTimes = TestCase $
    assertEqual "rotateRackCcw three times gets back to original" expected actual
    where
        actual = rotateRackCcw $ rotateRackCcw $ rotateRackCcw "abcdefghijklmno"
        expected = "abcdefghijklmno"

testSolvingCorrectRack :: Test
testSolvingCorrectRack = TestCase $
    assertEqual "solving correct rack yields a single solution with no moves" expected actual
    where
        actual = solve correctRack
        expected = [[]]

testSolvingCorrectRackRotatedCw :: Test
testSolvingCorrectRackRotatedCw = TestCase $
    assertEqual "solving correct rack rotated clockwise yields a single solution with one move" expected actual
    where
        r = rotateRackCw correctRack
        actual = solve r
        expected = [[RotateCcw r correctRack]]

testSolvingCorrectRackRotatedCcw :: Test
testSolvingCorrectRackRotatedCcw = TestCase $
    assertEqual "solving correct rack rotated counter clockwise yields a single solution with one move" expected actual
    where
        r = rotateRackCcw correctRack
        actual = solve r
        expected = [[RotateCw r correctRack]]

tests :: Test
tests = TestList [
        TestLabel "testRotateRackCw" testRotateRackCw,
        TestLabel "testRotateRackCcw" testRotateRackCcw,
        TestLabel "testRotateRackCwThreeTimes" testRotateRackCwThreeTimes,
        TestLabel "testRotateRackCcwThreeTimes" testRotateRackCcwThreeTimes,
        TestLabel "testSolvingCorrectRack" testSolvingCorrectRack,
        TestLabel "testSolvingCorrectRackRotatedCw" testSolvingCorrectRackRotatedCw,
        TestLabel "testSolvingCorrectRackRotatedCcw" testSolvingCorrectRackRotatedCcw
    ]

main :: IO ()
main = do
    c <- runTestTT tests
    when (errors c > 0 || failures c > 0) exitFailure
