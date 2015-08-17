import RackSortLib
import Test.HUnit
import System.Exit (exitFailure)

--     a            k
--    b c          l g
--   d e f        m h d
--  g h i j      n i e b
-- k l m n o    o j f c a
testRotateRackCw :: Test
testRotateRackCw = TestCase $ do
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
testRotateRackCcw = TestCase $ do
    assertEqual "rotateRackCcw works correctly" expected actual
    where
        actual = rotateRackCcw "abcdefghijklmno"
        expected = "ojnfimcehlabdgk"

testSolvingCorrectRack :: Test
testSolvingCorrectRack = TestCase $ do
    assertEqual "solving correct rack yields a single solution with no moves" expected actual
    where
        actual = solve correctRack
        expected = [[]]

testSolvingCorrectRackRotatedCw :: Test
testSolvingCorrectRackRotatedCw = TestCase $ do
    assertEqual "solving correct rack rotated clockwise yields a single solution with one move" expected actual
    where
        r = rotateRackCw correctRack
        actual = solve r
        expected = [[RotateCcw r correctRack]]

testSolvingCorrectRackRotatedCcw :: Test
testSolvingCorrectRackRotatedCcw = TestCase $ do
    assertEqual "solving correct rack rotated counter clockwise yields a single solution with one move" expected actual
    where
        r = rotateRackCcw correctRack
        actual = solve r
        expected = [[RotateCw r correctRack]]

tests :: Test
tests = TestList [
    TestLabel "testRotateRackCw" testRotateRackCw,
    TestLabel "testRotateRackCcw" testRotateRackCcw,
    TestLabel "testSolvingCorrectRack" testSolvingCorrectRack,
    TestLabel "testSolvingCorrectRackRotatedCw" testSolvingCorrectRackRotatedCw,
    TestLabel "testSolvingCorrectRackRotatedCcw" testSolvingCorrectRackRotatedCcw
    ]

main :: IO ()
main = do
    cs <- runTestTT tests
    if errors cs > 0 || failures cs > 0
        then exitFailure
        else return ()
