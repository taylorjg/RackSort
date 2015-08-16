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
    assertEqual "rotate rack clockwise is correct" r2 (rotateRackCw r1)
    where
        r1 = "abcdefghijklmno"
        r2 = "klgmhdniebojfca"

--     a            o
--    b c          j n
--   d e f        f i m
--  g h i j      c e h l
-- k l m n o    a b d g k
testRotateRackCcw :: Test
testRotateRackCcw = TestCase $ do
    assertEqual "rotate rack counter clockwise is correct" r2 (rotateRackCcw r1)
    where
        r1 = "abcdefghijklmno"
        r2 = "ojnfimcehlabdgk"

tests :: Test
tests = TestList [
    TestLabel "testRotateRackCw" testRotateRackCw,
    TestLabel "testRotateRackCcw" testRotateRackCcw
    ]

main :: IO ()
main = do
    cs <- runTestTT tests
    if errors cs > 0 || failures cs > 0
        then exitFailure
        else return ()
