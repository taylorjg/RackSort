import           Control.Arrow        (second)
import           Data.Maybe           (maybeToList)
import           RackSortLib
import           System.Exit          (exitFailure)
import           Test.QuickCheck
import           Test.QuickCheck.Test

oppositeMoves :: [Move] -> [Move]
oppositeMoves ms = reverse $ map oppositeMove ms

oppositeMove :: Move -> Move
oppositeMove (Swap idx1 idx2 r1 r2) = Swap idx2 idx1 r2 r1
oppositeMove (RotateCw r1 r2) = RotateCcw r2 r1
oppositeMove (RotateCcw r1 r2) = RotateCw r2 r1

data MoveMaker
    = SwapMaker Int Int
    | RotateCwMaker
    | RotateCcwMaker
    deriving Show

applyMoveMakers :: [MoveMaker] -> Rack -> (Rack, [Move])
applyMoveMakers mms r = second reverse $ foldl op (r, []) mms
    where
        op (r1, ms) (SwapMaker fromIdx toIdx) = (r2, m:ms)
            where
                r2 = swapBalls r fromIdx toIdx
                m = Swap fromIdx toIdx r1 r2
        op (r1, ms) (RotateCwMaker) = (r2, m:ms)
            where
                r2 = rotateRackCw r1
                m = RotateCw r1 r2
        op (r1, ms) (RotateCcwMaker) = (r2, m:ms)
            where
                r2 = rotateRackCcw r1
                m = RotateCcw r1 r2

newtype MoveMakersWrapper = MMW [MoveMaker] deriving Show

genMoveMakers :: Gen MoveMakersWrapper
genMoveMakers = do
    maybeRotate <- frequency [
            (1, return Nothing),
            (0, return $ Just RotateCwMaker),
            (0, return $ Just RotateCcwMaker)
        ]
    numSwaps <- choose (1, 1)
    swaps <- vectorOf numSwaps genSwap
    return (MMW $ (maybeToList maybeRotate) ++ swaps)

genSwap :: Gen MoveMaker
genSwap = do
    fromIdx <- elements [0..14]
    toIdx <- elements $ filter (/=fromIdx) [0..14]
    return (SwapMaker fromIdx toIdx)

instance Arbitrary MoveMakersWrapper where
    arbitrary = genMoveMakers

newtype RackWrapper = RW Rack deriving Show

genRackWrapper :: Gen RackWrapper
genRackWrapper = do
    r <- shuffle correctRack
    return (RW r)

instance Arbitrary RackWrapper where
    arbitrary = genRackWrapper


prop_rotateCwThreeTimes :: RackWrapper -> Bool
prop_rotateCwThreeTimes (RW r) = r == (rotateRackCw $ rotateRackCw $ rotateRackCw r)

prop_rotateCcwThreeTimes :: RackWrapper -> Bool
prop_rotateCcwThreeTimes (RW r) = r == (rotateRackCcw $ rotateRackCcw $ rotateRackCcw r)

prop_rotateCwThenCcw :: RackWrapper -> Bool
prop_rotateCwThenCcw (RW r) = r == (rotateRackCcw $ rotateRackCw r)

prop_rotateCcwThenCw :: RackWrapper -> Bool
prop_rotateCcwThenCw (RW r) = r == (rotateRackCw $ rotateRackCcw r)

prop_solveIncludesExpectedSolution :: MoveMakersWrapper -> Property
prop_solveIncludesExpectedSolution (MMW mms) =
    r' /= r ==> sm `elem` ss
    where
        r = correctRack
        (r', ms) = applyMoveMakers mms r
        sm = oppositeMoves ms
        ss = solve r'

main :: IO ()
main = do
    results1 <- mapM quickCheckResult [
            prop_rotateCwThreeTimes,
            prop_rotateCcwThreeTimes,
            prop_rotateCwThenCcw,
            prop_rotateCcwThenCw
        ]
    results2 <- mapM quickCheckResult [
            prop_solveIncludesExpectedSolution
        ]
    if all isSuccess $ results1 ++ results2
        then return ()
        else exitFailure
