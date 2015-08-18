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

applyMoveMakers :: Rack -> [MoveMaker] -> (Rack, [Move])
applyMoveMakers r mms =
    (a, reverse b)
    where
        (a, b) = foldl op (r, []) mms
        op (r, ms) (SwapMaker fromIdx toIdx) =
            (r', m:ms)
            where
                r' = swapBalls r fromIdx toIdx
                m = Swap fromIdx toIdx r r'
        op (r, ms) (RotateCwMaker) =
            (r', m:ms)
            where
                r' = rotateRackCw r
                m = RotateCw r r'
        op (r, ms) (RotateCcwMaker) =
            (r', m:ms)
            where
                r' = rotateRackCcw r
                m = RotateCcw r r'

newtype MoveMakersWrapper = MMW [MoveMaker] deriving Show

genMoveMakers :: Gen MoveMakersWrapper
genMoveMakers = do
    maybeRotate <- frequency [
            (1, return Nothing),
            (1, return $ Just RotateCwMaker),
            (1, return $ Just RotateCcwMaker)
        ]
    numSwaps <- choose (1, 5)
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

prop_DummyTest :: RackWrapper -> MoveMakersWrapper -> Bool
prop_DummyTest (RW r) (MMW _) = head r /= 'B'

main :: IO ()
main = do
    r1 <- quickCheckResult $ noShrinking prop_DummyTest
    if all isSuccess [r1]
        then return ()
        else exitFailure
