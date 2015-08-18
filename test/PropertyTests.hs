{-# LANGUAGE FlexibleInstances #-}

import           RackSortLib
import           System.Exit     (exitFailure)
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

genMoveMakers :: Gen [MoveMaker]
genMoveMakers = do
    -- TODO: ideally, optional RotateCw/RotateCcw followed by some Swap
    numMoves <- choose (1, 6)
    vectorOf numMoves genMoveMaker

genMoveMaker :: Gen MoveMaker
genMoveMaker = frequency [
        (10, genSwap),
        (1, genRotateCw),
        (1, genRotateCcw)
    ]

genSwap :: Gen MoveMaker
genSwap = do
    fromIdx <- elements [0..14]
    toIdx <- elements $ filter (/=fromIdx) [0..14]
    return $ SwapMaker fromIdx toIdx

genRotateCw :: Gen MoveMaker
genRotateCw = return RotateCwMaker

genRotateCcw :: Gen MoveMaker
genRotateCcw = return RotateCcwMaker

instance Arbitrary MoveMaker where
    arbitrary = genMoveMaker

instance Arbitrary [MoveMaker] where
    arbitrary = genMoveMakers

newtype RackWrapper = RackWrapper Rack deriving Show

genRackWrapper :: Gen RackWrapper
genRackWrapper = do
    r <- shuffle correctRack
    return (RackWrapper r)

instance Arbitrary RackWrapper where
    arbitrary = genRackWrapper

prop_DummyTest :: RackWrapper -> [MoveMaker] -> Bool
prop_DummyTest (RackWrapper r) _ = head r /= 'B'

main :: IO ()
main = do
    r1 <- quickCheckResult $ noShrinking prop_DummyTest
    if all isSuccess [r1]
        then return ()
        else exitFailure
