import           RackSortLib
import           System.Exit     (exitFailure)
import           Test.QuickCheck

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
    numMoves <- choose (1, 10)
    vectorOf numMoves genMoveMaker

genMoveMaker :: Gen MoveMaker
genMoveMaker = oneof [genSwap, genRotateCw, genRotateCcw]

genSwap :: Gen MoveMaker
genSwap = do
    fromIdx <- elements [0..14]
    toIdx <- elements $ filter (/=fromIdx) [0..14]
    return $ SwapMaker fromIdx toIdx

genRotateCw :: Gen MoveMaker
genRotateCw = return RotateCwMaker

genRotateCcw :: Gen MoveMaker
genRotateCcw = return RotateCcwMaker

genRack :: Gen Rack
genRack = undefined

main :: IO ()
main = do
    return ()
