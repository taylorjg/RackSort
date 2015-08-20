module RackSortLib (
    Rack,
    Move(..),
    Solution,
    solve,
    correctRack,
    swapBalls,
    rotateRackCw,
    rotateRackCcw
) where

import           Data.List  (minimumBy)
import           Data.Maybe (fromJust)

type Rack = String

data Move
    = Swap Int Int Rack Rack
    | RotateCw Rack Rack
    | RotateCcw Rack Rack
    deriving (Show, Eq)

type Solution = [Move]

yellow, red, black :: Char
yellow = 'Y'
red = 'R'
black = 'B'

--         0
--       1   2
--     3   4   5
--   6   7   8   9
-- 10  11  12  13  14

yellowIndices :: [Int]
yellowIndices = [1, 5, 6, 8, 11, 12, 14]

redIndices :: [Int]
redIndices = [0, 2, 3, 7, 9, 10, 13]

blackIndices :: [Int]
blackIndices = [4]

coloursToIndices :: [(Char, [Int])]
coloursToIndices = [
        (yellow, yellowIndices),
        (red, redIndices),
        (black, blackIndices)
    ]

correctRack :: Rack
correctRack = "RYRRBYYRYRRYYRY"

swapBalls :: Rack -> Int -> Int -> Rack
swapBalls r idx1 idx2 =
    zipWith f [0..] r
    where
        f n b
            | n == idx1 = r !! idx2
            | n == idx2 = r !! idx1
            | otherwise = b

rotateRackCw :: Rack -> Rack
rotateRackCw r = map (r!!) [10, 11, 6, 12, 7, 3, 13, 8, 4, 1, 14, 9, 5, 2, 0]

rotateRackCcw :: Rack -> Rack
rotateRackCcw r = map (r!!) [14, 9, 13, 5, 8, 12, 2, 4, 7, 11, 0, 1, 3, 6, 10]

wrongBalls :: Rack -> [(Int, [Int])]
wrongBalls r =
    map (\(fromIdx, b, _) -> (fromIdx, toIdxs b)) $
    filter (\(_, b1, b2) -> b1 /= b2) xs
    where
        xs = zip3 [0..] r correctRack
        toIdxs b =
            filter (\n -> r !! n /= b) idxs
            where
                idxs = fromJust $ lookup b coloursToIndices

type Partial = (Rack, [Move])

search :: (Partial -> Maybe Solution)
        -> (Partial -> [Partial])
        -> Partial
        -> [Solution]
search finished refine emptysoln =
    generate emptysoln
    where
        generate partial
            | Just soln <- finished partial = [soln]
            | otherwise  = concatMap generate (refine partial)

pickBestOrientation :: Rack -> Partial
pickBestOrientation r = snd tbest
    where
        rcw = rotateRackCw r
        rccw = rotateRackCcw r
        t1 = (wrongBalls r, (r, []))
        t2 = (wrongBalls rcw, (rcw, [RotateCw r rcw]))
        t3 = (wrongBalls rccw, (rccw, [RotateCcw r rccw]))
        tbest = minimumBy minWrongBalls [t1, t2, t3]
        minWrongBalls (ws1, _) (ws2, _) = length ws1 `compare` length ws2

solve :: Rack -> [Solution]
solve rack =
    search finished refine emptysoln
    where
        finished :: Partial -> Maybe Solution
        finished (r, ms)
            | r == correctRack = Just (reverse ms)
            | otherwise = Nothing

        refine :: (Partial -> [Partial])
        refine (r, ms) =
            foldl op1 [] (wrongBalls r)
            where
                op1 :: [Partial] -> (Int, [Int]) -> [Partial]
                op1 acc1 (fromIdx, toIdxs) =
                    acc1 ++ foldl op2 [] toIdxs
                    where
                        op2 :: [Partial] -> Int -> [Partial]
                        op2 acc2 toIdx =
                            acc2 ++ [p']
                            where
                                r' = swapBalls r fromIdx toIdx
                                m = Swap fromIdx toIdx r r'
                                p' = (r', m:ms)

        emptysoln :: Partial
        emptysoln = pickBestOrientation rack
