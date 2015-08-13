import           Data.List  (intersperse, sortBy, minimumBy)
import           Data.Maybe (fromJust)

type Rack = String

data Move
    = Swap Int Int Rack Rack
    -- | RotateCw Rack Rack
    -- | RotateCcw Rack Rack
    deriving Show

type Solution = [Move]

yellow, red, black :: Char
yellow = 'Y'
red = 'R'
black = 'B'

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

-- rotateRackCw :: Rack -> Rack
-- rotateRackCw r = map (r!!) [10, 11, 6, 12, 7, 3, 13, 8, 4, 1, 14, 9, 5, 2, 0]

-- rotateRackCcw :: Rack -> Rack
-- rotateRackCcw r = map (r!!) [14, 9, 13, 5, 8, 12, 2, 4, 7, 11, 0, 1, 3, 6, 10]

wrongBalls :: Rack -> [(Char, Int, [Int])]
wrongBalls r =
    sortBy (\(b1, _, _) (b2, _, _) -> b1 `compare` b2) $
    map (\(idx, b, _) -> (b, idx, availableIndices b)) $
    filter (\(_, b1, b2) -> b1 /= b2) xs
    where
        xs = zip3 [0..] r correctRack
        availableIndices b =
            filter (\n -> r !! n /= b) correctIndices
            where
                correctIndices = fromJust $ lookup b coloursToIndices

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
            | otherwise  = concatMap generate $ refine partial

solve :: Rack -> [Solution]
solve r =
    search finished refine (r, [])
    where
        finished :: Partial -> Maybe Solution
        finished (r, ms)
            | r == correctRack = Just $ reverse ms
            | otherwise = Nothing

        refine :: (Partial -> [Partial])
        refine p@(r, ms) =
            foldl op1 [] ws
            where
                ws = wrongBalls r
                op1 :: [Partial] -> (Char, Int, [Int]) -> [Partial]
                op1 acc1 (_, fromIdx, toIdxs) =
                    acc1 ++ foldl op2 [] toIdxs
                    where
                        op2 :: [Partial] -> Int -> [Partial]
                        op2 acc2 toIdx =
                            acc2 ++ [p']
                            where
                                r' = swapBalls r fromIdx toIdx
                                m = Swap fromIdx toIdx r r'
                                p' = (r', m:ms)

printRack :: Rack -> IO ()
printRack r =
    mapM_ (putStrLn . renderLine) [0..4]
    where
        renderLine n =
            padding ++ intersperse ' ' colours
            where
                padding = replicate (4 - n) ' '
                colours = take (n + 1) $ drop (sum [1..n]) r

printSolution :: Solution -> IO ()
printSolution s = do
    mapM_ print s

main :: IO ()
main = do
    let r = "RRRRRRRYYYYYYYB"
    printRack r
    putStrLn ""

    let ss = solve r
    putStrLn $ "Num solutions: " ++ show (length ss)
    putStrLn ""

    let s = minimumBy (\a b -> length a `compare` length b) ss
    printSolution s
    putStrLn ""

    let Swap _ _ _ r2 = last s
    printRack r2
    putStrLn ""
