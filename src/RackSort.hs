import           Data.List  (intersperse, sortBy)
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

wrongnesses :: Rack -> [(Char, Int, [Int])]
wrongnesses r =
    sortBy (\(b1, _, _) (b2, _, _) -> b1 `compare` b2) $
    map (\(idx, b, _) -> (b, idx, destinationIndices b)) $
    filter (\(_, b1, b2) -> b1 /= b2) xs
    where
        xs = zip3 [0..] r correctRack
        destinationIndices b =
            filter (\n -> r !! n /= b) correctIndices
            where
                correctIndices = fromJust $ lookup b coloursToIndices

swapBalls :: Rack -> Int -> Int -> Rack
swapBalls r idx1 idx2 =
    zipWith f [0..] r
    where
        b1 = r !! idx1
        b2 = r !! idx2
        f n _ | n == idx1 = b2
        f n _ | n == idx2 = b1
        f _ b = b

rotateRackCw :: Rack -> Rack
rotateRackCw r = map (r!!) [10, 11, 6, 12, 7, 3, 13, 8, 4, 1, 14, 9, 5, 2, 0]

rotateRackCcw :: Rack -> Rack
rotateRackCcw r = map (r!!) [14, 9, 13, 5, 8, 12, 2, 4, 7, 11, 0, 1, 3, 6, 10]

solve :: Rack -> Solution
solve r =
    reverse $ snd $ loop (r, [])
    where
        loop :: (Rack, [Move]) -> (Rack, [Move])
        loop t@(r1, ms) = case wrongnesses r1 of
            ((_, idx1, idx2:_):_) ->
                loop (r2, m:ms)
                where
                    r2 = swapBalls r1 idx1 idx2
                    m = Swap idx1 idx2 r1 r2
            _ -> t

solve2 :: Rack -> Move -> Solution
solve2 r initialMove =
    reverse $ snd $ loop (r, [initialMove])
    where
        loop :: (Rack, [Move]) -> (Rack, [Move])
        loop t@(r1, ms) = case wrongnesses r1 of
            ((_, idx1, idx2:_):_) ->
                loop (r2, m:ms)
                where
                    r2 = swapBalls r1 idx1 idx2
                    m = Swap idx1 idx2 r1 r2
            _ -> t

uberSolve :: Rack -> [Solution]
uberSolve r =
    foldl outerOp [] ws
    where
        ws = wrongnesses r
        outerOp :: [Solution] -> (Char, Int, [Int]) -> [Solution]
        outerOp outerAcc (_, fromIdx, toIdxs) =
            outerAcc ++ foldl innerOp [] toIdxs
            where
                innerOp :: [Solution] -> Int -> [Solution]
                innerOp innerAcc toIdx =
                    solve2 r' m : innerAcc
                    where
                        r' = swapBalls r fromIdx toIdx
                        m = Swap fromIdx toIdx r r'

drawRack :: Rack -> IO ()
drawRack r =
    mapM_ (putStrLn . renderLine) [0..4]
    where
        renderLine n =
            padding ++ intersperse ' ' colours
            where
                padding = replicate (4 - n) ' '
                colours = take (n + 1) $ drop (sum [1..n]) r

type Partial = (Rack, [Move])

isPartialFinished :: Partial -> Maybe Solution
isPartialFinished (r, ms)
    | r == correctRack = Just ms
    | otherwise = Nothing

refinePartial :: (Partial -> [Partial])
refinePartial p@(r, ms) = undefined

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

printSolution :: Solution -> IO ()
printSolution s = do
    mapM_ print s
    putStrLn ""

main :: IO ()
main = do
    let r = "RRRRRRRYYYYYYYB"
    let ss = uberSolve r
    mapM_ printSolution ss
