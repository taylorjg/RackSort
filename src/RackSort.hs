import           Data.List           (intersperse, minimumBy)
import           Data.List.Split     (splitOn)
import           Data.Maybe          (fromJust, maybeToList)
import           System.Console.ANSI

type Rack = String

data Move
    = Swap Int Int Rack Rack
    | RotateCw Rack Rack
    | RotateCcw Rack Rack
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

solve :: Rack -> [Solution]
solve r =
    search finished refine (r, [])
    where
        finished :: Partial -> Maybe Solution
        finished (r1, ms)
            | r1 == correctRack = Just (reverse ms)
            | otherwise = Nothing

        pickBestOrientation :: Rack -> (Rack, [(Int, [Int])], Maybe Move)
        pickBestOrientation r1 = res
            where
                r2 = rotateRackCw r1
                r3 = rotateRackCcw r1
                res1 = (r1, wrongBalls r1, Nothing)
                res2 = (r2, wrongBalls r2, Just $ RotateCw r1 r2)
                res3 = (r3, wrongBalls r3, Just $ RotateCcw r1 r3)
                res = minimumBy f [res1, res2, res3]
                f (_, w1, _) (_, w2, _) = length w1 `compare` length w2

        refine :: (Partial -> [Partial])
        refine (r1, ms) =
            foldl op1 [] bestw
            where
                (bestr, bestw, mm) = pickBestOrientation r1
                ms' = (maybeToList mm) ++ ms
                op1 :: [Partial] -> (Int, [Int]) -> [Partial]
                op1 acc1 (fromIdx, toIdxs) =
                    acc1 ++ foldl op2 [] toIdxs
                    where
                        op2 :: [Partial] -> Int -> [Partial]
                        op2 acc2 toIdx =
                            acc2 ++ [p']
                            where
                                r' = swapBalls bestr fromIdx toIdx
                                m = Swap fromIdx toIdx bestr r'
                                p' = (r', m:ms')

renderRack :: Rack -> [Int] -> [String]
renderRack r highlightIdxs =
    map renderLine [0..4]
    where
        renderLine n =
            padding ++ concat (intersperse " " colours) ++ padding
            where
                padding = replicate (4 - n) ' '
                idxs = take (n + 1) $ drop (sum [1..n]) [0..14]
                colours = map getColour idxs
                getColour idx
                    | idx `elem` highlightIdxs = ['`', colour, '`']
                    | otherwise = [colour]
                    where
                        colour = r !! idx

putStrLnWithHighlights :: String -> IO ()
putStrLnWithHighlights = writeTextSegments . lineToTextSegments
    where
        writeTextSegments textSegments = do
            mapM_ (\(ts, w) -> w ts) categorisedTextSegments
            putStrLn ""
            where
                categorisedTextSegments = categoriseTextSegments textSegments
        lineToTextSegments line = splitOn "`" line
        categoriseTextSegments textSegments =
            map (\(ts, idx) -> (ts, chooseWriter idx)) $ zip textSegments [0..]
            where
                chooseWriter :: Int -> String -> IO ()
                chooseWriter n
                    | even n = normalWriter
                    | otherwise = highlightingWriter
        normalWriter ts = putStr ts
        highlightingWriter ts = do
            setSGR [ SetColor Foreground Vivid Yellow ]
            putStr ts
            setSGR [ SetColor Foreground Dull White ]

printBeforeAndAfterRacks :: Rack -> Rack -> [Int] -> IO ()
printBeforeAndAfterRacks fromRack toRack highlightIdxs = do
    mapM_ putStrLnWithHighlights combinedLines
    where
        combinedLines = combineLines [
                (renderRack fromRack highlightIdxs),
                divider,
                (renderRack toRack highlightIdxs)
            ]
        divider = [
                replicate 10 ' ',
                replicate 10 ' ',
                replicate 4 ' ' ++ "=>" ++ replicate 4 ' ',
                replicate 10 ' ',
                replicate 10 ' '
            ]
        combineLines liness =
            loop liness (replicate 5 "")
            where
                loop [] ls = ls
                loop (xs:xss) ls = loop xss (zipWith (++) ls xs)

printMove :: Move -> IO ()
printMove (Swap fromIdx toIdx fromRack toRack) = do
    putStrLn $ "Swap index " ++ show fromIdx ++ " with " ++ show toIdx
    printBeforeAndAfterRacks fromRack toRack [fromIdx, toIdx]
    putStrLn $ replicate 28 '-'

printMove (RotateCw fromRack toRack) = do
    putStrLn "Rotate rack clockwise"
    printBeforeAndAfterRacks fromRack toRack []
    putStrLn $ replicate 28 '-'

printMove (RotateCcw fromRack toRack) = do
    putStrLn "Rotate rack counter clockwise"
    printBeforeAndAfterRacks fromRack toRack []
    putStrLn $ replicate 28 '-'

printSolution :: Solution -> IO ()
printSolution s = do
    putStrLn $ replicate 28 '-'
    mapM_ printMove s

main :: IO ()
main = do
    let r = "RRRRRRRYYYYYYYB"
    let ss = solve r
    putStrLn $ "Num solutions: " ++ show (length ss)
    putStrLn ""
    let s = minimumBy (\s1 s2 -> length s1 `compare` length s2) ss
    printSolution s
