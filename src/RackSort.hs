import Data.List (intersperse, sortBy)
import Data.Maybe (fromJust)

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
yellowIndices = [1,5,6,8,11,12,14]

redIndices :: [Int]
redIndices = [0,2,3,7,9,10,13]

blackIndices :: [Int]
blackIndices = [4]

coloursToIndices :: [(Char,[Int])]
coloursToIndices = [
        (yellow, yellowIndices),
        (red, redIndices),
        (black, blackIndices)
    ]

correctRack :: Rack
correctRack = "RYRRBYYRYRRYYRY"

wrongnesses :: Rack -> [(Char,Int,[Int])]
wrongnesses r =
    sortBy (\(b1, _, _) (b2, _, _) -> b1 `compare` b2) $
    map (\(idx, b, _) -> (b, idx, wrongIndices b)) $
    filter (\(_, b1, b2) -> b1 /= b2) xs
    where
        xs = zip3 [0..] r correctRack
        wrongIndices b =
            filter (\n -> r !! n /= b) idxs
            where
                idxs = fromJust $ lookup b coloursToIndices

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
    reverse $ snd $ loop (r,[]) ws
    where
        ws = wrongnesses r
        loop :: (Rack,[Move]) -> [(Char,Int,[Int])] -> (Rack,[Move])
        loop t [] = t
        loop (r1,ms) ((_, idx1, idx2:_):_) =
            loop (r2,m:ms) ws2
            where
                r2 = swapBalls r1 idx1 idx2
                m = Swap idx1 idx2 r1 r2
                ws2 = wrongnesses r2

drawRack :: Rack -> IO ()
drawRack r =
    mapM_ (putStrLn . renderLine) [0..4]
    where
        renderLine n =
            padding ++ intersperse ' ' colours
            where
                padding = replicate (4 - n) ' '
                colours = take (n + 1) $ drop (sum [1..n]) r

main :: IO ()
main = do

    let r1 = "RRRRRRRYYYYYYYB"
    drawRack r1
    let s1 = solve r1
    mapM_ print s1

    let r2 = rotateRackCw r1
    drawRack r2
    let s2 = solve r2
    mapM_ print s2

    let r3 = rotateRackCcw r1
    drawRack r3
    let s3 = solve r3
    mapM_ print s3
