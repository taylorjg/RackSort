import Data.List (intersperse)
import Data.Maybe (fromJust)

type Rack = String

data Move
    = Swap Int Int Rack Rack
    -- | Cw String Rack
    -- | Ccw String Rack

type Solution = [Move]

yellowIndices :: [Int]
yellowIndices = [1,5,6,8,11,12,14]

redIndices :: [Int]
redIndices = [0,2,3,7,9,10,13]

blackIndices :: [Int]
blackIndices = [4]

coloursToIndices :: [(Char,[Int])]
coloursToIndices = [
        ('Y', yellowIndices),
        ('R', redIndices),
        ('B', blackIndices)
    ]

correctRack :: Rack
correctRack = "RYRRBYYRYRRYYRY"

isCorrectRack :: Rack -> Bool
isCorrectRack = (==correctRack)

isValidRack :: Rack -> Bool
isValidRack r =
    length r == 15 &&
    numColour 'Y' 7 &&
    numColour 'R' 7 &&
    numColour 'B' 1
    where
        numColour c n = length (filter (==c) r) == n

wrongness :: Rack -> [(Char,Int,[Int])]
wrongness r =
    map (\(idx, a, _) -> (a, idx, wrongIndices r a idx)) $
    filter (\(_, a, b) -> a /= b) xs
    where
        xs = zip3 [0..] r correctRack

wrongIndices :: Rack -> Char -> Int -> [Int]
wrongIndices r c idx =
    filter (\n -> r !! n /= c && n /= idx) idxs
    where
        idxs = fromJust $ lookup c coloursToIndices

swapBalls :: Rack -> Int -> Int -> Rack
swapBalls r idx1 idx2 =
    zipWith f [0..] r
    where
        b1 = r !! idx1
        b2 = r !! idx2
        f n _ | n == idx1 = b2
        f n _ | n == idx2 = b1
        f _ b = b

solve :: Rack -> Rack
solve r =
    loop r ws
    where
        ws = wrongness r
        loop :: Rack -> [(Char,Int,[Int])] -> Rack
        loop r [] = r
        loop r ((_, idx1, idx2:_):_) =
            loop r2 ws2
            where
                r2 = swapBalls r idx1 idx2
                ws2 = wrongness r2

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
    let r2 = solve r1
    drawRack r2
