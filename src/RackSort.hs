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

findSolutions :: Rack -> [Solution]
findSolutions r = []

draw :: Rack -> IO ()
draw r =
    mapM_ putStrLn $ map renderLine [0..4]
    where
        renderLine n =
            padding ++ (intersperse ' ' colours)
            where
                padding = replicate (4 - n) ' '
                colours = take (n + 1) $ drop (sum [1..n]) $ r

main :: IO ()
main = do
    -- let r = "RYRRBYYRYRRYYRY"
    -- putStrLn $ "finishedRack r = " ++ show (finishedRack r)
    -- putStrLn $ "validRack r = " ++ show (validRack r)
    -- draw r
    -- putStrLn $ show (wrongness "RRRRRRRYYYYYYYB")
    mapM_ putStrLn $ map show (wrongness "RRRRRRRYYYYYYYB")
