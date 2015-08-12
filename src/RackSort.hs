import Data.List (intersperse, sortBy)
import Data.Maybe (fromJust)

type Rack = String

data Move
    = Swap Int Int Rack Rack
    -- | Cw String Rack
    -- | Ccw String Rack
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
    let s = solve r1
    mapM_ print $ s
    let lastMove = last s
    case lastMove of
        Swap _ _ _ r2 -> drawRack r2
        _ -> return  ()
