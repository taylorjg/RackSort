import Data.List (intersperse)

solved :: String -> Bool
solved = (=="RYRRBYYRYRRYYRY")

valid :: String -> Bool
valid r =
    length r == 15 &&
    numColour 'Y' 7 &&
    numColour 'R' 7 &&
    numColour 'B' 1
    where
        numColour :: Char -> Int -> Bool
        numColour c n = length (filter (==c) r) == n

draw :: String -> IO ()
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
    let r = "RYRRBYYRYRRYYRY"
    putStrLn $ "valid r = " ++ show (valid r)
    draw r
