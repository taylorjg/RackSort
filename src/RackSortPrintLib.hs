module RackSortPrintLib (
    printSolution
) where

import           Data.List           (intersperse)
import           Data.List.Split     (splitOn)
import           RackSortLib
import           System.Console.ANSI

renderRack :: Rack -> [Int] -> [String]
renderRack r highlightIdxs =
    map renderLine [0..4]
    where
        renderLine n =
            padding ++ unwords colours ++ padding
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
putStrLnWithHighlights = writeTextSegments . splitLine
    where
        writeTextSegments tss = sequence_ $ categoriseTextSegments tss ++ [putStrLn ""]
        splitLine = splitOn "`"
        categoriseTextSegments = zipWith id (cycle [w1, w2])
        w1 = putStr
        w2 ts = do
            setSGR [ SetColor Foreground Vivid Yellow ]
            putStr ts
            setSGR [ Reset ]

printBeforeAndAfterRacks :: Rack -> Rack -> [Int] -> IO ()
printBeforeAndAfterRacks fromRack toRack highlightIdxs =
    mapM_ putStrLnWithHighlights combinedLines
    where
        combinedLines = combineLines [
                renderRack fromRack highlightIdxs,
                arrow,
                renderRack toRack highlightIdxs
            ]
        arrow = [
                replicate 10 ' ',
                replicate 10 ' ',
                replicate 4 ' ' ++ "=>" ++ replicate 4 ' ',
                replicate 10 ' ',
                replicate 10 ' '
            ]
        combineLines = foldl (zipWith (++)) (replicate 5 "")

printMove :: Move -> IO ()
printMove (Swap fromIdx toIdx fromRack toRack) = do
    putStrLn $ "Swap index " ++ show fromIdx ++ " with " ++ show toIdx
    printBeforeAndAfterRacks fromRack toRack [fromIdx, toIdx]

printMove (RotateCw fromRack toRack) = do
    putStrLn "Rotate rack clockwise"
    printBeforeAndAfterRacks fromRack toRack []

printMove (RotateCcw fromRack toRack) = do
    putStrLn "Rotate rack counter clockwise"
    printBeforeAndAfterRacks fromRack toRack []

printDivider :: IO ()
printDivider = putStrLn $ replicate 28 '-'

printSolution :: Solution -> IO ()
printSolution s = do
    printDivider
    sequence_ $ intersperse printDivider (printMove `fmap` s)
    printDivider
