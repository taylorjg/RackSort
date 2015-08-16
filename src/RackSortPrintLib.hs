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
