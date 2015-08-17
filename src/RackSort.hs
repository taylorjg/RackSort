import           RackSortLib
import           RackSortPrintLib
import           Data.List        (minimumBy)

main :: IO ()
main = do
    let r = "RRRRRRRYYYYYYYB"
    -- let r = rotateRackCw correctRack
    let ss = solve r
    putStrLn $ "Num solutions: " ++ show (length ss)
    putStrLn ""
    let s = minimumBy (\s1 s2 -> length s1 `compare` length s2) ss
    printSolution s
