--module GrahamScan where

import MergeSort
import System.Environment (getArgs)

interactWith function inputFile outputFile = do
    input <- readFile inputFile
    let finput = toInts input
    let fout = myFormat finput
    putStr "input:  "
    putStrLn fout
    let output = myFormat (function finput)
    putStr "output: "
    putStrLn output
    writeFile outputFile output
    --let output = function input
    --putStr "output: "
    --putStrLn (myFormat output)
    --writeFile outputFile (myFormat output)

toInts :: String -> [Int]
toInts ss = map (\r -> read r :: Int) (words ss)

myFormat :: Show a => [a] -> String
myFormat xs = unwords (foldl (\acc x -> acc ++ [(show x)]) [] xs)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
                [input, output] -> interactWith function input output
                _      -> putStrLn "error: exactly two arguments needed"
          myFunction = mergeWrapper
