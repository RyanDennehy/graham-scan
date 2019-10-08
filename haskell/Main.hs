import System.Environment (getArgs)
import GrahamScan

-- interactWith
-- Interact with input and output
interactWith function inputFile outputFile = do
    rawInput <- readFile inputFile
    let input = toPoints rawInput
    let prettyInput = myFormat input
    putStr "input:  "
    putStrLn prettyInput
    let prettyOutput = myFormat (function input)
    putStr "output: "
    putStrLn prettyOutput
    writeFile outputFile prettyOutput

-- myFormat
-- Formats a list of items into a string
myFormat :: Show a => [a] -> String
myFormat xs = unwords (foldl (\acc x -> acc ++ [(show x)]) [] xs)

main = mainWith myFunction
    where mainWith function = do
            args <- getArgs
            case args of
                [input, output] -> interactWith function input output
                _      -> putStrLn "error: exactly two arguments needed"
          myFunction = grahamScan
