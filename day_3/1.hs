import System.Environment
import System.IO
import Data.Char(digitToInt)

main = do
    (filename:_) <- getArgs
    content <- readFile filename
    let input = map preProcess $ lines content
    putStrLn . prepareOutput . algorithm $ input

preProcess = map digitToInt
prepareOutput = show

algorithm xs = binaryToDec gammaRate * binaryToDec epsilon
    where gammaRate = calculateGammaRate (foldl sumLists (replicate 12 0) xs) (length xs)
          epsilon = invertBinary gammaRate

sumLists _ [] = []
sumLists [] _ = []
sumLists (x:xs) (y:ys) = [x+y] ++ sumLists xs ys

calculateGammaRate [] _ = []
calculateGammaRate (x:xs) nInput = 
        if x > nInput `div` 2 
            then [1] ++ calculateGammaRate xs nInput
            else [0] ++ calculateGammaRate xs nInput

invertBinary [] = []
invertBinary (x:xs) =
        if x == 1
            then [0] ++ invertBinary xs
            else [1] ++ invertBinary xs

binaryToDec = foldl (\acc digit -> acc * 2 + digit) 0