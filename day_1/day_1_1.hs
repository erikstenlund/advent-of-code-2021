import System.Environment
import System.IO

main = do
    (filename:_) <- getArgs
    content <- readFile filename
    let depths = map (read::String->Int) $ lines content
    putStrLn $ show $ checkDepths depths

checkDepths (x:y:xs) =
    if y > x then 1 + checkDepths (y:xs)
    else checkDepths (y:xs)
checkDepths (x:[]) = 0