import System.Environment
import System.IO

main = do
    (filename:_) <- getArgs
    content <- readFile filename
    let depths = map (read::String->Int) $ lines content
    putStrLn $ show $ checkDepths depths

checkDepths (s1:s2:s3:s4:samples) =
    if secondWindow > firstWindow then 1 + checkDepths rest
    else checkDepths rest
    where
        firstWindow = s1 + s2 + s3
        secondWindow = s2 + s3 + s4
        rest = (s2:s3:s4:samples)
checkDepths (s1:s2:s3:[]) = 0