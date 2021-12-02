import System.Environment
import System.IO

type Position = (Int, Int, Int) -- horizontal, depth, aim
type Command = (String, Int)

main = do
    (filename:_) <- getArgs
    content <- readFile filename
    let commands = map parseCommand $ lines content
    putStrLn $ show $ handleCommands commands (0, 0, 0)

parseCommand = secondValueToInt . createPair . words
    where 
        secondValueToInt (x,y) = (x, (read::String->Int) y)
        createPair (x:y:[]) = (x, y)

handleCommands :: [Command] -> Position -> Position
handleCommands (cmd:[]) position = handleCommand cmd position
handleCommands (cmd:cmds) position = handleCommands cmds $ handleCommand cmd position

handleCommand ("down", val) (x, y, aim) = (x, y, aim + val)
handleCommand ("up", val) (x, y, aim) = (x, y, aim - val)
handleCommand ("forward", val) (x, y, aim) = (x + val, y + val * aim, aim)