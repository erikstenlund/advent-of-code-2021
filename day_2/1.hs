import System.Environment
import System.IO

type Position = (Int, Int)
type Command = (String, Int)

main = do
    (filename:_) <- getArgs
    content <- readFile filename
    let commands = map parseCommand $ lines content
    putStrLn $ show $ handleCommands commands (0, 0)

parseCommand rawString = (\(x,y) -> (x, (read::String->Int) y)) $ (\(x:y:[]) -> (x,y)) $ words rawString

handleCommands :: [Command] -> Position -> Position
handleCommands (cmd:[]) position = handleCommand cmd position
handleCommands (cmd:cmds) position = handleCommands cmds $ handleCommand cmd position

handleCommand ("down", val) (x, y) = (x, y - val)
handleCommand ("up", val) (x, y) = (x, y + val)
handleCommand ("forward", val) (x, y) = (x + val, y)