module Game.Gameloop
( startGame,
  loop
) where

{-
This is the point that is tying everything together. The main part is the loop
funcion which coninues to run until it recieves a quit command. This module contains
no pure functions which is intended. 
-}
import Tools.Grid as Grid
import Game.View.View
import Game.Model.Item
import Game.Model.MyState
import Game.Model.Character
import Game.Model.Room
import Game.Model.CommandParser
import Control.Monad.State      -- State Monad
import System.Console.Haskeline -- Used to enable reading single input for user
import Game.Model.MyState

startGame :: IO()
startGame = setUp

setUp :: IO()
setUp = do
    putStrLn "Starting game..."
    loop startState

getInput :: InputT IO Char
getInput = do
  event <- getInputChar ""
  maybe getInput return event

loop :: GameState -> IO()
loop gs = do
    drawScreen gs

    input <- runInputT defaultSettings getInput
    let cmd = commandParser input (currentScreen gs)

    case cmd of
        StartNew        -> setUp
        Save            -> save gs
        Load            -> load gs
        ChScreen sc     -> changeScreen gs cmd
        Move dir        -> move gs dir
        Next            -> nItem gs
        Previous        -> pItem gs
        Equip           -> eItem gs
        Drop            -> dItem gs
        Consume         -> cItem gs
        Select          -> sItem gs
        Help            -> help gs
        Quit            -> quit
        Error err       -> errorHandler gs err
        NullCommand     -> do putStrLn "Null"; loop gs

updateState :: Command -> StateT GameState IO ()
updateState cmd = do
    gs <- get

    case cmd of
        ChScreen sc -> put $ gs {currentScreen = sc,
                                 text = "Switched to " ++ show sc}
        MoveTo room -> do
            if room == wall
                then put $ gs {text = "You walked into a wall. Success?"}
                else put $ gs {text = "You walked into a new room!",
                               world = (world gs) {currentRoom = room}}
        Error x     -> put $ gs {text = x}
        Next        -> put $ gs {player = (player gs) {inventory = nextItem (inventory $ player gs)}}
        Previous    -> put $ gs {player = (player gs) {inventory = previousItem (inventory $ player gs)}}
        _           -> put $ gs {text = "Nothing happened..."}

-- TODO: Impement
save :: GameState -> IO()
save gs = do
    putStrLn "Saved"
    loop gs

-- TODO: Implement
load :: GameState -> IO()
load gs = do
    putStrLn "Loaded"
    loop gs

-- Changes the current screen in the GameState to a new one based on the Command
changeScreen :: GameState -> Command -> IO()
changeScreen gs cmd = do
    newGs <- execStateT (updateState cmd) gs
    loop newGs

-- TODO: Implement
move :: GameState -> Direction -> IO()
move gs dir = do
    let w        = world gs
        pos      = coordinates $ currentRoom w
        newPos   = Grid.getPos pos dir
        roomGrid = grid w
        newRoom  = Grid.getCell roomGrid newPos

    case newRoom of
        Nothing -> do
            newGs <- execStateT (updateState (Error "You tried to walk of the end of the world...")) gs
            loop newGs

        Just room -> do
            let vRoom = visitRoom room
                updatedGs = updateRoomInState gs vRoom

            newGs <- execStateT (updateState (MoveTo room)) updatedGs
            loop newGs

-- TODO: Implement
nItem :: GameState -> IO()
nItem gs = do
    newGs <- execStateT (updateState Next) gs
    loop newGs

-- TODO: Implement
pItem :: GameState -> IO()
pItem gs = do
    newGs <- execStateT (updateState Previous) gs
    loop newGs

-- TODO: Implement
eItem :: GameState -> IO()
eItem gs = do
    putStrLn "Equipted"
    loop gs

-- TODO: Implement
dItem :: GameState -> IO()
dItem gs = do
    putStrLn "Dropped"
    loop gs

-- TODO: Implement
cItem :: GameState -> IO()
cItem gs = do
    putStrLn "Consumed"
    loop gs

-- TODO: Implement
sItem :: GameState -> IO()
sItem gs = do
    putStrLn "Selected"
    loop gs

-- TODO: Implement
help :: GameState -> IO()
help gs = do
    putStrLn "Help"
    loop gs

-- TODO: Implement
quit :: IO()
quit = do
    putStrLn "Quit"

-- TODO: Implement
errorHandler :: GameState -> String -> IO()
errorHandler gs err = do
    putStrLn "error"
    loop gs
