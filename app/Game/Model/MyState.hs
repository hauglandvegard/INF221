module Game.Model.MyState where

{-
This is the module which ties all the datatypes together and creates the GameState
which is passed around like a hot potato. Do be alarmed, here be monads. 
-}
import Game.Model.Character ( basicPlayer, Character )
import Game.Model.Room ( roomsGrid, startRoom, Room(coordinates) )
import Tools.Grid as Grid ( updateCell, Grid(cells) )
import Data.Array ( elems )
import Control.Monad.State

data Screen
    = MenuScreen
    | InRoomScreen
    | MoveScreen
    | SearchScreen
    | CharacterInfoScreen
    | InventoryScreen
    | WeaponArmorScreen
    | ConsumableScreen
    deriving (Eq, Ord, Show)

data World = World {
    currentRoom :: Room,
    grid :: Grid Room
} deriving Show


startWorld = World {
    currentRoom = startRoom,
    grid = roomsGrid
}

data GameState = GameState {
    text :: String,
    currentScreen :: Screen,
    player :: Character,
    world :: World,
    flag :: Int
} deriving Show

startState :: GameState
startState = GameState {
    text = "",
    currentScreen = InRoomScreen,
    player = basicPlayer,
    world = startWorld,
    flag = 0
}

-- Dirty monad functions
updateRoomInState' :: Room -> State GameState ()
updateRoomInState' room = do
    gs <- get

    let wr = world gs
        gr = grid wr

    modify $ \gs -> gs {world = wr {grid = updateCell gr (coordinates room) room}}

getRooms' :: State GameState [Room]
getRooms' = do
    gs <- get

    let w = world gs
        g = grid w

    return (elems $ cells g)

-- Clean nice and friendly pure functions
updateRoomInState :: GameState -> Room -> GameState
updateRoomInState gs room = execState (updateRoomInState' room) gs

getRooms :: GameState -> [Room]
getRooms = evalState getRooms'
