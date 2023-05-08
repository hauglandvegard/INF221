module Game.Model.Room where

{-
The room is basically where everything happends in a Dungeon Crawler.
Here some rooms are desrcibed for context, but ideally would these be randomly
generated, which I did not have time for during this prosject. 
-}

import Game.Model.Item ( basicArmor, basicSword )
import Game.Model.Character ( Character, Loot )
import Tools.Grid as Grid
import Control.Monad.State ( State, modify, execState )
import Data.Array ( assocs )

data Room = Room {
    visited :: Bool,
    description :: String,
    coordinates :: Pos,
    enemies :: [Character],
    stash :: Loot
}

instance Eq Room where
    x == y = description x == description y

instance Show Room where
    show x = description x ++ " " ++ show (coordinates x) ++ (if visited x then "v/" else "x") -- ++ "\n"
          -- ++ "Enemies: " ++ (show $ enemies x) ++ "\n"
          -- ++ "Loot: " ++ (show $ loot x) ++ "\n"


visitRoom' :: State Room ()
visitRoom' = do
    modify $ \room -> room {visited = True}

visitRoom :: Room -> Room
visitRoom = execState visitRoom'

updateRoomCoordinates' :: Pos -> State Room ()
updateRoomCoordinates' pos = do
    modify $ \room -> room {coordinates = pos}

updateRoomCoordinates :: Room -> Pos -> Room
updateRoomCoordinates room pos = execState (updateRoomCoordinates' pos) room

startRoom :: Room
startRoom = Room {
    visited = True,
    description = "Starting Room",
    coordinates = (2,2),
    enemies = [],
    stash = [basicSword]
}

room1 :: Room
room1 = Room {
    visited = False,
    description = "Room 1",
    coordinates = (2,1),
    enemies = [],
    stash = [basicArmor]
}

room2 :: Room
room2 = Room {
    visited = False,
    description = "Room 2",
    coordinates = (1,1),
    enemies = [],
    stash = [basicArmor]
}

room3 :: Room
room3 = Room {
    visited = False,
    description = "Room 3",
    coordinates = (0,1),
    enemies = [],
    stash = [basicArmor]
}

room4 :: Room
room4 = Room {
    visited = False,
    description = "Room 4",
    coordinates = (0,2),
    enemies = [],
    stash = [basicArmor]
}

wall :: Room
wall = Room {
    visited = False,
    description = "This is a wall",
    coordinates = (-1,-1),
    enemies = [],
    stash = []
}

curRooms :: [(Pos, Room)]
curRooms = [
        (coordinates startRoom, startRoom),
        (coordinates room1, room1),
        (coordinates room2, room2),
        (coordinates room3, room3),
        (coordinates room4, room4)
        ]

roomsGrid :: Grid Room
roomsGrid = let grid = Grid.makeGrid 5 5 wall
                updated = Grid.updateCells grid curRooms
            in setRoomCoordinates updated

setRoomCoordinates :: Grid Room -> Grid Room
setRoomCoordinates grid = Grid.updateCells grid
    [(pos, updateRoomCoordinates room pos) | (pos, room) <- assocs $ cells grid]
