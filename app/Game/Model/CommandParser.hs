module Game.Model.CommandParser (
    commandParser,
    Command(..)
 ) where 

{-
This module is responsable for recieving a character and returning the correct
Command datatype. Nothing magic going on here, just a lot of PURE FUNCTIONS.
-}
import Game.Model.Room ( Room )
import Game.Model.MyState ( Screen(..) )
import Tools.Grid ( Direction(..) )

data Command
    = ChScreen Screen
    | StartNew
    | Save
    | Load
    | Space
    | Move Direction
    | MoveTo Room
    | Next
    | Previous
    | Equip
    | Drop
    | Consume
    | Select
    | Help
    | Quit 
    | Error String
    | NullCommand
    deriving (Show)

commandParser :: Char -> Screen -> Command
commandParser cmd screen = 
    case screen of
        MenuScreen          -> parseMenu cmd
        InRoomScreen        -> parseInRoom cmd
        MoveScreen          -> parseMove cmd
        SearchScreen        -> parseSearch cmd
        CharacterInfoScreen -> parseCharacterInfo cmd
        InventoryScreen     -> parseInventory cmd
        WeaponArmorScreen   -> parseWeaponArmor cmd
        ConsumableScreen    -> parseConsumable cmd

parseMenu :: Char -> Command
parseMenu cmd = 
    case cmd of
        '1' -> StartNew              -- (1) Start new game
        '2' -> Save                  -- (2) Save game
        '3' -> Load                  -- (3) Load game
        'b' -> ChScreen InRoomScreen -- (b) Back
        'q' -> Quit                  -- (q) Quit
        _   -> NullCommand

parseInRoom :: Char -> Command
parseInRoom cmd = 
    case cmd of
        '1' -> ChScreen MoveScreen           -- (1) Move
        '2' -> ChScreen SearchScreen         -- (2) Search for loot
        'i' -> ChScreen InventoryScreen      -- (i) Open inventory
        'c' -> ChScreen CharacterInfoScreen  -- (c) Character stats
        'm' -> ChScreen MenuScreen           -- (m) Menu
        _   -> NullCommand

parseMove :: Char -> Command
parseMove cmd = 
    case cmd of
        '1' -> Move North            -- (1/w) North
        'w' -> Move North            -- (1/w) North
        '2' -> Move West             -- (2/a) West
        'a' -> Move West             -- (2/a) West
        '3' -> Move South            -- (3/s) South
        's' -> Move South            -- (3/s) South
        '4' -> Move East             -- (4/d) East
        'd' -> Move East             -- (4/d) East
        'b'        -> ChScreen InRoomScreen -- (b) Back
        _          -> NullCommand 

parseSearch :: Char -> Command
parseSearch cmd = 
    case cmd of
        'b' -> ChScreen InRoomScreen -- (b) back
        _   -> NullCommand 


parseCharacterInfo :: Char -> Command
parseCharacterInfo cmd = 
    case cmd of
        'b' -> ChScreen InRoomScreen -- (b) Back
        _   -> NullCommand

parseInventory :: Char -> Command
parseInventory cmd = 
    case cmd of
        'w' -> Previous
        's' -> Next
        ' ' -> Select
        'b' -> ChScreen InRoomScreen
        _   -> NullCommand

parseWeaponArmor :: Char -> Command
parseWeaponArmor cmd = 
    case cmd of
        '1' -> Equip                    -- (1) Equip
        '2' -> Drop                     -- (2) Drop
        'b' -> ChScreen InventoryScreen -- (b) Back
        _   -> NullCommand 

parseConsumable :: Char -> Command
parseConsumable cmd = 
    case cmd of
        '1' -> Consume                   -- (1) Consume
        '2' -> Drop                      -- (2) Drop
        'b' -> ChScreen InventoryScreen  -- (b) Back
        _   -> NullCommand 