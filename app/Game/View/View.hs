module Game.View.View where

{-
This module is responsable for everything that is happening on the screen. It utilises
the different instances of show that are given for different datatypes. 
-}
import Game.Model.Character ( Character(inventory) )
import Game.Model.Room ( wall, Room(coordinates, visited) )
import Game.Model.MyState 
import System.FilePath ((</>))
import System.Directory () --Used only to check if file exists
import qualified Data.Map.Strict as Map -- Used to create dictionary type where I can use the data type as a key
import Game.View.Formatting

path :: String -> String
path str = "app" </> "assets" </> "Rooms" </> str
-- path str = "assets" </> "Rooms" </> str

screenFiles :: Map.Map Screen FilePath
screenFiles = Map.fromList
  [ (MenuScreen, "Menu")
  , (InRoomScreen, "InRoom")
  , (MoveScreen, "Move")
  , (SearchScreen, "Search")
  , (CharacterInfoScreen, "CharacterInfo")
  , (InventoryScreen, "Inventory")
  , (WeaponArmorScreen, "WeaponArmor")
  , (ConsumableScreen, "Consumable")
  ]

drawChoises ::Screen -> IO()
drawChoises cs = do
    let screenFile = screenFiles Map.! cs
        textPosition = (2, 27)

    content <- readFile $ path screenFile

    writeMultiLineAt textPosition content


drawVerticalLine :: Int -> Int -> IO()
drawVerticalLine _ 0 = putStr ""
drawVerticalLine x y = do
    writeAtCell (x, y) "|"
    drawVerticalLine x (y-1)

drawHorisontalLine :: Int -> Int -> IO()
drawHorisontalLine 0 _ = putStr ""
drawHorisontalLine x y = do
    writeAtCell (x, y) "="
    drawHorisontalLine (x-1) y

drawBounds :: IO()
drawBounds = do
    drawVerticalLine  0 29
    drawVerticalLine  90 29
    drawHorisontalLine 89 0
    drawHorisontalLine 89 30

    writeAtCell (0 ,0 ) "x"
    writeAtCell (0 ,30) "x"
    writeAtCell (90, 0) "x"
    writeAtCell (90,30) "x"

drawRooms :: [Room] -> IO()
drawRooms []           = putStr ""
drawRooms (room:rooms) = do
    let (x,y) = coordinates room
        v     = visited room

    writeAtCell (x+81, y+23)
        (if v
            then if room /= wall
                then ""
                else "█"
            else "░")

    drawRooms rooms

drawCurrentRoom :: GameState -> IO()
drawCurrentRoom gs = do
    let (x,y) = coordinates $ currentRoom $ world gs

    writeAtCell (x+81, y+23) "x"

drawMiniMap :: GameState -> IO()
drawMiniMap gs = do
    drawRooms $ getRooms gs
    drawCurrentRoom gs

drawStateText :: GameState -> IO()
drawStateText gs = do
    goto (29, 2)
    putStrLn $ text gs

drawInventory :: GameState -> IO()
drawInventory gs = do
    let inv = inventory $ player gs

    writeFromRightToLeft (50, 15) $ show inv

drawCharacterInfo :: GameState -> IO()
drawCharacterInfo gs = do
    let p = player gs

    writeMultiLineAt (2, 10) (show p)

drawScreen :: GameState -> IO ()
drawScreen gs = do
    clear

    let cs = currentScreen gs

    drawMiniMap gs
    drawBounds
    drawStateText gs
    drawChoises cs

    case cs of
        InventoryScreen -> do
            drawInventory gs
            drawCharacterInfo gs
        CharacterInfoScreen ->
            drawCharacterInfo gs
        _ -> putStr ""


    goto (31, 91)
