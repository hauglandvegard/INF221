module Game.View.Formatting where

{-
This module is responsable for that everything is being written to the screen at 
the right spot. This is relaying heavly upon escape codes that are working 
(to some degree) in Windows.
-}

-- Moves cursor to the coordinates
goto :: (Int,Int) -> IO ()
goto (x,y) = putStr("\ESC["++show x++";"++show y++"H") -- Moves cursor to (x,y)


-- Allows user to write at the given terminal coordinates
writeAt :: (Int, Int) -> String -> IO ()
writeAt (x,y) text = do
    goto (x,y)
    putStr text

-- Allows user to write at a specific cell in the board
writeAtCell :: (Int, Int) -> String -> IO ()
writeAtCell (x,y) text = do
    putStr "\ESC[s" -- saves cursor location
    writeAt (y,x) text
    putStr "\ESC[u" -- restores cursor location

writeFromRightToLeft'' :: (Int, Int) -> [Char] -> IO()
writeFromRightToLeft'' _ [] = putStr ""
writeFromRightToLeft'' (x,y) (c:cs) = do
    writeAtCell (x,y) [c]
    writeFromRightToLeft'' (x-1, y) cs  

writeFromRightToLeft' :: (Int, Int) -> [String] -> IO ()
writeFromRightToLeft' _ [] = putStr ""
writeFromRightToLeft' (x,y) (line:ls) = do
    writeFromRightToLeft'' (x,y) (reverse line)
    writeFromRightToLeft' (x, y-1) ls


writeFromRightToLeft :: (Int, Int) -> String -> IO ()
writeFromRightToLeft (x,y) str = writeFromRightToLeft' (x,y) (reverse $ lines str) 

-- Allows user to write multiple lines with the same distance from left side
writeMultiLineAt' :: (Int, Int) -> [String] -> IO()
writeMultiLineAt' _ [] = putStr ""
writeMultiLineAt' (x,y) (line:ls) = do
    writeAtCell (x,y) line
    writeMultiLineAt' (x, y-1) ls

writeMultiLineAt :: (Int, Int) -> String -> IO()
writeMultiLineAt (x,y) str = writeMultiLineAt' (x,y) (reverse $ lines str)

clear :: IO()
clear = do 
    putStr "\ESC[2J" -- clears screen
    putStr "\ESC[H" -- moves cursor to upper left corner

-- -----------------------------------------------------------------
-- Code that might be used later
-- -----------------------------------------------------------------

-- Allows program to write bellow the curser, then restores the cursor position
writeBellow :: Int -> String -> IO ()
writeBellow y text = do
    putStr "\ESC[s" -- saves cursor location
    writeAt (0,0) text
    putStr "\ESC[u" -- moves cursor to last saved location
    
clearDown :: IO()
clearDown = putStr "\ESC[0J" -- Clears everything bellow cursor

clearLine :: IO()
clearLine = putStr "\ESC[K" -- Clears line that curser is at

-- Waits for the user to press enter, then breaks the loop and retunrs
waitForEnter :: IO()
waitForEnter = do
    putStr "\ESC[s" -- saves location of cursor
    clearLine
    putStr "> "
    command <- getLine
    case command of
        "" -> return()
        _  -> do putStr "\ESC[u"; waitForEnter -- moves cursor to last saved location
