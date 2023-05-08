module Tools.Grid where

{-
This is just a file where I have made a very generic Grid that can hold any value
-}

import Control.Monad.State
import Data.Array

data Direction = North | East | South | West deriving (Enum, Show, Eq)

type Coordinate = Int

type Pos = (Coordinate, Coordinate)

getPos :: Pos -> Direction -> Pos
getPos (x,y) North = (x  ,y-1)
getPos (x,y) South = (x  ,y+1)
getPos (x,y) West  = (x-1,y  )
getPos (x,y) East  = (x+1,y  )

data Grid value = Grid {
    defaultValue :: value,
    cells :: Array Pos value,
    gridFlag :: Int
    }

showCells :: (Show value) => [(Pos, value)] -> String
showCells [] = ""
showCells ((pos, val):xs) = "(" ++ show pos ++ ", " ++ show val ++ ")" ++ "\n" ++ showCells xs

instance (Show a) => Show (Grid a) where
    show x = let c = cells x
                 b = snd $ bounds c
                 f = gridFlag x
             in "Outer Bounds " ++ show (fst b)
             ++ " | " ++ show (snd b) ++ "\n"
             ++ showCells (assocs c)
             ++ if f /= 0 then show f else ""

makeGrid :: Int -> Int -> value -> Grid value
makeGrid x y base = Grid {
    defaultValue = base,
    gridFlag = 0,
    cells = array ((0,0), (x-1, y-1)) [((a,b), base) | a <- [0..x-1], b <- [0..y-1]]
    }

onGrid :: (Pos,Pos) -> Pos -> Bool
onGrid ((xLower, yLower), (xUpper, yUpper)) (x, y) =
    (x <= xUpper) &&
    (x >= xLower) &&
    (y <= yUpper) &&
    (y >= yLower)

getCell :: Grid value -> Pos -> Maybe value
getCell (Grid {cells = cells}) pos
    | not $ onGrid (bounds cells) pos = Nothing
    | otherwise = Just $ cells ! pos

updateCell' :: Pos -> value -> State (Grid value) ()
updateCell' pos val = do
    grid <- get
    let possible = onGrid (bounds $ cells grid) pos
    let newCells = cells grid // [(pos, val)]
    if possible
        then put $ grid { cells = newCells }
        else put $ grid { gridFlag = 1 }

updateCell :: Grid value -> Pos -> value -> Grid value
updateCell grid pos val = execState (updateCell' pos val) grid


updateCells :: Grid value -> [(Pos, value)] -> Grid value
updateCells grid [] = grid
updateCells grid ((pos, val):xs) =
    updateCells (updateCell grid pos val) xs
