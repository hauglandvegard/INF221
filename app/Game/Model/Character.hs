module Game.Model.Character where
{-
This module is all about the character and a few functions to help the interaction
with the inventory. Also a setup for a basic player and a basic enemy. 
-}
import Game.Model.Item ( basicArmor, basicSword, noItem, Item )
import Data.List (intercalate)
import Control.Monad.State
    ( State, execState, MonadState(put, get) )

type HealthPoints = Int

type Gold = Int

type Loot = [Item]

data Inventory = Inventory {
    over :: [Item],
    under :: [Item],
    selected :: Item
} deriving (Eq)

instance Show Inventory where
    show x = let o = over x
                 u = under x
                 s = selected x
             in
                intercalate "\n" [show item | item <- o]
                ++ "\n --> " ++ show s ++ "\n" ++
                intercalate "\n" [show item | item <- u]

-- Dirty State functions
nextItem' :: State Inventory ()
nextItem' = do
    inv <- get

    let s = selected inv
        o = over inv
        u = under inv

    let emptyInv = (s == noItem)
                && null o
                && null u

    if emptyInv || null u
        then put inv
        else do
            let newOver = o ++ [s]
                newSelected = head u
                newUnder = tail u

            put $ inv {over = newOver,
                       selected = newSelected,
                       under = newUnder}

previousItem' :: State Inventory ()
previousItem' = do
    inv <- get

    let s = selected inv
        o = over inv
        u = under inv

    let emptyInv = (s == noItem)
                && null o
                && null u

    if emptyInv || null o
        then put inv
        else do
            let newUnder = s : u
                newSelected = last o
                newOver = init o

            put $ inv {over = newOver,
                       selected = newSelected,
                       under = newUnder}

-- Pure non State functions
nextItem :: Inventory -> Inventory
nextItem = execState nextItem'

previousItem :: Inventory -> Inventory
previousItem = execState previousItem'


data Character
    = Player {
        name :: String,
        maxHp :: HealthPoints,
        curHp :: HealthPoints,
        inventory :: Inventory,
        gold :: Int,
        weapon :: Item,
        armor :: Item
        }
    | Enemy {
        name :: String,
        maxHp :: HealthPoints,
        curHp :: HealthPoints,
        loot :: Loot,
        weapon :: Item,
        armor :: Item
        } deriving Eq

instance Show Character where
    show x = "Name: "      ++ show (name x)      ++ "\n"
          ++ "HP: "        ++ show (curHp x)     ++ "/" ++ show (maxHp x) ++ "\n"
          ++ "Weapon: "    ++ show (weapon x)    ++ "\n"
          ++ "Armor: "     ++ show (armor x)     ++ "\n"
       -- ++ "Inventory: " ++ show (inventory x) ++ "\n"


basicPlayer :: Character
basicPlayer = Player {
    name = "Best Player",
    maxHp = 100,
    curHp = 100,
    inventory = Inventory {
        over = [basicSword, basicArmor],
        under = [basicSword, basicSword, basicArmor],
        selected = basicSword
    },
    gold = 100,
    weapon = basicSword,
    armor = basicArmor
}

basicEnemy :: Character
basicEnemy = Enemy {
    name = "Monter 1",
    maxHp = 100,
    curHp = 100,
    loot = [],
    weapon = basicSword,
    armor = basicArmor
}