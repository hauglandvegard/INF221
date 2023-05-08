module Game.Model.Item where

{-
A pure module describing what an item is and defines some basic items.
-}
data ItemCategory = Weapon | Armor | Utility | Consumable | NoItem  deriving (Enum, Show, Eq)

data Item = Item {
    shortName :: String,
    longName :: String,
    value :: Int,
    category :: ItemCategory
} deriving Eq

instance Show Item where
    show = shortName


basicSword = Item {
    shortName = "bSword",
    longName = "Basic sword of swordiness",
    value = 1,
    category = Weapon
}

basicArmor = Item {
    shortName = "bArmor",
    longName = "Basic armor of protectiveness",
    value = 1,
    category = Armor
}

noItem = Item {
    shortName = "-",
    longName = "nothing",
    value = -1,
    category = NoItem
}