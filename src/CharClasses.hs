module CharClasses where

import Data.Text as T
import Discord.Types
import qualified TextShow.TH as TSG

data CharClass = Amazon | Assassin | Barbarian | Druid | Necromancer | Paladin | Sorceress deriving (Show, Eq, Enum)

showC :: CharClass -> T.Text
showC Amazon = "Amazon"
showC Assassin = "Assassin"
showC Barbarian = "Barbarian"
showC Druid = "Druid"
showC Necromancer = "Necromancer"
showC Paladin = "Paladin"
showC Sorceress = "Sorceress"

readC :: T.Text -> Maybe CharClass
readC t =
  case T.toLower t of
    "amazon" -> Just Amazon
    "assassin" -> Just Assassin
    "barbarian" -> Just Barbarian
    "druid" -> Just Druid
    "necromancer" -> Just Necromancer
    "paladin" -> Just Paladin
    "sorceress" -> Just Sorceress
    _ -> Nothing