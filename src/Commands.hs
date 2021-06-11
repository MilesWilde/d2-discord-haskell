module Commands where

import qualified Data.Text as T

data Command = Quest | Run | CreateCharacter | SelectCharacter | ViewSkills | ViewStats | Equip | Unequip | Default deriving (Show, Eq)

getCommand :: T.Text -> Command
getCommand c = case T.toLower c of
  "quest" -> Quest
  "run" -> Run
  "create" -> CreateCharacter
  "select" -> SelectCharacter
  "viewskills" -> ViewSkills
  "viewstats" -> ViewStats
  "equip" -> Equip
  "unequip" -> Unequip
  _ -> Default
