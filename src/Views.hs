module Views where

import qualified Data.Text as T

data ViewCommand = Stats | Skills | Inventory | Characters deriving (Show, Eq, Enum)

readV :: T.Text -> Maybe ViewCommand
readV t =
  case T.toLower t of
    "stats" -> Just Stats
    "skills" -> Just Skills
    "inventory" -> Just Inventory
    "characters" -> Just Characters
    _ -> Nothing

showV :: ViewCommand -> T.Text
showV Stats = "Stats"
showV Skills = "Skills"
showV Inventory = "Inventory"
showV Characters = "Characters"

parseView :: [T.Text] -> Maybe ViewCommand
parseView = readV . head