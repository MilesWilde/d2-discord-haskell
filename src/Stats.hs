module Stats where

import qualified Data.Text as T

data Stat = Strength | Dexterity | Vitality | Energy deriving (Show, Eq, Enum)

readStat :: T.Text -> Maybe Stat
readStat t =
  case T.toLower t of
    "strength" -> Just Strength
    "dexterity" -> Just Dexterity
    "energy" -> Just Energy
    "vitality" -> Just Vitality
    _ -> Nothing

showStat :: Stat -> T.Text
showStat Strength = "Strength"
showStat Dexterity = "Dexterity"
showStat Energy = "Energy"
showStat Vitality = "Vitality"

parseStat :: [T.Text] -> Maybe Stat
parseStat ts = readStat (ts !! 1)
