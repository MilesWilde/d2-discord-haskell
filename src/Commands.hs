module Commands where

import CharClasses
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Text.Read as TR
import Discord.Types

data Command = Quest | Run | CreateCharacter CharClass CharName | SelectCharacter CharName | View ViewCommand (Maybe CharName) | Equip | Unequip | AssignStats Int Stat | AssignSkills Int Skill deriving (Show, Eq)

data ViewCommand = Stats | Skills | Inventory | Characters deriving (Show, Eq, Enum)

data Stat = Strength | Dexterity | Intelligence | Vitality deriving (Show, Eq, Enum)

data Skill = Unimplemented deriving (Show, Eq)

readStat :: T.Text -> Maybe Stat
readStat t =
  case T.toLower t of
    "strength" -> Just Strength
    "dexterity" -> Just Dexterity
    "intelligence" -> Just Intelligence
    "vitality" -> Just Vitality
    _ -> Nothing

showStat :: Stat -> T.Text
showStat Strength = "Strength"
showStat Dexterity = "Dexterity"
showStat Intelligence = "Intelligence"
showStat Vitality = "Vitality"

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

type CmdError = T.Text

type CharName = T.Text

getCommand :: Message -> Either CmdError Command
getCommand m =
  case baseCommand of
    "quest" -> Right Quest
    "run" -> Right Run
    "create"
      | length commandArgs == 2 ->
        case parseClass commandArgs of
          Nothing -> Left $ "Matched none of the available options: \\create (" <> charactersText <> ") [Name]"
          Just charClass ->
            case parseName commandArgs of
              Just charName -> Right $ CreateCharacter charClass charName
              Nothing -> Left "Name needs to be 20 characters or less"
      | otherwise -> Left "Wrong number of arguments. CreateCharacter usage: \\create [Class] [CharacterName]"
    "select"
      | length commandArgs == 1 ->
        case parseSelectName commandArgs of
          Just charName -> Right $ SelectCharacter charName
          Nothing -> Left "Name needs to be 20 characters or less"
      | otherwise -> Left "Wrong number of arguments. SelectCharacter usage: \\select [CharacterName]"
    "view"
      | length commandArgs == 1 ->
        case parseView commandArgs of
          Just viewCommand -> Right $ View viewCommand Nothing
          Nothing -> Left $ "Matched none of the available options: \\view (" <> viewCText <> ") ([Name]  - optional)"
      | length commandArgs == 2 ->
        case parseView commandArgs of
          Nothing -> Left $ "Matched none of the available options: \\view (" <> viewCText <> ") ([Name]  - optional)"
          Just viewCommand -> case parseName commandArgs of
            Just name -> Right $ View viewCommand (Just name)
            Nothing -> Left "Name needs to be 20 characters or less"
      | otherwise -> Left $ "Wrong number of arguments. View usage: \\view (" <> viewCText <> ") ([Name] - optional)"
    "equip" -> Right Equip
    "unequip" -> Right Unequip
    "assignstats"
      | length commandArgs == 2 ->
        case parseInitAmount commandArgs of
          Nothing -> Left $ "Amount must be an integer. Usage: \\assignstats (amount) (" <> statText <> ")"
          Just amount ->
            case parseStat commandArgs of
              Nothing -> Left $ "Stat must be one of the available options. Usage: \\assignstats (amount) (" <> statText <> ")"
              Just stat -> Right $ AssignStats amount stat
      | otherwise -> Left $ "Wrong number of arguments. Usage: \\assignstats (amount) (" <> statText <> ")"
    _ -> Left $ "Matched none of the available commands. You said: " <> baseCommand
  where
    baseCommand = fst $ parseCommand m
    commandArgs = snd $ parseCommand m
    viewCText = T.intercalate ", " (showV <$> [Stats .. Characters])
    charactersText = T.intercalate ", " (showC <$> [Amazon .. Sorceress])
    statText = T.intercalate ", " (showStat <$> [Strength .. Vitality])

parseView :: [T.Text] -> Maybe ViewCommand
parseView = readV . head

parseStat :: [T.Text] -> Maybe Stat
parseStat ts = readStat (ts !! 1)

parseCommand :: Message -> (T.Text, [T.Text])
parseCommand m = (baseCommand, commandArgs)
  where
    baseCommand = T.tail $ T.toLower $ head cmdList
    commandArgs = tail cmdList
    cmdList = (T.words . messageText) m

parseClass :: [T.Text] -> Maybe CharClass
parseClass = readC . head

parseName :: [T.Text] -> Maybe T.Text
parseName ts
  | T.length name <= 20 = Just name
  | otherwise = Nothing
  where
    name = ts !! 1

parseSelectName :: [T.Text] -> Maybe T.Text
parseSelectName ts
  | T.length name <= 20 = Just name
  | otherwise = Nothing
  where
    name = head ts

parseInitAmount :: [T.Text] -> Maybe Int
parseInitAmount ts =
  case decimalInit of
    Left err -> Nothing
    Right tuple
      | fst tuple >= 0 && snd tuple == "" -> Just $ fst tuple
      | otherwise -> Nothing
  where
    decimalInit = TR.decimal $ head ts