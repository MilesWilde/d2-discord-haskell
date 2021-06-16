module Commands where

import CharClasses
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Discord.Types

data Command = Quest | Run | CreateCharacter CharClass CharName | SelectCharacter CharName | View ViewCommand (Maybe CharName) | Equip | Unequip deriving (Show, Eq)

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
          Just charClass ->
            case parseName commandArgs of
              Just charName -> Right $ CreateCharacter charClass charName
              Nothing -> Left "Name needs to be 20 characters or less"
          Nothing -> Left $ "Matched none of the available options: \\create " <> T.intercalate ", " (showC <$> [Amazon .. Sorceress]) <> " [Name]"
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
          Nothing -> Left $ "Matched none of the available options: \\view " <> T.intercalate ", " (showV <$> [Stats .. Characters]) <> " ([Name]  - optional)"
      | length commandArgs == 2 ->
        case parseView commandArgs of
          Just viewCommand -> case parseName commandArgs of
            Just name -> Right $ View viewCommand (Just name)
            Nothing -> Left "Name needs to be 20 characters or less"
          Nothing -> Left $ "Matched none of the available options: \\view " <> T.intercalate ", " (showV <$> [Stats .. Characters]) <> " ([Name]  - optional)"
      | otherwise -> Left $ "Wrong number of arguments. View usage: \\view [" <> T.intercalate ", " (showV <$> [Stats .. Characters]) <> "] ([Name] - optional)"
    "equip" -> Right Equip
    "unequip" -> Right Unequip
    _ -> Left $ "Matched none of the available commands. You said: " <> baseCommand
  where
    baseCommand = fst $ parseCommand m
    commandArgs = snd $ parseCommand m

parseView :: [T.Text] -> Maybe ViewCommand
parseView = readV . head

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