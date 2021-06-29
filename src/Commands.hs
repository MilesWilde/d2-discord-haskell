module Commands where

import CharClasses
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Text.Read as TR
import Discord.Types
import Stats
import Views

data Command = Quest | Run (Maybe Int) [T.Text] | CreateCharacter CharClass CharName | SelectCharacter CharName | View ViewCommand (Maybe CharName) | Equip | Unequip | AssignStats Int Stat | AssignSkills Int Skill | Cancel deriving (Show, Eq)

data Skill = Unimplemented deriving (Show, Eq)

type CmdError = T.Text

type CharName = T.Text

getCommand :: Message -> Either CmdError Command
getCommand m =
  case baseCommand of
    "quest" -> Right Quest
    "run"
      | length commandArgs >= 2 ->
        case parseInitAmount commandArgs of
          Nothing -> Right $ Run Nothing commandArgs
          Just amount -> Right $ Run (Just amount) commandArgs
      | otherwise -> Left "Wrong number of arguments. Run usage: \\run ([Amount] - optional) [AreaName]"
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
          Nothing -> Left $ "Amount must be an integer greater than zero. Usage: \\assignstats (amount) (" <> statText <> ")"
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
    statText = T.intercalate ", " (showStat <$> [Strength .. Energy])

parseCommand :: Message -> (T.Text, [T.Text])
parseCommand m = (baseCommand, commandArgs)
  where
    baseCommand = T.tail $ T.toLower $ head cmdList
    commandArgs = tail cmdList
    cmdList = (T.words . messageText) m

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
      | fst tuple > 0 && snd tuple == "" -> Just $ fst tuple
      | otherwise -> Nothing
  where
    decimalInit = TR.decimal $ head ts