module Commands where

import CharClasses
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Discord.Types

data Command = Quest | Run | CreateCharacter CharClass CharName | SelectCharacter | ViewSkills | ViewStats | Equip | Unequip deriving (Show, Eq)

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
    "select" -> Right SelectCharacter
    "viewskills" -> Right ViewSkills
    "viewstats" -> Right ViewStats
    "equip" -> Right Equip
    "unequip" -> Right Unequip
    _ -> Left $ "Matched none of the available commands. You said: " <> baseCommand
  where
    baseCommand = fst $ parseCommand m
    commandArgs = snd $ parseCommand m

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