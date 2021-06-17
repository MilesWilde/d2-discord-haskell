{-# LANGUAGE FlexibleContexts #-}

module Db.DbCommands where

import CharClasses
import Commands
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Logger (runStderrLoggingT)
import Data.ByteString.Internal as BLU
import Data.Int
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Word
import Database.Persist (Entity (..))
import Database.Persist.Postgresql
import Database.Persist.Sql
import qualified Database.Persist.TH as PTH
import Db.DbHelpers
import Db.Migrations
import qualified Discord.Types as DT
import System.Environment
import TextShow
import UnliftIO (liftIO)

type MsgUserId = DT.Snowflake

type DbStatus = T.Text

createCharacter :: CharClass -> CharName -> Key User -> IO (Either CmdError DbStatus)
createCharacter cClass cName userKey = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        validatedCharName <- liftIO $ validateCharCreation cName userKey
        case validatedCharName of
          Right _ ->
            case cClass of
              Sorceress -> do
                character <-
                  insert $ createSorceress userKey cName
                user <- update userKey [UserCurrentCharId =. (fromIntegral . fromSqlKey) character]
                pure $ Right $ showC cClass <> " created with name " <> cName
              Barbarian -> do
                character <-
                  insert $ createBarbarian userKey cName
                user <- update userKey [UserCurrentCharId =. (fromIntegral . fromSqlKey) character]
                pure $ Right $ showC cClass <> " created with name " <> cName
              Amazon -> do
                character <-
                  insert $ createAmazon userKey cName
                user <- update userKey [UserCurrentCharId =. (fromIntegral . fromSqlKey) character]
                pure $ Right $ showC cClass <> " created with name " <> cName
              Necromancer -> do
                character <-
                  insert $ createNecromancer userKey cName
                user <- update userKey [UserCurrentCharId =. (fromIntegral . fromSqlKey) character]
                pure $ Right $ showC cClass <> " created with name " <> cName
              Paladin -> do
                character <-
                  insert $ createPaladin userKey cName
                user <- update userKey [UserCurrentCharId =. (fromIntegral . fromSqlKey) character]
                pure $ Right $ showC cClass <> " created with name " <> cName
              Druid -> do
                character <-
                  insert $ createDruid userKey cName
                user <- update userKey [UserCurrentCharId =. (fromIntegral . fromSqlKey) character]
                pure $ Right $ showC cClass <> " created with name " <> cName
              Assassin -> do
                character <-
                  insert $ createAssassin userKey cName
                user <- update userKey [UserCurrentCharId =. (fromIntegral . fromSqlKey) character]
                pure $ Right $ showC cClass <> " created with name " <> cName
          Left error -> pure $ Left error

selectCharacter :: CharName -> Key User -> IO (Either CmdError DbStatus)
selectCharacter cName userKey = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        character <- selectFirst [CharacterUser ==. userKey, CharacterName ==. cName] []
        case character of
          Just character -> do
            let characterId = (fromIntegral . fromSqlKey . entityKey) character
            update userKey [UserCurrentCharId =. characterId]
            pure $ Right $ cName <> " will now be used for commands."
          Nothing -> pure $ Left $ "You do not have a character with name " <> cName <> ". Try \\viewcharacters to see your available characters."

viewCharactersNames :: Key User -> IO (Either CmdError DbStatus)
viewCharactersNames userKey = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        dbCharacters <- selectList [CharacterUser ==. userKey] []
        let names
              | not (null dbCharacters) = pure $ Right $ viewCharactersText dbCharacters
              | otherwise = pure $ Left "It looks like you have no characters. Try creating one using \\create [Class] [Name]"
        names

viewCharactersText :: [Entity Character] -> T.Text
viewCharactersText [] = ""
viewCharactersText (e : es) = name <> ": Level " <> level <> " " <> charClass <> "\n" <> viewCharactersText es
  where
    character = entityVal e
    charClass = (showC . toEnum . characterClass) character
    name = characterName character
    level = (T.pack . show . characterLevel) character

validateCharCreation :: CharName -> Key User -> IO (Either CmdError ())
validateCharCreation cName muid = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        character <- selectList [CharacterUser ==. muid, CharacterName ==. cName] []
        let validated
              | not (null character) = Left ("Can't create character. You have a character named " <> cName <> " already. You can \\delete them to reuse the name.")
              | otherwise = Right ()
        pure validated

createUser :: DT.Snowflake -> IO ()
createUser muid = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        nowEpoch <- liftIO $ round `fmap` getPOSIXTime
        insertUnique
          User
            { userCurrentCharId = 0,
              userTimeCreated = nowEpoch,
              userDiscordId = fromIntegral muid
            }
      pure ()

snowflakeToKey :: Integral a => a -> Key User
snowflakeToKey s = UserKey {unUserKey = fromIntegral s}

createSorceress :: Key User -> CharName -> Character
createSorceress userMuid cName =
  Character
    { characterUser = userMuid,
      characterClass = fromEnum Sorceress,
      characterName = cName,
      characterTimePlayed = 0,
      characterExperience = 0,
      characterLevel = 1,
      characterStr = 10,
      characterDex = 25,
      characterInt = 35,
      characterVit = 10,
      characterAvlStats = 0,
      characterAvlSkills = 0
    }

createBarbarian :: Key User -> CharName -> Character
createBarbarian userMuid cName =
  Character
    { characterUser = userMuid,
      characterClass = fromEnum Barbarian,
      characterName = cName,
      characterTimePlayed = 0,
      characterExperience = 0,
      characterLevel = 1,
      characterStr = 30,
      characterDex = 20,
      characterInt = 10,
      characterVit = 25,
      characterAvlStats = 0,
      characterAvlSkills = 0
    }

createPaladin :: Key User -> CharName -> Character
createPaladin userMuid cName =
  Character
    { characterUser = userMuid,
      characterClass = fromEnum Paladin,
      characterName = cName,
      characterTimePlayed = 0,
      characterExperience = 0,
      characterLevel = 1,
      characterStr = 25,
      characterDex = 20,
      characterInt = 15,
      characterVit = 25,
      characterAvlStats = 0,
      characterAvlSkills = 0
    }

createNecromancer :: Key User -> CharName -> Character
createNecromancer userMuid cName =
  Character
    { characterUser = userMuid,
      characterClass = fromEnum Necromancer,
      characterName = cName,
      characterTimePlayed = 0,
      characterExperience = 0,
      characterLevel = 1,
      characterStr = 15,
      characterDex = 25,
      characterInt = 25,
      characterVit = 15,
      characterAvlStats = 0,
      characterAvlSkills = 0
    }

createAmazon :: Key User -> CharName -> Character
createAmazon userMuid cName =
  Character
    { characterUser = userMuid,
      characterClass = fromEnum Amazon,
      characterName = cName,
      characterTimePlayed = 0,
      characterExperience = 0,
      characterLevel = 1,
      characterStr = 15,
      characterDex = 25,
      characterInt = 25,
      characterVit = 15,
      characterAvlStats = 0,
      characterAvlSkills = 0
    }

createDruid :: Key User -> CharName -> Character
createDruid userMuid cName =
  Character
    { characterUser = userMuid,
      characterClass = fromEnum Druid,
      characterName = cName,
      characterTimePlayed = 0,
      characterExperience = 0,
      characterLevel = 1,
      characterStr = 15,
      characterDex = 20,
      characterInt = 20,
      characterVit = 25,
      characterAvlStats = 0,
      characterAvlSkills = 0
    }

createAssassin :: Key User -> CharName -> Character
createAssassin userMuid cName =
  Character
    { characterUser = userMuid,
      characterClass = fromEnum Assassin,
      characterName = cName,
      characterTimePlayed = 0,
      characterExperience = 0,
      characterLevel = 1,
      characterStr = 20,
      characterDex = 20,
      characterInt = 25,
      characterVit = 20,
      characterAvlStats = 0,
      characterAvlSkills = 0
    }
