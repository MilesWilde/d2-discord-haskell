{-# LANGUAGE FlexibleContexts #-}

module Db.DbCommands where

import CharClasses
import Commands
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Logger (NoLoggingT, runStderrLoggingT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
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
import Stats
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
          Left error -> pure $ Left error
          Right _ -> do
            mCreateClass <- liftIO $ createClass cClass cName userKey
            case mCreateClass of
              Nothing -> liftIO $ pure $ Left "Unable to create class, this would happen if seeds aren't run"
              Just characterToInsert -> do
                character <- insert characterToInsert
                user <- update userKey [CurrentCharId =. (fromIntegral . fromSqlKey) character]
                pure $ Right $ showC cClass <> " created with name " <> cName

selectCharacter :: CharName -> Key User -> IO (Either CmdError DbStatus)
selectCharacter cName userKey = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        character <- selectFirst [CUser ==. userKey, Name ==. cName] []
        case character of
          Just character -> do
            let characterId = (fromIntegral . fromSqlKey . entityKey) character
            update userKey [CurrentCharId =. characterId]
            pure $ Right $ cName <> " will now be used for commands."
          Nothing -> pure $ Left $ "You do not have a character with name " <> cName <> ". Try \\viewcharacters to see your available characters."

viewCharacterNames :: Key User -> IO (Either CmdError DbStatus)
viewCharacterNames userKey = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        dbCharacters <- selectList [CUser ==. userKey] []
        let names
              | not (null dbCharacters) = pure $ Right $ viewCharactersText dbCharacters
              | otherwise = pure $ Left "It looks like you have no characters. Try creating one using \\create [Class] [Name]"
        names

viewCharactersText :: [Entity Character] -> T.Text
viewCharactersText [] = ""
viewCharactersText (e : es) = cName <> ": Level " <> cLevel <> " " <> cClass <> "\n" <> viewCharactersText es
  where
    character = entityVal e
    cClass = (showC . toEnum . charClass) character
    cName = name character
    cLevel = (T.pack . show . level) character

viewStats :: DT.Snowflake -> IO (Either CmdError DbStatus)
viewStats muid = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        mCharacter <- getUserCurrentCharacter $ fromIntegral muid
        case mCharacter of
          Nothing -> pure $ Left "User has no current character. Try creating one with \\create."
          Just character -> pure $ Right $ viewStatsText character

viewStatsText :: Entity Character -> T.Text
viewStatsText e = levelLine <> strLine <> dexLine <> vitLine <> nrgLine <> lifeLine <> manaLine <> avlStatsLine
  where
    character = entityVal e
    cClass = (showC . toEnum . charClass) character
    cName = name character
    cLevel = (T.pack . show . level) character
    levelLine = cName <> ": Level " <> cLevel <> " " <> cClass <> "\n"
    strLine = "Strength: " <> (T.pack . show . str) character <> "\n"
    dexLine = "Dexterity: " <> (T.pack . show . dex) character <> "\n"
    vitLine = "Vitality: " <> (T.pack . show . vit) character <> "\n"
    nrgLine = "Energy: " <> (T.pack . show . nrg) character <> "\n"
    lifeLine = "Life: " <> (T.pack . show) (getLife character) <> "\n"
    manaLine = "Mana: " <> (T.pack . show) (getMana character) <> "\n"
    avlStatsLine = "Available points to spend: " <> (T.pack . show . avlStats) character <> "\n"

validateCharCreation :: CharName -> Key User -> IO (Either CmdError ())
validateCharCreation cName userKey = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        character <- selectList [CUser ==. userKey, Name ==. cName] []
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
            { currentCharId = 0,
              timeCreated = nowEpoch,
              discordId = fromIntegral muid
            }
      pure ()

assignStats :: Int -> Stat -> DT.Snowflake -> IO (Either CmdError DbStatus)
assignStats pts stat userKey = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        mCharacter <- (getUserCurrentCharacter . fromIntegral) userKey
        case mCharacter of
          Nothing -> pure $ Left "User has no current character. Try creating one with \\create."
          Just character -> do
            let charAvlStats = (avlStats . entityVal) character
            if charAvlStats < pts
              then pure $ Left $ "You do not have enough available stat points to allocate. You currently have " <> (T.pack . show) charAvlStats <> " points to spend."
              else case stat of
                Strength -> do
                  update (entityKey character) [Str +=. pts, AvlStats -=. pts]
                  pure $ Right $ "Your " <> showStat stat <> " is now " <> (T.pack . show) ((str . entityVal) character + pts) <> ". You now have " <> (T.pack . show) (charAvlStats - pts) <> " points to spend."
                Dexterity -> do
                  update (entityKey character) [Dex +=. pts, AvlStats -=. pts]
                  pure $ Right $ "Your " <> showStat stat <> " is now " <> (T.pack . show) ((dex . entityVal) character + pts) <> ". You now have " <> (T.pack . show) (charAvlStats - pts) <> " points to spend."
                Vitality -> do
                  update (entityKey character) [Vit +=. pts, AvlStats -=. pts]
                  pure $ Right $ "Your " <> showStat stat <> " is now " <> (T.pack . show) ((vit . entityVal) character + pts) <> ". You now have " <> (T.pack . show) (charAvlStats - pts) <> " points to spend."
                Energy -> do
                  update (entityKey character) [Nrg +=. pts, AvlStats -=. pts]
                  pure $ Right $ "Your " <> showStat stat <> " is now " <> (T.pack . show) ((nrg . entityVal) character + pts) <> ". You now have " <> (T.pack . show) (charAvlStats - pts) <> " points to spend."

getUserCurrentCharacter :: Int -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe (Entity Character))
getUserCurrentCharacter userKeyInt = do
  mUser <- selectFirst [DiscordId ==. userKeyInt] []
  case mUser of
    Nothing -> pure Nothing
    Just user -> do
      let userCurCharId = (currentCharId . entityVal) user
      selectFirst [CharacterId ==. (toSqlKey . fromIntegral) userCurCharId] []

snowflakeToKey :: Integral a => a -> Key User
snowflakeToKey s = UserKey {unUserKey = fromIntegral s}

createClass :: CharClass -> CharName -> Key User -> IO (Maybe Character)
createClass cClass cName userKey = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        mgdClassE <- selectFirst [GDClassClass ==. showC cClass] []
        case mgdClassE of
          Nothing -> pure Nothing
          Just gdClassE -> do
            let gdClass = entityVal gdClassE
            pure $
              Just
                Character
                  { cUser = userKey,
                    charClass = fromEnum cClass,
                    name = cName,
                    timePlayed = 0,
                    experience = 0,
                    level = 1,
                    dex = gDClassDex gdClass,
                    str = gDClassStr gdClass,
                    vit = gDClassVit gdClass,
                    baseVit = gDClassVit gdClass,
                    baseNrg = gDClassNrg gdClass,
                    nrg = gDClassNrg gdClass,
                    avlStats = 0,
                    avlSkills = 0,
                    lifePl = gDClassLifePl gdClass,
                    manaPl = gDClassManaPl gdClass,
                    lifePVit = gDClassLifePVit gdClass,
                    manaPNrg = gDClassManaPNrg gdClass
                  }
