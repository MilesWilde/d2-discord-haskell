{-# LANGUAGE FlexibleContexts #-}

module Db.DbCommands where

import CharClasses
import Commands
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Logger (runStderrLoggingT)
import Data.ByteString.Internal as BLU
import Data.Int
import qualified Data.List as L
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

createCharacter :: CharClass -> CharName -> MsgUserId -> IO (Either CmdError DbStatus)
createCharacter cClass cName muid = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        case cClass of
          Sorceress -> do
            character <-
              insert $
                Character
                  { characterUser = snowflakeToKey muid,
                    characterClass = fromEnum Sorceress,
                    characterName = T.unpack cName,
                    characterTimePlayed = 0,
                    characterExperience = 0,
                    characterLevel = 1,
                    characterStr = 10,
                    characterDex = 25,
                    characterInt = 35,
                    characterVit = 10
                  }
            pure $ Right $ showC cClass <> " created with name " <> cName
          _ -> do
            pure $ Left $ "Matched none of the available options: \\create " <> T.intercalate ", " (showC <$> [Amazon .. Sorceress])

createUser :: MsgUserId -> IO (Either CmdError DbStatus)
createUser muid = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        nowEpoch <- liftIO $ round `fmap` getPOSIXTime
        res <-
          insertUnique
            User
              { userCurrentCharId = 0,
                userTimeCreated = nowEpoch,
                userDiscordId = fromIntegral muid
              }
        pure $ Right $ showC cClass <> " created with name " <> cName

snowflakeToKey :: Integral a => a -> Key Db.Migrations.User
snowflakeToKey s = UserKey {unUserKey = fromIntegral s}
