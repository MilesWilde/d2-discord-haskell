{-# LANGUAGE FlexibleContexts #-}

module Db.DbCommands where

import CharClasses
import Commands
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Logger (NoLoggingT, runStderrLoggingT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Int
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as DS
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Database.Persist.Postgresql
import Db.DbCharacter
import Db.DbHelpers
import Db.DbMonstersZones
import Db.DbTrip
import Db.Migrations
import qualified Discord.Types as DT
import System.Environment
import UnliftIO (liftIO, throwIO)

type MsgUserId = DT.Snowflake

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