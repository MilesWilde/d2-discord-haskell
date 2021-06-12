{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Db.Migrations where

import CharClasses
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString.Internal as BLU
import Database.Persist (Entity (..))
import Database.Persist.Postgresql
import Database.Persist.Sql
import qualified Database.Persist.TH as PTH
import System.Environment

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
    User sql=users
        currentCharId Int
        timeCreated Int
        discordId Int
        Primary discordId
        deriving Show Read

    Character sql=characters
        user UserId
        charClass Int
        name String
        timePlayed Int
        experience Int
        level Int
        dex Int
        str Int
        vit Int
        int Int
         
        deriving Show Read
|]

-- connString = pure "host=ip port=port user=whatever dbname=whatever password=pass"
connString :: IO ConnectionString
connString = stringToConn <$> getEnv "DB_CONNECTION_STRING"

stringToConn :: String -> ConnectionString
stringToConn = BLU.packChars

migrate :: IO ()
migrate = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        runMigrationUnsafe migrateAll

      -- johnId <- insert $ User 123 321
      -- janeId <- insert $ User 321 123

      liftIO $ print "whatever"