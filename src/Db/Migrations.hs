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
import Commands
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Text as T
import Database.Persist (Entity (..))
import Database.Persist.Postgresql
import Database.Persist.Sql
import qualified Database.Persist.TH as PTH
import Db.DbHelpers
import System.Environment

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateMutable"]
  [PTH.persistLowerCase|
    Character sql=characters
        user UserId
        class Int
        name T.Text
        timePlayed Int
        experience Int
        level Int
        dex Int
        str Int
        vit Int
        nrg Int
        avlStats Int
        avlSkills Int
        deriving Show Read

    User sql=users
        currentCharId Int
        timeCreated Int
        discordId Int
        Primary discordId
        UniqueUser discordId
        deriving Show Read Eq
|]

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateImmutable"]
  [PTH.persistLowerCase|
    GDClasses sql=gdclasses
        class Int
        deriving Show Read Eq
|]

migrate :: IO ()
migrate = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        runMigrationUnsafe migrateMutable
        runMigrationUnsafe migrateImmutable
        liftIO $ print "whatever"
