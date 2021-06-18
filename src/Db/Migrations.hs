{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  [PTH.mkPersist PTH.sqlSettings {PTH.mpsPrefixFields = False}, PTH.mkMigrate "migrateMutable"]
  [PTH.persistLowerCase|
    Character sql=characters
        cUser UserId
        charClass Int
        name T.Text
        timePlayed Int
        experience Int
        level Int
        dex Int
        str Int
        vit Int
        nrg Int
        baseVit Int
        baseNrg Int
        avlStats Int
        avlSkills Int
        lifePl Int
        manaPl Int
        lifePVit Int
        manaPNrg Int
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
    GDClass sql=gdclasses
        class T.Text
        dex Int
        str Int
        vit Int
        nrg Int
        hpadd Int
        lifePl Int
        manaPl Int
        lifePVit Int
        manaPNrg Int
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

-- separate eventually into characters.hs/users,etc, if too many functions start existing
getLife :: Character -> Int
getLife c = floor (baseHp + cBaseVit + lifeFromVit + lifeFromLevels)
  where
    baseHp = (fromIntegral :: Int -> Double) 30
    cBaseVit = (fromIntegral . baseVit) c
    lifeFromVit = ((fromIntegral . lifePVit) c / 4) * ((fromIntegral . vit) c - (fromIntegral . baseVit) c)
    lifeFromLevels = ((fromIntegral . lifePl) c / 4) * ((fromIntegral . level) c - 1)

getMana :: Character -> Int
getMana c = floor (cBaseNrg + manaFromNrg + manaFromLevels)
  where
    cBaseNrg = (fromIntegral . baseNrg) c
    manaFromNrg = ((fromIntegral . manaPNrg) c / 4) * ((fromIntegral . nrg) c - (fromIntegral . baseNrg) c)
    manaFromLevels = ((fromIntegral . manaPl) c / 4) * ((fromIntegral . level) c - 1)