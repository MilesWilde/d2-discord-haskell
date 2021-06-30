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
import UnliftIO.Concurrent

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
        highestDiff Int
        currentDiff Int
        currentQuest Int
        waypoints Int
        hasCube Bool
        deriving Show Read

    Trip sql=trips
        tripUser UserId
        tripCharId CharacterId
        tChannel Int 
        start Int 
        duration Int 
        end Int
        zone T.Text
        difficulty Int
        tripLevel Int
        isActive Bool
        monsters [T.Text]
        amounts [Int]
        rarities [Int]
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
    GDExperience sql=experiences
        level Int 
        nextLevelAbs Int
        amountToNext Int
        deriving Show Read Eq

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

    MonLvl sql=monlvls
        ac Int
        th Int
        hp Int
        dam Int
        xp Int
        level Int
        difficulty Int
        deriving Show Read Eq

    MonDiffStat sql=mondiffstats
        level Int
        resDm Int
        resMa Int
        resFi Int
        resLi Int
        resCo Int
        resPo Int
        minHp Int
        maxHp Int
        ac Int
        exp Int
        difficulty Int
        textId T.Text
        treasureClasses [T.Text]
        deriving Show Read Eq

    MonStat sql=monstats
        minions [T.Text]
        name T.Text
        code T.Text
        type T.Text 
        partyMin Int
        partyMax Int
        grpMin Int
        grpMax Int
        rarity Int
        textId T.Text
        undead Bool
        demon Bool
        UniqueId textId
        Primary textId
        deriving Show Read Eq

    ZoneLvl sql=zonelvl
        normMons [T.Text]
        nmhMons [T.Text]
        uniqueMons [T.Text]
        critters [T.Text]
        critterSpwnPct [Int]
        monLvlsExp [Int]
        name T.Text
        zoneName T.Text
        quest T.Text
        questPart Int
        act Int
        teleport Int
        numMon Int
        waypoint Int
        town Bool 
        zoneId Int
        isInside Bool
        Primary zoneId
        deriving Show Read Eq

    ZoneLvlDiffVars sql=zonelvldiffvars
        zoneName T.Text
        zoneId Int
        sizeX Int
        sizeY Int
        monDen Int
        monUMin Int
        monUMax Int
        difficulty Int
        deriving Show Read Eq

    ZoneAdjacent sql=adjzones
        zone T.Text 
        adjacentZones [T.Text]
        Primary zone
        UniqueZone zone
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

data MonRarity = Common | Champion | UniqueMon deriving (Show, Eq, Enum)

type Amount = Int

data MonsterPackRarity = MonsterPackRarity
  { monPStat :: MonStat,
    monPAmount :: Amount,
    monPRarity :: MonRarity
  }
  deriving (Show, Eq)

mPRNames :: [MonsterPackRarity] -> [T.Text]
mPRNames [] = []
mPRNames (MonsterPackRarity mon _ _ : ms) = monStatTextId mon : mPRNames ms

mPRAmnts :: [MonsterPackRarity] -> [Int]
mPRAmnts [] = []
mPRAmnts (MonsterPackRarity _ amnt _ : ms) = amnt : mPRAmnts ms

mPRRarities :: [MonsterPackRarity] -> [Int]
mPRRarities [] = []
mPRRarities (MonsterPackRarity _ _ rarity : ms) = fromEnum rarity : mPRRarities ms

snowfToUserKey :: Integral a => a -> Key User
snowfToUserKey s = UserKey {unUserKey = fromIntegral s}