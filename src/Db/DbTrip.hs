module Db.DbTrip where

import CharClasses
import Commands
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Logger (NoLoggingT, runStderrLoggingT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Database.Persist.Postgresql
import Db.DbCharacter
import Db.DbHelpers
import Db.DbMonstersZones
import Db.Migrations
import qualified Discord.Types as DT
import Stats
import System.Environment
import UnliftIO (liftIO, throwIO)

createTripCompleted :: Trip -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (DT.ChannelId, Either CmdError DbStatus)
createTripCompleted t = do
  mECharacter <- selectFirst [CharacterId ==. tripCharId t] []
  eitherCase <- case mECharacter of
    Nothing -> pure $ Left "The character on that trip no longer exists."
    Just eCharacter -> do
      let character = entityVal eCharacter
      mPRs <- getMPRaritiesFromLists (monsters t) (zip (amounts t) (toEnum <$> rarities t))
      eTripXps <- liftIO $ sequence $ getXpFromMonPackRar (tripLevel t) (difficulty t) <$> mPRs
      case sequence eTripXps of
        Left err -> pure $ Left err
        Right tripXps -> do
          let tripXp = sum tripXps
          mNewCharLvl <- liftIO $ getCharLvlFromExp (experience character + tripXp)
          case mNewCharLvl of
            Nothing -> pure $ Left "Max level"
            Just newCharLvl -> do
              if newCharLvl == level character
                then do
                  update (entityKey eCharacter) [Experience +=. tripXp]
                  pure $ Right $ name character <> " just finished " <> zone t <> ". You gained: " <> (T.pack . show) tripXp <> " experience."
                else do
                  let levelsGained = newCharLvl - level character
                  update (entityKey eCharacter) [Experience +=. tripXp, AvlStats +=. (5 * levelsGained), AvlSkills +=. levelsGained, Level =. newCharLvl]
                  pure $ Right $ name character <> " just finished " <> zone t <> ". You gained: " <> (T.pack . show) tripXp <> " experience. You gained a level and are now level " <> (T.pack . show) newCharLvl <> "."
  pure (chanId, eitherCase)
  where
    chanId = fromIntegral $ tChannel t :: DT.ChannelId

createTripMessage :: Trip -> IO (DT.ChannelId, Either CmdError DbStatus)
createTripMessage t = do
  loadFile defaultConfig
  connectionString <- liftIO connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ createTripCompleted t

insertTrip :: Trip -> IO (Key Trip)
insertTrip t = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        insert t

createTrip :: Maybe Int -> [T.Text] -> DT.MessageId -> DT.ChannelId -> IO (Either CmdError DbStatus)
createTrip mTripAmnt zoneWords userKey channelId = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        setupTrip mTripAmnt zoneWords userKey channelId

formTrip :: DT.MessageId -> Entity Character -> DT.ChannelId -> T.Text -> [MonsterPackRarity] -> ZoneLvl -> ZoneLvlDiffVars -> Int -> Int -> IO Trip
formTrip userKey char chanId zoneT monPRs zoneLvl zoneDiffs diff dur = do
  nowEpoch <- round `fmap` getPOSIXTime
  pure
    Trip
      { tripUser = snowfToUserKey userKey,
        tripCharId = charId,
        tChannel = fromIntegral chanId,
        start = nowEpoch,
        duration = dur,
        end = nowEpoch + dur,
        zone = zoneT,
        isActive = True,
        difficulty = diff,
        tripLevel = tLvl,
        monsters = mPRNames monPRs,
        amounts = mPRAmnts monPRs,
        rarities = mPRRarities monPRs
      }
  where
    tLvl = zoneLvlMonLvlsExp zoneLvl !! diff
    charId = entityKey char

setupTrip :: Maybe Int -> [T.Text] -> DT.MessageId -> DT.ChannelId -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Either CmdError DbStatus)
setupTrip mTripAmnt zoneWords userKey chanId = do
  mCharacter <- getUserCurrentCharacter $ fromIntegral userKey
  case mCharacter of
    Nothing -> pure $ Left "User doesn't have a character. Try \\create"
    Just eCharacter -> do
      let character = entityVal eCharacter
      let charCurrDiff = currentDiff character
      mZoneLvl <- liftIO $ getZoneLvl zoneText
      case mZoneLvl of
        Nothing -> pure $ Left "That zone doesn't exist."
        Just zoneLvl -> do
          if zoneLvlTown zoneLvl
            then pure $ Left "That zone is not runnable."
            else do
              mZoneDiffs <- liftIO $ getZoneDiff zoneText charCurrDiff
              case mZoneDiffs of
                Nothing -> pure $ Left $ "The zone " <> zoneLvlZoneName zoneLvl <> " does not have a ZoneLvl corresponding. Please report this error."
                Just zoneDiffs -> do
                  let zoneMonLvl
                        | charCurrDiff == 0 = (head . zoneLvlMonLvlsExp) zoneLvl
                        | charCurrDiff == 1 = (head . zoneLvlMonLvlsExp) zoneLvl
                        | charCurrDiff == 2 = (head . zoneLvlMonLvlsExp) zoneLvl
                        | otherwise = 0
                  eMonsterPacks <- liftIO $ getCommonPacks zoneLvl zoneDiffs
                  case eMonsterPacks of
                    Left err -> pure $ Left err
                    Right mprs -> do
                      let durText = calcTripDurText character zoneMonLvl mprs
                      let durTime = calcTripDur character zoneMonLvl mprs
                      trip <- liftIO $ formTrip userKey eCharacter chanId zoneText mprs zoneLvl zoneDiffs charCurrDiff durTime
                      keyTrip <- liftIO $ insertTrip trip
                      pure $ Right $ name character <> " is headed to " <> zoneLvlZoneName zoneLvl <> ". It will take approximately " <> durText <> "."
  where
    zoneText = upperFirst zoneWords

calcTripDurText :: Character -> Int -> [MonsterPackRarity] -> T.Text
calcTripDurText c mLvl ms = minutes <> "m " <> remSecs <> "s"
  where
    secs = sum $ calcPackDur c mLvl <$> ms
    minutes = (T.pack . show) (secs `div` 60)
    remSecs = (T.pack . show) (secs `mod` 60)

calcTripDur :: Character -> Int -> [MonsterPackRarity] -> Int
calcTripDur c mLvl ms = sum secs
  where
    secs = calcPackDur c mLvl <$> ms

-- this function will be very complicated, including items, level, stats, monster level, resistances, etc
calcPackDur :: Character -> Int -> MonsterPackRarity -> Int
calcPackDur c mLvl m = 1
  where
    -- calcPackDur c mLvl m = (1 + ((5 + mLvl) `div` cLvl)) * 5 * monPAmount m
    cLvl = level c

getFinishedTrips :: IO (Maybe [Entity Trip])
getFinishedTrips = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        nowEpoch <- liftIO $ round `fmap` getPOSIXTime
        queriedTrips <- selectList [IsActive ==. True, End <=. nowEpoch] []
        let finishedTrips
              | not (null queriedTrips) = pure $ Just queriedTrips
              | otherwise = pure Nothing
        finishedTrips

updateTripsComplete :: [Entity Trip] -> IO ()
updateTripsComplete ts = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        mapM_ updateTripComplete ts

updateTripComplete :: Entity Trip -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) ()
updateTripComplete t = do
  update (entityKey t) [IsActive =. False]