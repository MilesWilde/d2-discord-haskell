{-# LANGUAGE FlexibleContexts #-}

module Db.DbCommands where

import CharClasses
import Commands
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Logger (NoLoggingT, runStderrLoggingT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.ByteString.Internal as BLU
import qualified Data.Foldable as DF
import Data.Int
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as DS
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Word
import Database.Persist (Entity (..))
import Database.Persist.Postgresql
import Database.Persist.Sql
import qualified Database.Persist.TH as PTH
import Db.DbHelpers
import Db.Migrations
import qualified Discord as D
import qualified Discord.Requests as R
import qualified Discord.Types as DT
import Stats
import System.Environment
import System.Random
import TextShow
import UnliftIO (liftIO, throwIO)

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
        mECharacter <- (getUserCurrentCharacter . fromIntegral) userKey
        case mECharacter of
          Nothing -> pure $ Left "User has no current character. Try creating one with \\create."
          Just eCharacter -> do
            let charAvlStats = (avlStats . entityVal) eCharacter
            if charAvlStats < pts
              then pure $ Left $ "You do not have enough available stat points to allocate. You currently have " <> (T.pack . show) charAvlStats <> " points to spend."
              else case stat of
                Strength -> do
                  update (entityKey eCharacter) [Str +=. pts, AvlStats -=. pts]
                  pure $ Right $ "Your " <> showStat stat <> " is now " <> (T.pack . show) ((str . entityVal) eCharacter + pts) <> ". You now have " <> (T.pack . show) (charAvlStats - pts) <> " points to spend."
                Dexterity -> do
                  update (entityKey eCharacter) [Dex +=. pts, AvlStats -=. pts]
                  pure $ Right $ "Your " <> showStat stat <> " is now " <> (T.pack . show) ((dex . entityVal) eCharacter + pts) <> ". You now have " <> (T.pack . show) (charAvlStats - pts) <> " points to spend."
                Vitality -> do
                  update (entityKey eCharacter) [Vit +=. pts, AvlStats -=. pts]
                  pure $ Right $ "Your " <> showStat stat <> " is now " <> (T.pack . show) ((vit . entityVal) eCharacter + pts) <> ". You now have " <> (T.pack . show) (charAvlStats - pts) <> " points to spend."
                Energy -> do
                  update (entityKey eCharacter) [Nrg +=. pts, AvlStats -=. pts]
                  pure $ Right $ "Your " <> showStat stat <> " is now " <> (T.pack . show) ((nrg . entityVal) eCharacter + pts) <> ". You now have " <> (T.pack . show) (charAvlStats - pts) <> " points to spend."

getUserCurrentCharacter :: DT.MessageId -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) (Maybe (Entity Character))
getUserCurrentCharacter userKeyInt = do
  mUser <- selectFirst [DiscordId ==. fromIntegral userKeyInt] []
  case mUser of
    Nothing -> pure Nothing
    Just user -> do
      let userCurCharId = (currentCharId . entityVal) user
      selectFirst [CharacterId ==. (toSqlKey . fromIntegral) userCurCharId] []

snowfToUserKey :: Integral a => a -> Key User
snowfToUserKey s = UserKey {unUserKey = fromIntegral s}

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
                    nrg = gDClassNrg gdClass,
                    baseVit = gDClassVit gdClass,
                    baseNrg = gDClassNrg gdClass,
                    avlStats = 0,
                    avlSkills = 0,
                    lifePl = gDClassLifePl gdClass,
                    manaPl = gDClassManaPl gdClass,
                    lifePVit = gDClassLifePVit gdClass,
                    manaPNrg = gDClassManaPNrg gdClass,
                    highestDiff = 0,
                    currentDiff = 0,
                    currentQuest = 0,
                    waypoints = 0,
                    hasCube = False
                  }

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

-- get current difficulty from user
-- get zone from zonelvl
-- get zone
-- create monsters from zone

-- monster
type Experience = Int

getXpFromMonPackRar :: Int -> Int -> MonsterPackRarity -> IO (Either CmdError Experience)
getXpFromMonPackRar lvl diff mpr = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        mEMonLvl <- selectFirst [MonLvlLevel ==. lvl, MonLvlDifficulty ==. diff] []
        case mEMonLvl of
          Nothing -> pure $ Left "No monster level corresponding to the level of the mission."
          Just eMonLvl -> do
            mEMonDiffStat <- selectFirst [MonDiffStatTextId ==. (monStatTextId . monPStat) mpr] []
            case mEMonDiffStat of
              Nothing -> pure $ Left $ "No monster difficulty stats available for the monster with ID " <> (monStatTextId . monPStat) mpr
              Just eMonDiffStat -> pure $ Right $ monPAmount mpr * getXpFromMonster (entityVal eMonDiffStat) (entityVal eMonLvl)

getXpFromMonster :: MonDiffStat -> MonLvl -> Experience
getXpFromMonster mds ml = floor (xp * xpMult / 100)
  where
    xp = (fromIntegral :: Int -> Double) $ monDiffStatExp mds
    xpMult = fromIntegral $ monLvlXp ml

getCommonPacks :: ZoneLvl -> ZoneLvlDiffVars -> IO (Either CmdError [MonsterPackRarity])
getCommonPacks zone zoneDiffs = do
  packSize <- getAmountOfPacks zone zoneDiffs
  let tMons
        | diff == 0 = zoneLvlNormMons zone
        | otherwise = zoneLvlNmhMons zone
  eMonStats <- getMonStats tMons
  case eMonStats of
    Left err -> pure $ Left err
    Right monStats -> do
      gen <- getStdGen
      let selectedMons = selectMons monStats gen numMonTypes
      packs <- addAmountToPacks (initializeMonsterPacks selectedMons Common) packSize <$> getStdGen
      pure $ Right packs
  where
    diff = zoneLvlDiffVarsDifficulty zoneDiffs
    numMonTypes = zoneLvlNumMon zone

initializeMonsterPacks :: [MonStat] -> MonRarity -> [MonsterPackRarity]
initializeMonsterPacks ms mr = (\m -> MonsterPackRarity m 0 mr) <$> ms

addAmountToPacks :: [MonsterPackRarity] -> Int -> StdGen -> [MonsterPackRarity]
addAmountToPacks ms 0 _ = ms
addAmountToPacks [] _ _ = []
addAmountToPacks ms packsLeft gen = do
  let (spawnRoll, newGen) = rollSpawnRarity mStat gen
  addAmountToPacks (DF.toList $ DS.update currIndex (MonsterPackRarity mStat (amnt + spawnRoll) rarity) (DS.fromList ms)) (packsLeft - spawnRoll) newGen
  where
    currIndex = packsLeft `mod` length ms
    currMon = ms !! currIndex
    mStat = monPStat currMon
    amnt = monPAmount currMon
    rarity = monPRarity currMon

rollSpawnRarity :: MonStat -> StdGen -> (Int, StdGen)
rollSpawnRarity mStat gen
  | msRarity == 1 = (1, gen)
  | otherwise = randomR (0, 1) gen
  where
    msRarity = monStatRarity mStat

selectMons :: [MonStat] -> StdGen -> Int -> [MonStat]
selectMons mms gen amount = do
  let (shuffled, newGen) = shuffle mms gen
  take amount shuffled

getMonStats :: [T.Text] -> IO (Either CmdError [MonStat])
getMonStats ms = do
  monStats <- mapM getMonStat ms
  pure $ sequence monStats

getMonStat :: T.Text -> IO (Either CmdError MonStat)
getMonStat monId = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        mMonStat <- selectFirst [MonStatTextId ==. monId] []
        case mMonStat of
          Just monStat -> pure $ Right $ entityVal monStat
          Nothing -> pure $ Left $ "The monster " <> monId <> " does not have corresponding stats. Please report this to a developer."

getAmountOfPacks :: ZoneLvl -> ZoneLvlDiffVars -> IO Int
getAmountOfPacks zone zoneDiffs = do
  let g = newStdGen
  rndAmntPacks density [1 .. area]
  where
    area = zoneLvlDiffVarsSizeX zoneDiffs * zoneLvlDiffVarsSizeY zoneDiffs
    density = zoneLvlDiffVarsMonDen zoneDiffs
    isInside = zoneLvlIsInside zone

getZoneLvl :: T.Text -> IO (Maybe ZoneLvl)
getZoneLvl z = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        mZoneLvl <- selectFirst [ZoneLvlZoneName ==. z] []
        case mZoneLvl of
          Nothing -> pure Nothing
          Just zoneLvl -> (pure . Just . entityVal) zoneLvl

getZoneDiff :: T.Text -> Int -> IO (Maybe ZoneLvlDiffVars)
getZoneDiff z diff = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        mZoneLvlDiff <- selectFirst [ZoneLvlDiffVarsZoneName ==. z, ZoneLvlDiffVarsDifficulty ==. diff] []
        case mZoneLvlDiff of
          Nothing -> pure Nothing
          Just zoneLvlDiff -> (pure . Just . entityVal) zoneLvlDiff

packSpawnTop :: Int
packSpawnTop = 100000

-- maybe reduce this into O(1) later by doing equation with rand e.g. density * area / packSpawnTop + rand some int idk

rndAmntPacks :: Int -> [Int] -> IO Int
rndAmntPacks _ [] = pure 0
rndAmntPacks density (_ : xs) = do
  spawnDecider <- (randomRIO :: (Int, Int) -> IO Int) (1, packSpawnTop)
  let packShouldSpawn
        | density >= spawnDecider = pure 1
        | otherwise = pure 0
  (+) <$> packShouldSpawn <*> rndAmntPacks density xs

getCharLvlFromExp :: Experience -> IO (Maybe Int)
getCharLvlFromExp e = do
  loadFile defaultConfig
  connectionString <- connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
        mGDExperience <- selectFirst [GDExperienceNextLevelAbs >. e] []
        case mGDExperience of
          Nothing -> pure Nothing
          Just gDExperience -> (pure . Just . gDExperienceLevel . entityVal) gDExperience

createTripStatus :: Trip -> IO (DT.ChannelId, Either CmdError DbStatus)
createTripStatus t = do
  loadFile defaultConfig
  connectionString <- liftIO connString
  runStderrLoggingT $
    withPostgresqlPool connectionString 10 $ \pool -> liftIO $ do
      flip runSqlPersistMPool pool $ do
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
      flip runSqlPersistMPool pool $ do
        liftIO $ createTripStatus t

type MonsterTextId = T.Text

getMPRaritiesFromLists :: [MonsterTextId] -> [(Amount, MonRarity)] -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) [MonsterPackRarity]
getMPRaritiesFromLists [] _ = pure []
getMPRaritiesFromLists _ [] = pure []
getMPRaritiesFromLists ms xs = do
  monStats <- selectList [MonStatTextId <-. ms] []
  pure $
    zipWith
      ( \eMonStat (a, r) ->
          MonsterPackRarity
            { monPStat = entityVal eMonStat,
              monPAmount = a,
              monPRarity = r
            }
      )
      monStats
      xs
