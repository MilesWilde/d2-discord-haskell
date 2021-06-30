module Db.DbMonstersZones where

import CharClasses
import Commands
import Configuration.Dotenv (defaultConfig, loadFile)
import Control.Monad.Logger (NoLoggingT, runStderrLoggingT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import qualified Data.Foldable as DF
import qualified Data.Sequence as DS
import qualified Data.Text as T
import Database.Persist.Postgresql
import Db.DbHelpers
import Db.Migrations
import qualified Discord.Types as DT
import Stats
import System.Environment
import System.Random
import UnliftIO (liftIO, throwIO)

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

getMonStats :: [MonsterTextId] -> IO (Either CmdError [MonStat])
getMonStats ms = do
  monStats <- mapM getMonStat ms
  pure $ sequence monStats

getMonStat :: MonsterTextId -> IO (Either CmdError MonStat)
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

type ZoneName = T.Text

getZoneLvl :: ZoneName -> IO (Maybe ZoneLvl)
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

getZoneDiff :: ZoneName -> Int -> IO (Maybe ZoneLvlDiffVars)
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