module Core where

import Commands
import Control.Monad (forM_, when)
import Control.Monad.Reader (ReaderT)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock.POSIX
import Database.Persist.Sql
import Db.DbCharacter
import Db.DbCommands
import Db.DbHelpers
import Db.DbMonstersZones
import Db.DbTrip
import Db.Migrations
import Discord
import qualified Discord.Requests as R
import Discord.Types
import Stats
import UnliftIO (liftIO)
import UnliftIO.Concurrent
import Views

runDiabloServer :: IO ()
runDiabloServer = do
  tok <- TIO.readFile "auth-token.secret"

  -- open ghci and run  [[ :info RunDiscordOpts ]] to see available fields
  t <-
    runDiscord $
      def
        { discordToken = tok,
          discordOnStart = startHandler,
          discordOnEnd = liftIO $ putStrLn "Ended",
          discordOnEvent = eventHandler,
          discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
        }
  threadDelay (1 `div` 10 * 10 ^ (6 :: Int))
  TIO.putStrLn t

-- If the start handler throws an exception, discord-haskell will gracefully shutdown
--     Use place to execute commands you know you want to complete
startHandler :: DiscordHandler ()
startHandler = do
  Right partialGuilds <- restCall R.GetCurrentUserGuilds

  let activity =
        Activity
          { activityName = "D2-Haskell",
            activityType = ActivityTypeGame,
            activityUrl = Just "https://github.com/MilesWilde/d2-discord-haskell"
          }
  let opts =
        UpdateStatusOpts
          { updateStatusOptsSince = Nothing,
            updateStatusOptsGame = Just activity,
            updateStatusOptsNewStatus = UpdateStatusOnline,
            updateStatusOptsAFK = False
          }
  sendCommand (UpdateStatus opts)
  forkIO tripThread
  forM_ partialGuilds $ \pg -> do
    Right guild <- restCall $ R.GetGuild (partialGuildId pg)
    Right chans <- restCall $ R.GetGuildChannels (guildId guild)
    case filter isTextChannel chans of
      (c : _) -> do
        _ <- restCall $ R.CreateMessage (channelId c) "Hello! Try to \\create a character, and then send it on a \\run"
        pure ()
      _ -> pure ()

tripThread :: DiscordHandler ()
tripThread = do
  mETrips <- liftIO getFinishedTrips
  case mETrips of
    Nothing -> pure ()
    Just eTrips ->
      do
        let trips = entityVal <$> eTrips
        eTripMessages <- liftIO $ sequence $ createTripMessage <$> trips
        forM_
          eTripMessages
          ( \(chanId, eTripMessage) -> do
              case eTripMessage of
                Left err -> restCall (R.CreateMessage chanId err)
                Right tripMessage -> restCall (R.CreateMessage chanId tripMessage)
          )
        liftIO $ updateTripsComplete eTrips
  threadDelay (10 ^ 6)
  tripThread

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (not (fromBot m) && isBotCommand m) $ do
    createUserStatus <- liftIO $ createUser (messageUserId m)
    case getCommand m of
      Left error -> restCall (R.CreateMessage mChannel error)
      Right cmd -> case cmd of
        Quest -> do
          restCall (R.CreateMessage mChannel "Quest")
        Run amnt zone -> do
          createTripStatus <- liftIO $ createTrip amnt zone muid mChannel
          case createTripStatus of
            Right status -> restCall (R.CreateMessage mChannel status)
            Left err -> restCall (R.CreateMessage mChannel err)
        CreateCharacter cClass cName -> do
          createCharacterStatus <- liftIO $ createCharacter cClass cName userKey
          case createCharacterStatus of
            Right status -> restCall (R.CreateMessage mChannel status)
            Left err -> restCall (R.CreateMessage mChannel err)
        SelectCharacter cName -> do
          selectCharacterStatus <- liftIO $ selectCharacter cName userKey
          case selectCharacterStatus of
            Right status -> restCall (R.CreateMessage mChannel status)
            Left err -> restCall (R.CreateMessage mChannel err)
        View viewCommand cName ->
          case cName of
            Nothing -> sendViewMessage viewCommand m
            Just name -> do
              selectCharacterStatus <- liftIO $ selectCharacter name userKey
              case selectCharacterStatus of
                Left err -> restCall (R.CreateMessage mChannel err)
                Right _ -> sendViewMessage viewCommand m
        AssignStats amount stat -> do
          assignStatsStatus <- liftIO $ assignStats amount stat muid
          case assignStatsStatus of
            Right status -> restCall (R.CreateMessage mChannel status)
            Left err -> restCall (R.CreateMessage mChannel err)
        AssignSkills int skill -> restCall (R.CreateMessage mChannel "AssignSkill")
        Equip -> restCall (R.CreateMessage mChannel "Equip")
        Unequip -> restCall (R.CreateMessage mChannel "Unequip")
        Cancel -> restCall (R.CreateMessage mChannel "Unequip")
    pure ()
    where
      muid = messageUserId m
      userKey = snowfToUserKey muid
      mChannel = messageChannel m
  _ -> pure ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isBotCommand :: Message -> Bool
isBotCommand = (== '\\') . T.head . messageText

showSnowflake :: Snowflake -> T.Text
showSnowflake (Snowflake w) = (T.pack . show) w

messageUserId :: Message -> Snowflake
messageUserId = userId . messageAuthor

sendViewMessage :: ViewCommand -> Message -> DiscordHandler (Either RestCallErrorCode Message)
sendViewMessage vc m = case vc of
  Characters -> do
    viewCharsStatus <- liftIO $ viewCharacterNames userKey
    case viewCharsStatus of
      Right status -> restCall (R.CreateMessage (messageChannel m) status)
      Left err -> restCall (R.CreateMessage (messageChannel m) err)
  Stats -> do
    viewStatsStatus <- liftIO $ viewStats (messageUserId m)
    case viewStatsStatus of
      Right status -> restCall (R.CreateMessage (messageChannel m) status)
      Left err -> restCall (R.CreateMessage (messageChannel m) err)
  Skills -> restCall (R.CreateMessage (messageChannel m) "View Skills")
  Inventory -> restCall (R.CreateMessage (messageChannel m) "View Inventory")
  where
    userKey = snowfToUserKey $ messageUserId m
