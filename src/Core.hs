module Core where

import Commands
import Control.Monad (forM_, when)
import Control.Monad.Reader (ReaderT)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Db.DbCommands
import Db.DbHelpers
import Discord
import qualified Discord.Requests as R
import Discord.Types
import UnliftIO (liftIO)
import UnliftIO.Concurrent

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
          { activityName = "ping-pong",
            activityType = ActivityTypeGame,
            activityUrl = Nothing
          }
  let opts =
        UpdateStatusOpts
          { updateStatusOptsSince = Nothing,
            updateStatusOptsGame = Just activity,
            updateStatusOptsNewStatus = UpdateStatusOnline,
            updateStatusOptsAFK = False
          }
  sendCommand (UpdateStatus opts)

  forM_ partialGuilds $ \pg -> do
    Right guild <- restCall $ R.GetGuild (partialGuildId pg)
    Right chans <- restCall $ R.GetGuildChannels (guildId guild)
    case filter isTextChannel chans of
      (c : _) -> do
        _ <- restCall $ R.CreateMessage (channelId c) "Hello! Try to \\create a character, and then send it on a \\run"
        pure ()
      _ -> pure ()

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (not (fromBot m) && isBotCommand m) $ do
    createUserStatus <- liftIO $ createUser (messageUserId m)
    case getCommand m of
      Left error -> restCall (R.CreateMessage (messageChannel m) error)
      Right cmd -> case cmd of
        Quest -> do
          restCall (R.CreateMessage (messageChannel m) "Quest")
        Run -> restCall (R.CreateMessage (messageChannel m) "Run")
        CreateCharacter cClass cName -> do
          createCharacterStatus <- liftIO $ createCharacter cClass cName userKey
          case createCharacterStatus of
            Right status -> restCall (R.CreateMessage (messageChannel m) status)
            Left err -> restCall (R.CreateMessage (messageChannel m) err)
        SelectCharacter cName -> do
          selectCharacterStatus <- liftIO $ selectCharacter cName userKey
          case selectCharacterStatus of
            Right status -> restCall (R.CreateMessage (messageChannel m) status)
            Left err -> restCall (R.CreateMessage (messageChannel m) err)
        View viewCommand cName ->
          case cName of
            Nothing -> sendViewMessage viewCommand m
            Just name -> do
              selectCharacterStatus <- liftIO $ selectCharacter name userKey
              case selectCharacterStatus of
                Left err -> restCall (R.CreateMessage (messageChannel m) err)
                Right _ -> sendViewMessage viewCommand m
        AssignStats amount stat -> restCall (R.CreateMessage (messageChannel m) "AssignStats")
        AssignSkills int skill -> restCall (R.CreateMessage (messageChannel m) "AssignSkill")
        Equip -> restCall (R.CreateMessage (messageChannel m) "Equip")
        Unequip -> restCall (R.CreateMessage (messageChannel m) "Unequip")
    pure ()
    where
      userKey = snowflakeToKey $ messageUserId m
  _ -> pure ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isBotCommand :: Message -> Bool
isBotCommand = (== '\\') . T.head . messageText

showSnowflake :: Snowflake -> String
showSnowflake (Snowflake w) = show w

messageUserId :: Message -> Snowflake
messageUserId = userId . messageAuthor

sendViewMessage :: ViewCommand -> Message -> DiscordHandler (Either RestCallErrorCode Message)
sendViewMessage vc m = case vc of
  Characters -> do
    viewCharsStatus <- liftIO $ viewCharactersNames userKey
    case viewCharsStatus of
      Right status -> restCall (R.CreateMessage (messageChannel m) status)
      Left err -> restCall (R.CreateMessage (messageChannel m) err)
  Stats -> restCall (R.CreateMessage (messageChannel m) "View Stats")
  Skills -> restCall (R.CreateMessage (messageChannel m) "View Skills")
  Inventory -> restCall (R.CreateMessage (messageChannel m) "View Inventory")
  where
    userKey = snowflakeToKey $ messageUserId m
