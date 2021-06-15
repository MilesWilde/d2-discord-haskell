module Core where

import Commands
import Control.Monad (forM_, when)
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
        _ <- restCall $ R.CreateMessage (channelId c) "Hello! I will reply to pings with pongs"
        pure ()
      _ -> pure ()

-- If an event handler throws an exception, discord-haskell will continue to run
eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
  MessageCreate m -> when (not (fromBot m) && isBotCommand m) $ do
    createUserStatus <- liftIO $ createUser muid
    case getCommand m of
      Right cmd -> case cmd of
        Quest -> do
          restCall (R.CreateMessage (messageChannel m) "Quest")
        Run -> restCall (R.CreateMessage (messageChannel m) "Run")
        CreateCharacter cc name -> do
          createCharacterStatus <- liftIO $ createCharacter cc name muid
          case createCharacterStatus of
            Right status -> restCall (R.CreateMessage (messageChannel m) status)
            Left err -> restCall (R.CreateMessage (messageChannel m) err)
        SelectCharacter -> restCall (R.CreateMessage (messageChannel m) "SelectCharacter")
        ViewSkills -> restCall (R.CreateMessage (messageChannel m) "ViewSkills")
        ViewStats -> restCall (R.CreateMessage (messageChannel m) "ViewStats")
        Equip -> restCall (R.CreateMessage (messageChannel m) "Equip")
        Unequip -> restCall (R.CreateMessage (messageChannel m) "Unequip")
      Left error -> restCall (R.CreateMessage (messageChannel m) error)
    pure ()
    where
      muid = messageUserId m
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
