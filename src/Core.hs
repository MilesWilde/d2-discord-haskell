module Core where

import Commands
import Control.Monad (forM_, when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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
    -- threadDelay (4 * 10 ^ (6 :: Int))
    case (getCommand . T.tail . messageText) m of
      Quest -> restCall (R.CreateMessage (messageChannel m) "Quest")
      Run -> restCall (R.CreateMessage (messageChannel m) "Run")
      CreateCharacter -> restCall (R.CreateMessage (messageChannel m) "CreateCharacter")
      SelectCharacter -> restCall (R.CreateMessage (messageChannel m) "SelectCharacter")
      ViewSkills -> restCall (R.CreateMessage (messageChannel m) "ViewSkills")
      ViewStats -> restCall (R.CreateMessage (messageChannel m) "ViewStats")
      Equip -> restCall (R.CreateMessage (messageChannel m) "Equip")
      Unequip -> restCall (R.CreateMessage (messageChannel m) "Unequip")
      Default -> restCall (R.CreateMessage (messageChannel m) "Default")

    _ <- restCall (R.CreateMessage (messageChannel m) ("Your ID: " <> (T.pack . show . messageUserId) m))
    _ <- restCall (R.CreateMessage (messageChannel m) ("Your Text: " <> (T.pack . show . messageText) m))
    -- _ <- restCall (R.CreateReaction (messageChannel m, messageId m) "eyes")
    -- liftIO . putStrLn . show . messageUserId $ m
    -- liftIO . putStrLn . show . messageText $ m
    -- -- A very simple message.
    -- _ <- restCall (R.CreateMessage (messageChannel m) "Pong!")

    -- -- A more complex message. Text-to-speech, does not mention everyone nor
    -- -- the user, and uses Discord native replies.
    -- -- Use ":info" in ghci to explore the type
    -- let opts :: R.MessageDetailedOpts
    --     opts =
    --       def
    --         { R.messageDetailedContent = "Here's a more complex message, but doesn't ping @everyone!",
    --           R.messageDetailedTTS = True,
    --           R.messageDetailedAllowedMentions =
    --             Just $
    --               def
    --                 { R.mentionEveryone = False,
    --                   R.mentionRepliedUser = False
    --                 },
    --           R.messageDetailedReference =
    --             Just $
    --               def {referenceMessageId = Just $ messageId m}
    --         }
    -- _ <- restCall (R.CreateMessageDetailed (messageChannel m) opts)

    pure ()
  _ -> pure ()

isTextChannel :: Channel -> Bool
isTextChannel (ChannelText {}) = True
isTextChannel _ = False

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isBotCommand :: Message -> Bool
isBotCommand = (== '\\') . T.head . messageText

messageUserId :: Message -> Snowflake
messageUserId = userId . messageAuthor

showSnowflake :: Snowflake -> String
showSnowflake (Snowflake w) = show w
