module Db.DbHelpers where

import CharClasses
import Commands
import Data.ByteString.Internal as BLU
import Database.Persist (Entity (..))
import Database.Persist.Postgresql
import Discord.Types
import System.Environment

-- connString = pure "host=ip port=port user=whatever dbname=whatever password=pass"
connString :: IO ConnectionString
connString = stringToConn <$> getEnv "DB_CONNECTION_STRING"

stringToConn :: String -> ConnectionString
stringToConn = BLU.packChars

messageUserId :: Message -> Snowflake
messageUserId = userId . messageAuthor
