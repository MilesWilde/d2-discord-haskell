module Db.DbHelpers where

import CharClasses
import Commands
import Control.Concurrent
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.ByteString.Internal as BLU
import Data.Char
import Data.List
import Data.STRef
import qualified Data.Text as T
import Database.Persist (Entity (..))
import Database.Persist.Postgresql
import Discord.Types
import System.Environment
import System.Random

data Difficulty = Normal | Nightmare | Hell deriving (Show, Eq, Enum)

-- connString = pure "host=ip port=port user=whatever dbname=whatever password=pass"
connString :: IO ConnectionString
connString = stringToConn <$> getEnv "DB_CONNECTION_STRING"

stringToConn :: String -> ConnectionString
stringToConn = BLU.packChars

upperFirst :: [T.Text] -> T.Text
upperFirst t = T.unwords $ map upperCaseWord t

upperCaseWord :: T.Text -> T.Text
upperCaseWord t = (T.singleton . toUpper . T.head) t <> (T.toLower . T.tail) t

shuffle :: [a] -> StdGen -> ([a], StdGen)
shuffle xs gen =
  runST
    ( do
        g <- newSTRef gen
        let randomRST lohi = do
              (a, s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1 .. n] $ \i -> do
          j <- randomRST (i, n)
          vi <- readArray ar i
          vj <- readArray ar j
          writeArray ar j vi
          return vj
        gen' <- readSTRef g
        return (xs', gen')
    )
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs = newListArray (1, n) xs