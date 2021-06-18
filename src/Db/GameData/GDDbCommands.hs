{-# LANGUAGE DeriveGeneric #-}

module Db.GameData.GDDbCommands where

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Db.Migrations
import GHC.Generics

-- classStats :: IO (Either T.Text JGDClass)
-- classStats = do
--   fileContents <- B.readFile "d2data/charstats.json"
--   case decode fileContents of
--     Nothing -> pure $ Left "Some Issue?"
--     Just contents -> pure contents

data JGDClass = GDClass
  { cclass :: T.Text,
    str :: Int,
    dex :: Int,
    vit :: Int,
    int :: Int,
    hpadd :: Int,
    lifePerLevel :: Int,
    manaPerLevel :: Int,
    lifePerVitality :: Int,
    manaPerMagic :: Int
  }
  deriving (Show, Generic)

instance FromJSON JGDClass

instance ToJSON JGDClass
