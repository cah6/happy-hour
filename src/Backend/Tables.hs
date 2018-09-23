{-# LANGUAGE DeriveAnyClass #-}
module Backend.Tables where

import Database.Beam
import Data.Text (Text)
import Data.UUID

-- Define DB
data HappyHourDb f = HappyHourDb
  { _tableHappyHour :: f (TableEntity HappyHourT)
  } deriving (Generic, Database be)

happyHourDb :: DatabaseSettings be HappyHourDb
happyHourDb = defaultDbSettings

-- Define table
data HappyHourT f = HappyHour
  { _id :: Columnar f UUID
  , _city :: Columnar f Text
  , _restaurant :: Columnar f Text
  } deriving (Generic, Beamable)

instance Table HappyHourT where
  data PrimaryKey HappyHourT f = HappyHourId (Columnar f UUID) deriving (Generic, Beamable)
  primaryKey = HappyHourId . _id

type HappyHour = HappyHourT Identity
type HappyHourId = PrimaryKey HappyHourT Identity

deriving instance Show HappyHour
deriving instance Eq HappyHour
