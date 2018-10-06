{-# LANGUAGE DeriveAnyClass #-}
module Backend.Tables where

import Database.Beam
import Data.Text (Text)
import Data.UUID

-- Define DB
data HappyHourDb f = HappyHourDb
  { _tableHappyHour :: f (TableEntity HappyHourT)
  , _tableSchedules :: f (TableEntity ScheduleT)
  } deriving (Generic, Database be)

happyHourDb :: DatabaseSettings be HappyHourDb
happyHourDb = defaultDbSettings

-- HappyHour table
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

-- Schedule table
data ScheduleT f = Schedule
  { _scheduleId :: Columnar f Int
  , _scheduleHappyHour :: PrimaryKey HappyHourT f
  , _scheduleDayOfWeek :: Columnar f Text 
  } deriving (Generic, Beamable)

instance Table ScheduleT where
  data PrimaryKey ScheduleT f = ScheduleId (Columnar f Int) deriving (Generic, Beamable)
  primaryKey = ScheduleId . _scheduleId

type Schedule = ScheduleT Identity
type ScheduleId = PrimaryKey ScheduleT Identity

deriving instance Show (PrimaryKey HappyHourT Identity)
deriving instance Show Schedule