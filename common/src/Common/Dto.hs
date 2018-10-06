{-# LANGUAGE RecordWildCards, DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}
module Common.Dto where

import qualified Data.Attoparsec.Text as AP
import           Data.Attoparsec.Text (decimal, char)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
-- import Control.Lens (makeLenses)
import Data.Monoid ((<>))
import Data.Text (Text, pack, breakOn, intercalate, unpack)
import Data.Time (formatTime)
import Data.Time.Calendar (Day(..))
import Data.Time.Format (defaultTimeLocale)
import Data.Time.LocalTime (TimeOfDay(..))
import System.Directory (listDirectory)

import Common.Helper (attoToAeson)
import GHC.Generics (Generic)

newtype TimeRange = TimeRange (TimeOfDay, TimeOfDay)
  deriving (Generic)

instance ToJSON TimeRange where
  toJSON = String . printTimeRangeMilitary

instance Show TimeRange where
  show a = (unpack . printTimeRange) a

instance FromJSON TimeRange where
  parseJSON = attoToAeson parseTimeRange

printTimeRange :: TimeRange -> Text 
printTimeRange (TimeRange (start, end)) = printTimeOfDay start <> "-" <> printTimeOfDay end 

printTimeRangeMilitary :: TimeRange -> Text 
printTimeRangeMilitary (TimeRange (start, end)) = 
  printTimeOfDayMilitary start <> "-" <> printTimeOfDayMilitary end 

printTimeOfDay :: TimeOfDay -> Text
printTimeOfDay = pack . formatTime defaultTimeLocale "%-l:%M%P"

printTimeOfDayMilitary :: TimeOfDay -> Text
printTimeOfDayMilitary = pack . formatTime defaultTimeLocale "%H:%M"

loadHHs :: IO (Either String [HappyHour])
loadHHs = do
  let baseDir = "resources/data/"
  filenames <- listDirectory baseDir
  bytestrings <- traverse (B.readFile . (<>) baseDir) filenames
  return $ traverse eitherDecode bytestrings

parseTimeRange :: AP.Parser TimeRange
parseTimeRange = do
  startH <- decimal
  _ <- char ':'
  startM <- decimal
  _ <- char '-'
  endH <- decimal
  _ <- char ':'
  endM <- decimal
  return $ TimeRange (TimeOfDay startH startM 0, TimeOfDay endH endM 0)

defaultHH :: HappyHour
defaultHH = HappyHour 
  { _city = ""
  , _restaurant = ""
  , _schedule = []
  , _link = ""
  }

data HappyHour = HappyHour
  { _city :: Text
  , _restaurant :: Text
  , _schedule :: [Schedule]
  , _link :: Text
  } deriving (Generic, Show)

instance ToJSON HappyHour where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

instance FromJSON HappyHour where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

data Schedule = Schedule
  { _days :: [DayOfWeek]
  , _time :: TimeRange
  , _scheduleDescription :: Text
  } deriving (Generic, Show)

data DayOfWeek =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Generic, Show, Read, Eq, Ord, ToJSON, FromJSON)

instance ToJSON Schedule where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

printDays :: [DayOfWeek] -> Text
printDays []  = ""
printDays [x]  = printDay x
printDays xs = printDay (head xs) <> "-" <> printDay (last xs)

printDay :: DayOfWeek -> Text 
printDay day = case day of
  Monday -> "M"
  Tuesday -> "Tu"
  Wednesday -> "W"
  Thursday -> "Th"
  Friday -> "F"
  Saturday -> "Sat"
  Sunday -> "Sun"

instance FromJSON Schedule where
  parseJSON = withObject "schedule" $ \o -> do
    _days                 <- map read <$> o .: "days"
    _time                 <- o .: "time" >>= parseJSON
    _scheduleDescription  <- o .: "scheduleDescription"
    return Schedule{..}

-- Make all the lenses

-- makeLenses ''HappyHour
-- makeLenses ''Schedule