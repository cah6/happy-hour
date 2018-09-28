module Common.Dto where

import qualified Data.Attoparsec.Text as AP
import           Data.Attoparsec.Text (decimal, char)
import qualified Money
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Data.Aeson.Types
import Control.Lens (makeLenses)
import Data.Monoid ((<>))
import Data.Text (Text, pack, breakOn, intercalate, unpack)
import Data.Time (formatTime)
import Data.Time.Exts.Base hiding (pack, unpack)
import Data.Time.Calendar (Day(..))
import Data.Time.Format (defaultTimeLocale)
import Data.Time.LocalTime (TimeOfDay(..))
import System.Directory (listDirectory)

import Common.Helper (attoToAeson)
import GHC.Generics (Generic)

type USD = Money.Discrete "USD" "cent"

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
  char ':'
  startM <- decimal
  char '-'
  endH <- decimal
  char ':'
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
  { _days :: [DayOfWeek Gregorian]
  , _time :: TimeRange
  , _scheduleDescription :: Text
  } deriving (Generic, Show)

instance ToJSON Schedule where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }

printDays :: [DayOfWeek Gregorian] -> Text
printDays []  = ""
printDays [x]  = printDay x
printDays xs = printDay (head xs) <> "-" <> printDay (last xs)  

printDay :: DayOfWeek Gregorian -> Text 
printDay = \case
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

instance ToJSON (DayOfWeek 'Gregorian) where
  toJSON a = String $ (pack . show) a

data MenuItem = MenuItem
  { _id :: Int
  , _itemDescription :: Text
  , _price :: USD
  } deriving (Generic, Show)

instance FromJSON MenuItem where
  parseJSON = withObject "menuItem" $ \o -> do
    _id <- o .: "id"
    _itemDescription  <- o .: "itemDescription"
    _price <- fromInteger <$> o .: "price"
    return MenuItem{..}

mkUSD :: Integer -> USD
mkUSD = fromInteger

ppUSD :: USD -> Text
ppUSD usd = "$" <> (pack . show) dollar <> "." <> (pack . show) cents
  where (dollar, cents) = divMod (toInteger usd) 100

-- Make all the lenses

makeLenses ''HappyHour
makeLenses ''Schedule