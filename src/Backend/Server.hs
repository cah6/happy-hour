module Backend.Server where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Builder as B

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (MonadLogger(..), logInfoN)
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.UUID
import Servant

import Backend.Interfaces
import Backend.Tables as DB
import Common.Dto as DTO

createHH :: (MonadLogger m, MonadError ServantErr m, SaveHappyHour m, GenUUID m)
  => DTO.HappyHour 
  -> m UUID
createHH hh = do
  logInfoN $ "POST called with data:\n" <> (toStrict . toLazyText . encodePrettyToTextBuilder) hh 
  uuid <- genUUID
  createHappyHour $ happyHourDtoToDb uuid hh
  return uuid

getHH :: (MonadLogger m) 
  => Integer 
  -> m DTO.HappyHour
getHH a = do 
  logInfoN "Getting happy hour by id..."
  return defaultHH

getHHs :: (MonadLogger m, QueryHappyHours m) 
  => m [DTO.HappyHour]
getHHs = do 
  logInfoN "Getting all happy hours..."
  dbHHs <- getAllHappyHours
  return (happyHourDbToDto <$> dbHHs)

getHappyHourById :: Integer -> Handler DTO.HappyHour
getHappyHourById a = do 
  menu <- liftIO jnkMenu
  case (parseHH menu) of
    Left err -> throwError err400
    Right hh -> return hh

jnkMenu :: IO B.ByteString
jnkMenu = B.readFile "resources/data/johnny_noodle_king.json"

parseHH :: B.ByteString -> Either String DTO.HappyHour
parseHH bs = eitherDecode bs

happyHourDtoToDb :: UUID -> DTO.HappyHour -> DB.HappyHour
happyHourDtoToDb uuid DTO.HappyHour{..} = DB.HappyHour { _id = uuid, ..}

happyHourDbToDto :: DB.HappyHour -> DTO.HappyHour
happyHourDbToDto DB.HappyHour{..} = defaultHH 
  { DTO._city = _city
  , DTO._restaurant = _restaurant
  }