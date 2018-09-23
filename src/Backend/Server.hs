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
import Servant

import Backend.Tables ()
import Common.Dto

putHH :: (MonadLogger m, MonadError ServantErr m)
  => Integer 
  -> HappyHour 
  -> m ()
putHH i hh = do
  logInfoN $ "PUT called with data:\n" <> (toStrict . toLazyText . encodePrettyToTextBuilder) hh 
  throwError err400
  return ()

getHH :: (MonadLogger m) 
  => Integer 
  -> m HappyHour
getHH a = do 
  logInfoN "Getting happy hour by id..."
  return defaultHH

getAllHH :: (MonadLogger m) 
  => m [HappyHour]
getAllHH = do 
  logInfoN "Getting all happy hours..."
  return [defaultHH]

getHappyHourById :: Integer -> Handler HappyHour
getHappyHourById a = do 
  menu <- liftIO jnkMenu
  case (parseHH menu) of
    Left err -> throwError err400
    Right hh -> return hh

jnkMenu :: IO B.ByteString
jnkMenu = B.readFile "resources/data/johnny_noodle_king.json"

parseHH :: B.ByteString -> Either String HappyHour
parseHH bs = eitherDecode bs
