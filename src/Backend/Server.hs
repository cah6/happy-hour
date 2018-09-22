module Backend.Server where

import qualified Data.ByteString.Lazy as B

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (MonadLogger(..), logInfoN)
import Data.Aeson
import Data.Text
import Servant

import Common.Dto

putHH :: (MonadLogger m, MonadError ServantErr m)
  => Integer 
  -> HappyHour 
  -> m ()
putHH i hh = do
  throwError err400
  (logInfoN . pack . show) hh 
  return ()

getHH :: (MonadLogger m) 
  => Integer 
  -> m HappyHour
getHH a = do 
  logInfoN "Getting hh by id..."
  return defaultHH

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