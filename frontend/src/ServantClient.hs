{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module ServantClient where

import Control.Monad.Trans (liftIO)
import Data.Aeson
import Data.Proxy
import Data.UUID
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Reflex.Dom.Core

import Common.Dto
import Common.Routes

(createHH :<|> updateHH :<|> deleteHH :<|> getHH :<|> queryHH) = client hhApi

restCreateHH :: (PerformEvent t m, MonadWidget t m)
  => ClientEnv
  -> Event t HappyHour
  -> m (Event t (Maybe UUID))
restCreateHH env event = performEvent $ ffor event $ \hh -> liftIO $ do 
  servantResponse <- runClientM (createHH hh) env
  case servantResponse of 
    Left err -> return Nothing
    Right a -> return (Just a)

restQueryHH :: (PerformEvent t m, MonadWidget t m)
  => ClientEnv
  -> Event t ()
  -> m (Event t [HappyHour])
restQueryHH env event = performEvent $ ffor event $ \_ -> liftIO $ do 
  servantResponse <- runClientM queryHH env
  case servantResponse of 
    Left err -> return []
    Right a -> return a