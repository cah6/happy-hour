{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module ServantClient where

import Control.Monad.Trans (liftIO, MonadIO)
import Data.Aeson
import Data.Proxy
import Data.UUID
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
#ifdef ghcjs_HOST_OS
import Servant.Client.Ghcjs (client, ClientEnv(..))
import Servant.Client.Internal.XhrClient (runClientMOrigin)
#else
import Servant.Client (runClientM, client, ClientEnv(..))
#endif

import Reflex.Dom.Core

import Common.Dto
import Common.Routes

(createHH :<|> updateHH :<|> deleteHH :<|> getHH :<|> queryHH) = client hhApi

#ifdef ghcjs_HOST_OS
runClientM' = runClientMOrigin
#else
runClientM' = runClientM
#endif

-- restCreateHH :: (PerformEvent t m, MonadWidget t m)
restCreateHH :: (PerformEvent t m, MonadIO (Performable m))
  => ClientEnv
  -> Event t HappyHour
  -> m (Event t (Maybe UUID))
restCreateHH env event = performEvent $ ffor event $ \hh -> liftIO $ do 
  servantResponse <- runClientM' (createHH hh) env
  case servantResponse of 
    Left err -> return Nothing
    Right a -> return (Just a)

restQueryHH :: (PerformEvent t m, MonadIO (Performable m))
  => ClientEnv
  -> Event t ()
  -> m (Event t [HappyHour])
restQueryHH env event = performEvent $ ffor event $ \_ -> liftIO $ do 
  servantResponse <- runClientM' queryHH env
  case servantResponse of 
    Left err -> return []
    Right a -> return a