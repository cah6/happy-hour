{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module ServantClient where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

import Common.Dto
import Common.Routes

(createHH :<|> updateHH :<|> deleteHH :<|> getHH :<|> queryHH) = client hhApi