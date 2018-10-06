{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ServantClient where

import Common.Routes
import Data.Proxy
import Data.UUID
import Reflex.Dom 
import Servant.API
import Servant.Reflex

-- createHH :<|> updateHH :<|> deleteHH :<|> getHH :<|> queryHH = 
--   client hhApi (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn (BasePath "/"))