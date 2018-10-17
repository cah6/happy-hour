{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | This modules purpose is just to generate the xhr clients.
--   there is some type magick going on generating these,
--   therefore the functions are isolated.
module ServantReflexClient where

import Common.Dto
import Common.Routes
import Data.UUID
import Reflex
import Reflex.Dom
import qualified Data.Text as T
import Servant.API
import Common.Routes
import Servant.Reflex
import Data.Proxy

apiClients :: forall t m. (MonadWidget t m) => _
apiClients = client hhApi (Proxy @m) (Proxy @()) (constDyn url)
  where url :: BaseUrl
        url = BaseFullUrl Http "localhost" 3000 "/"

createHH :: MonadWidget t m 
  => Dynamic t (Either T.Text HappyHour) 
  -> Event t () 
  -> m (Event t (ReqResult () UUID))
updateHH :: MonadWidget t m
  => Dynamic t (Either T.Text UUID)
  -> Dynamic t (Either T.Text HappyHour)
  -> Event t ()
  -> m (Event t (ReqResult () NoContent))
deleteHH :: MonadWidget t m
  => Dynamic t (Either T.Text UUID)
  -> Event t () 
  -> m (Event t (ReqResult () NoContent))
getHH :: MonadWidget t m
  => Dynamic t (Either T.Text UUID)
  -> Event t () 
  -> m (Event t (ReqResult () HappyHour))
queryHH :: MonadWidget t m 
  => Event t () 
  -> m (Event t (ReqResult () [HappyHour]))
(createHH :<|> updateHH :<|> deleteHH :<|> getHH :<|> queryHH) = apiClients

showReqResult :: Show a => ReqResult () a -> String
showReqResult result = case result of 
  ResponseSuccess _ a _ -> "Response success: " ++ show a
  ResponseFailure _ t _ -> "Response failure: " ++ show t
  RequestFailure _ t    -> "Request failure: " ++ show t