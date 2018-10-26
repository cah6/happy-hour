{-# LANGUAGE AllowAmbiguousTypes #-}
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

module ServantReflexClient
  ( createHH
  , queryHH
  )
  where

import Data.UUID
import Reflex.Dom
import qualified Data.Text as T
import Servant.API
import Servant.Reflex
import Data.Proxy

import Common.Dto
import Common.ServantRoutes

apiClients :: forall t m. (MonadWidget t m) => _
apiClients = client hhApi (Proxy @m) (Proxy @()) (constDyn url)
  where url :: BaseUrl
        url = BaseFullUrl Http "52.87.157.165" 3000 "/"

genCreateHH :: MonadWidget t m 
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
genQueryHH :: MonadWidget t m 
  => Event t () 
  -> m (Event t (ReqResult () [HappyHour]))
(genCreateHH :<|> updateHH :<|> deleteHH :<|> getHH :<|> genQueryHH) = apiClients

createHH :: MonadWidget t m 
  => Event t (HappyHour)
  -> m (Event t ())
createHH eHH = do 
  dHH <- holdDyn defaultHH eHH
  eCreateResult <- genCreateHH (Right <$> dHH) (() <$ eHH)
  return $ () <$ eCreateResult

queryHH :: MonadWidget t m 
  => Event t ()
  -> m (Event t [HappyHour])
queryHH e = do
  eReqResult <- genQueryHH e
  return $ valueOrEmpty <$> eReqResult

valueOrEmpty :: ReqResult () [a] -> [a]
valueOrEmpty result = case result of
  ResponseSuccess _ xs _ -> xs
  _ -> []

showReqResult :: Show a => ReqResult () a -> String
showReqResult result = case result of 
  ResponseSuccess _ a _ -> "Response success: " ++ show a
  ResponseFailure _ t _ -> "Response failure: " ++ show t
  RequestFailure _ t    -> "Request failure: " ++ show t