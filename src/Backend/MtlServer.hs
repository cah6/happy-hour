{-# LANGUAGE DeriveAnyClass #-}
module Backend.MtlServer where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Builder as B

import Control.Applicative
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (MonadLogger(..), logInfoN)
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.UUID hiding (fromString)
import Servant

-- temp
import Control.Lens.Operators
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy      as BL
import Database.V5.Bloodhound.Types
import Network.HTTP.Client (responseBody)
import Data.Aeson.Lens as Lens
import GHC.Generics

import Backend.Interfaces
import Backend.Tables as DB
import Common.Dto as DTO

-- MTL style definitions of the server go in this file. As MTL is a different
-- "layer" than easily testable pure functions, those should go in a different
-- file.

createHH :: (MonadLogger m, MonadError ServantErr m, MonadCrudHappyHour m, GenUUID m)
  => DTO.HappyHour 
  -> m UUID
createHH hh = do
  _     <- logInfoN $ "POST called with data:\n" <> (toStrict . toLazyText . encodePrettyToTextBuilder) hh 
  uuid  <- genUUID
  _     <- upsertHappyHour uuid hh
  return uuid

updateHH :: (MonadLogger m, MonadError ServantErr m, MonadCrudHappyHour m)
  => UUID
  -> DTO.HappyHour
  -> m ()
updateHH uuid hh = do
  _     <- logInfoN $ "PUT called with id [" <> toText uuid <> "] and data:\n" <> (toStrict . toLazyText . encodePrettyToTextBuilder) hh 
  _     <- upsertHappyHour uuid hh
  return ()

deleteHH :: (MonadLogger m, MonadError ServantErr m, MonadCrudHappyHour m)
  => UUID
  -> m ()
deleteHH uuid = do
  _     <- logInfoN $ "DELETE called with id [" <> toText uuid <> "]"
  deleteHappyHour uuid 
  return ()

getHH :: (MonadLogger m, MonadError ServantErr m, MonadCrudHappyHour m) 
  => UUID 
  -> m DTO.HappyHour
getHH uuid = do 
  logInfoN $ "GET called with id [" <> toText uuid <> "]..."
  reply <- getHappyHour uuid
  handleSearchResponse $ parseSingleDocResponse (responseBody reply)

queryHHs :: (MonadLogger m, MonadError ServantErr m, MonadCrudHappyHour m) 
  => m [DTO.HappyHour]
queryHHs = do 
  logInfoN $ "GET called with input..."
  reply <- queryHappyHours QueryParams
  handleSearchResponse $ parseMultiDocResponse (responseBody reply)

parseSingleDocResponse :: BL.ByteString -> SearchResponse DTO.HappyHour
parseSingleDocResponse bs = case eitherDecode bs of 
  (Left err)          -> ParserError (pack err)
  (Right esResponse)  -> case esResponse of
    (MyEsError (EsError{..})) -> EsJsonError errorMessage
    (EsSuccess (EsResult{..})) -> case foundResult of 
      Nothing                           -> SourceNotFound
      Just (EsResultFound version doc)  -> Entity doc
    
parseMultiDocResponse :: BL.ByteString -> SearchResponse [DTO.HappyHour]
parseMultiDocResponse bs = case eitherDecode bs of 
  (Left err)          -> ParserError (pack err)
  (Right esResponse)  -> case esResponse of
    (MyEsError (EsError{..}))       -> EsJsonError errorMessage
    (EsSuccess (SearchResult{..}))  -> Entity $ catMaybes (hitSource <$> hits searchHits)
    
logEsReply :: MonadLogger m => Reply -> m ()
logEsReply r = logInfoN text >>= \_ -> return () where 
  text = (decodeUtf8 . BL.toStrict . responseBody) r

data SearchResponse a = 
    Entity a
  | ParserError Text
  | SourceNotFound
  | EsJsonError Text

handleSearchResponse :: (MonadError ServantErr m) => SearchResponse a -> m a
handleSearchResponse r = case r of
  Entity a        -> return a
  ParserError e   -> throwError $ err500 { errBody = (fromString . show) e }
  SourceNotFound  -> throwError err404
  EsJsonError e   -> throwError $ err500 { errBody = "Database responded with: "<> (fromString . show) e }

data EsResponse a = EsSuccess a | MyEsError EsError

instance FromJSON a => FromJSON (EsResponse a) where 
  parseJSON v = (EsSuccess <$> parseJSON v) <|> (MyEsError <$> parseJSON v)