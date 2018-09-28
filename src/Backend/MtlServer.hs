module Backend.MtlServer where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy.Builder as B

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (MonadLogger(..), logInfoN)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Aeson.Encode.Pretty (encodePrettyToTextBuilder)
import Data.Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.UUID
import Servant

-- temp
import Control.Lens.Operators
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy      as BL
import Database.V5.Bloodhound.Types
import Network.HTTP.Client (responseBody)

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
  logInfoN $ "Getting happy hour with id [" <> toText uuid <> "]..."
  reply <- getHappyHour uuid
  case decode (responseBody reply) >>= parseMaybe parseHH of 
    Nothing -> throwError err404
    Just a  -> return a

parseHH :: Value -> Parser DTO.HappyHour
parseHH = withObject "Expected object from Elasticsearch" (.: "_source")

queryHHs :: (MonadLogger m, MonadError ServantErr m, MonadCrudHappyHour m) 
  => m [DTO.HappyHour]
queryHHs = do 
  logInfoN "Getting all happy hours..."
  throwError err400
  return []
