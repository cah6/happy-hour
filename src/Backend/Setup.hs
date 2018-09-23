{-# LANGUAGE UndecidableInstances #-}
module Backend.Setup where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
-- import Control.Monad.Logger (LoggingT, MonadLogger(..), logInfoN, runStderrLoggingT, runStdoutLoggingT)
import Control.Monad.Logger
import qualified Data.ByteString.Lazy as B
import            Data.Text
import           Data.Aeson
import Data.String (IsString)
import Database.Beam
import Database.Beam.Postgres
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger       (withStdoutLogger)
import           Servant
import           System.IO

import Backend.Interfaces
import Backend.Server
import qualified Backend.Tables as DB
import Common.Dto
import Common.Routes

hhApi :: Proxy HappyHourApi
hhApi = Proxy

-- Define servant level settings and run server
run :: IO ()
run = do
  withStdoutLogger $ \apacheLogger -> do
    let port = 3000
        mainLoopF = hPutStrLn stderr ("listening on port " ++ show port)
        settings = (setPort port . setBeforeMainLoop mainLoopF . setLogger apacheLogger) defaultSettings
    runSettings settings mkApp

mkApp :: Application
mkApp = serve hhApi serverDefinition
    where
  serverDefinition = hoistServer hhApi nt server

-- My custom servant stack
newtype MyApp a = MyApp { runMyApp :: ExceptT ServantErr (LoggingT IO) a }
  deriving (Functor, Applicative, Monad, MonadLogger, MonadIO, MonadError ServantErr, 
            SaveHappyHour, GenUUID)

class (Monad m) => GetHappyHours m where
  getHappyHours :: m [HappyHour]

-- Custom stack -> predefined servant stack
nt :: MyApp a -> Handler a 
nt (MyApp m) = do
  res <- (liftIO . runStdoutLoggingT . runExceptT) m
  case res of
    Left e    -> throwError e
    Right res -> return res

server :: ServerT HappyHourApi MyApp
server = createHH :<|> getHH :<|> getAllHH