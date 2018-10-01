{-# LANGUAGE UndecidableInstances #-}
module Backend.Setup where

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger
import Database.V5.Bloodhound.Client
import Database.V5.Bloodhound.Types
import Data.Text
import Data.Aeson
import Data.String (IsString)
import Database.Beam
import Database.Beam.Postgres
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Logger (withStdoutLogger)
import Servant
import System.IO


import Backend.Interfaces
import Backend.MtlServer
import qualified Backend.Tables as DB
import Common.Dto
import Common.Routes

-- Define servant level settings and run server
run :: IO ()
run = do
  withStdoutLogger $ \apacheLogger -> do
    let port = 3000
        mainLoopF = hPutStrLn stderr ("listening on port " ++ show port)
        settings = (setPort port . setBeforeMainLoop mainLoopF . setLogger apacheLogger) defaultSettings
    runSettings settings mkApp

mkApp :: Application
mkApp = simpleCors $ serve hhApi serverDefinition
    where
  serverDefinition = hoistServer hhApi nt server

-- My custom servant stack
newtype MyApp a = MyApp { runMyApp :: BH (ExceptT ServantErr (LoggingT IO)) a }
  deriving (Functor, Applicative, Monad, MonadLogger, MonadIO, MonadError ServantErr, 
            MonadCrudHappyHour, GenUUID, MonadBH)

instance MonadBH IO where
  getBHEnv = liftIO getBHEnv

instance MonadBH m => MonadBH (ExceptT e m) where
  getBHEnv = lift getBHEnv

instance MonadBH m => MonadBH (LoggingT m) where
  getBHEnv = lift getBHEnv

instance MonadLogger m => MonadLogger (BH m)
instance GenUUID m => GenUUID (BH m)

-- Custom stack -> predefined servant Handler stack
nt :: MyApp a -> Handler a
nt (MyApp m) = do
  bhEnv <- liftIO provideEnv
  res <- (liftIO . runStdoutLoggingT . runExceptT . runBH bhEnv) m
  case res of
    Left e    -> throwError e
    Right res -> return res

provideEnv :: IO BHEnv
provideEnv = do
  manager <- newManager defaultManagerSettings
  return $ mkBHEnv (Server "http://localhost:9200") manager

server :: ServerT HappyHourApi MyApp
server = createHH :<|> updateHH :<|> deleteHH :<|> getHH :<|> queryHHs