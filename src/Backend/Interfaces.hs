{-# LANGUAGE DefaultSignatures #-}
module Backend.Interfaces where

import Control.Monad.Except (ExceptT)
import Control.Monad.Logger (LoggingT)
import Control.Monad.Trans (MonadTrans(..))
import Database.Beam
import Database.Beam.Postgres
import Data.UUID
import Data.UUID.V4 (nextRandom)

import Backend.Tables as DB

-- This file defines my "custom effects". The general procedure for this is:
-- 1. Define the monad
-- 2. Give it a default way to thread itself through transformer stack
-- 3. Generate an instance for each transformer in the stack
-- 4. Give it an IO instance that it will use for production. This should be as
--    short and passthrough-y as possible.

-- GenUUID section
class (Monad m) => GenUUID m where
  genUUID :: m UUID

  default genUUID :: (MonadTrans t, GenUUID m', m ~ t m') => m UUID
  genUUID = lift genUUID 

instance GenUUID m => GenUUID (LoggingT m)
instance GenUUID m => GenUUID (ExceptT e m)

instance GenUUID IO where
  genUUID = nextRandom

-- SaveHappyHour section
class (Monad m) => SaveHappyHour m where
  createHappyHour :: HappyHour -> m ()
  
  default createHappyHour :: (MonadTrans t, SaveHappyHour m', m ~ t m') => HappyHour -> m ()
  createHappyHour = lift . createHappyHour

instance SaveHappyHour m => SaveHappyHour (LoggingT m)
instance SaveHappyHour m => SaveHappyHour (ExceptT e m)

instance SaveHappyHour IO where
  createHappyHour hh = do
    conn <- liftIO $ connect defaultConnectInfo { connectDatabase = "happyhour1", connectUser = "" }
    _ <- liftIO $ runBeamPostgres conn $ runInsert $ 
      insert (DB._tableHappyHour DB.happyHourDb) $ 
      insertValues [hh]
    return ()