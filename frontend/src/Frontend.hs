{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend where

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import qualified Data.Map.Lazy as M
import Data.Monoid ((<>))
import qualified Data.Text as T
import Reflex.Dom 
import Reflex.Dom.Core

import Common.Dto
import Static

import Common.Routes
import Data.Proxy
import Data.UUID
import Reflex.Dom 
import Servant.API
import Servant.Reflex
-- import Servant.Reflex.BaseUrl

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = do
      el "title" $ text "Happy Hours"
      elAttr "link" ("href" =: "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" 
                  <> "rel" =: "stylesheet" 
                  <> "type" =: "text/css") $ return ()

body :: forall t m. MonadWidget t m => m ()
body = mdo -- had as "panel $ grid $ mdo" before
  let 
    baseUrl = BaseFullUrl Http "localhost" 3000 "/"
    queryHH :: Event t ()  -> m (Event t (ReqResult () [HappyHour]))
    (createHH :<|> updateHH :<|> deleteHH :<|> getHH :<|> queryHH) = 
      client hhApi (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
    init = [ defaultHH ]
  eHHs <- liftIO loadHHs
  started <- getPostBuild
  eQueryResult <- queryHH started
  dHHs <- (holdDyn init $ handleResponse <$> eQueryResult)
  tabbedPanels dHHs

  
  -- case (1 @> queryResult) of
  --   ResponseSuccess _ hh _ -> tabbedPanels hh
  -- case eHHs of
  --   (Left error) -> text $ "Could not load list due to: " <> T.pack error
  --   (Right res)  -> tabbedPanels (constDyn res)

handleResponse :: ReqResult () [HappyHour] -> [HappyHour]
handleResponse result = case result of 
  ResponseSuccess _ xs _ -> xs
  _ -> []


tabbedPanels :: MonadWidget t m => Dynamic t [HappyHour] -> m ()
tabbedPanels xs = tabDisplay "nav nav-tabs" "active" $ 
  M.fromList $ zip [1..] [("Search", searchTab xs), ("Create", blank)] 
  
createTab :: MonadWidget t m => m ()
createTab = blank

searchTab :: MonadWidget t m => Dynamic t [HappyHour] -> m ()
searchTab xs = panel $ 
  -- same as "elClass table table"
  elAttr "table" ("class" =: "table table-striped table-bordered") $ do 
    el "thead" $ 
      el "tr" $ 
        mapM_ (elAttr "th" ("scope" =: "col") . text) cols
    _ <- dyn (mkTableBody <$> xs)
    return ()
    -- el "tbody" 
    --   $ mapM_ mkRow xs 

mkTableBody :: MonadWidget t m => [HappyHour] -> m ()
mkTableBody xs = el "tbody" 
      $ mapM_ mkRow xs 

cols :: [T.Text]
cols = ["Restaurant", "City", "Time", "Description"]

mkRow :: MonadWidget t m 
       => HappyHour
       -> m ()
mkRow hh = forM_ (_schedule hh) $ \schedule ->
  let
    c1 = elAttr "a" ("href" =: _link hh) (text (_restaurant hh))
    c2 = text $ _city hh
    c3 = text $ times schedule
    c4 = text $ _scheduleDescription schedule
  in 
    row [c1, c2, c3, c4]

times :: Schedule -> T.Text
times Schedule{ _days, _time } = 
  let 
    days = printDays _days
    time = printTimeRange _time
  in 
    days <> ", " <> time

flattenHH :: HappyHour -> [HappyHour]
flattenHH hh@HappyHour{_schedule, ..} = map (\s -> HappyHour {_schedule = [s], .. } ) $ _schedule

grid ::
  MonadWidget t m =>
  m a ->
  m a
grid =
  elClass "div" "container"

row :: MonadWidget t m
  => [m a]
  -> m ()
row xs = el "tr" $ mapM_ (el "td") xs

panel ::
  MonadWidget t m =>
  m a ->
  m a
panel =
  divClass "panel panel-default" . divClass "panel-body"