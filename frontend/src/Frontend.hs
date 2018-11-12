{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend where

import qualified Data.Text as T

import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Data.Bifunctor (first)
import Data.Monoid ((<>))
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom

import Common.Dto
import Common.Route
import CreateModal
import FrontendCommon
import ServantReflexClient

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do 
      el "title" $ text "Happy Hours"
      elAttr "link" ("href" =: "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css" 
                  <> "rel" =: "stylesheet" 
                  <> "type" =: "text/css"
                  ) blank
      return ()
  , _frontend_body = prerender (text "Loading...") body 
  }

body :: forall t m. MonadWidget t m => m ()
body = mdo
  started <- getPostBuild
  eQueryResult <- queryHH started
  dHHs <- holdDyn [defaultHH] eQueryResult
  eHappyHourCreated <- searchTab dHHs
  _ <- createHH eHappyHourCreated
  return ()

searchTab :: MonadWidget t m => Dynamic t [HappyHour] -> m (Event t HappyHour)
searchTab xs = elClass "div" "box" $ do
  eCreate <- b_button "Create new"
  filterVal <- _textInput_value <$> horizontalInput "Filter"
  dynMaybeHH <- removingModal eCreate createModal
  let flattenMaybe :: Reflex t => Maybe (Event t a) -> Event t a
      flattenMaybe Nothing  = never
      flattenMaybe (Just a) = a 
      flattened = switchDyn $ flattenMaybe <$> dynMaybeHH
  elClass "table" "table is-bordered" $ do 
    el "thead" $ 
      el "tr" $ 
        mapM_ (elAttr "th" ("scope" =: "col") . text) cols
    _ <- mkTableBody (filterHappyHours <$> xs <*> filterVal)
    return ()
  elAttr "iframe" (
        "width" =: "600"
    <> "height" =: "450"
    <> "frameborder" =: "0"
    <> "style" =: "border:0"
    <> "src" =: "https://www.google.com/maps/embed/v1/place?key=AIzaSyDxM3_sjDAP1kDHzbRMkZ6Ky7BYouXfVOs&q=place_id:ChIJMSuIlbnSJIgRbUFj__-VGdA&q=ChIJMUfEOWEtO4gRddDKWmgPMpI"
    ) blank
  return flattened

filterHappyHours :: [HappyHour] -> T.Text -> [HappyHour]
filterHappyHours xs currentTextInput = 
  let
    containsInput = T.isInfixOf currentTextInput
    anyScheduleContains :: [Schedule] -> Bool
    anyScheduleContains sx = any (containsInput . _scheduleDescription) sx
    filterSingle x = 
        containsInput (_restaurant x) 
     || containsInput (_city x)
     || anyScheduleContains (_schedule x)
  in
    filter filterSingle xs

mkTableBody :: MonadWidget t m => Dynamic t [HappyHour] -> m ()
mkTableBody xs = void $ el "tbody" (simpleList xs mkRow)

-- mkBuiltInTable :: MonadWidget t m => Dynamic t [HappyHour] -> m ()
-- mkBuiltInTable dynHHs =
--   let
--     dRowsList = zip [(1 :: Integer)..] <$> dynHHs
--     dRows = M.fromList <$> dRowsList
--     dCols = M.fromList $ zip cols mkRow
--   in 
--     tableDynAttr "td" dCols dRows undefined >> return ()

cols :: [T.Text]
cols = ["Restaurant", "City", "Time", "Description", "Action"]

mkRow :: MonadWidget t m
      => Dynamic t HappyHour
      -> m ()
mkRow dHHs = simpleList (_schedule <$> dHHs) (\schedule ->
  let
    mkLinkAttrs hh = ("href" =: _link hh)
    c1 = elDynAttr "a" (mkLinkAttrs <$> dHHs) (dynText (_restaurant <$> dHHs))
    c2 = dynText $ _city <$> dHHs
    c3 = dynText $ times <$> schedule
    c4 = dynText $ _scheduleDescription <$> schedule
  in 
    row [c1, c2, c3, c4]) >> return ()

times :: Schedule -> T.Text
times Schedule{ _days, _time } = 
  let 
    days = printDays _days
    time = printTimeRange _time
  in 
    days <> ", " <> time

flattenHH :: HappyHour -> [HappyHour]
flattenHH HappyHour{_schedule, ..} = map (\s -> HappyHour {_schedule = [s], .. } ) $ _schedule

row :: MonadWidget t m
  => [m a]
  -> m ()
row xs = el "tr" $ mapM_ (el "td") xs

removingModal
  :: MonadWidget t m
  => Event t a
  -- ^ Event to open the model
  -> (a -> m (b, Event t ()))
  -- ^ Widget rendering the body of the modal.  Returns an event with a
  -- success value and an event triggering the close of the modal.
  -> m (Dynamic t (Maybe b))
removingModal showm modalBody = do
    rec let visE = leftmost [Just <$> showm, Nothing <$ closem]
        res <- widgetHold (removeFromDOMWrapper Nothing) (removeFromDOMWrapper <$> visE)
        let resE = fst <$> res
            closem = switchDyn $ snd <$> res
    return resE
  where
    removeFromDOMWrapper Nothing = return (Nothing, never)
    removeFromDOMWrapper (Just a) =
      elClass "div" "modal is-active" $
        first Just <$> modalBody a

-- Utils

putDebugLnE :: MonadWidget t m => Event t a -> (a -> String) -> m ()
putDebugLnE e mkStr = do
    performEvent_ (liftIO . putStrLn . mkStr <$> e)
