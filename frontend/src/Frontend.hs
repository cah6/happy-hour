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

import Control.Monad (forM_, join, liftM)
import Control.Monad.Trans (liftIO)
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Data.Bifunctor (first)
import qualified Data.Map.Lazy as M
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Time
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Generated.Static
import Reflex.Dom 
import Reflex.Dom.Core

import Data.UUID
import Data.UUID.V4
import Reflex.Dom 
import Servant.API

import Common.Dto
import Common.Route
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
  eRecentlyCreated <- createHH eHappyHourCreated
  return ()

searchTab :: MonadWidget t m => Dynamic t [HappyHour] -> m (Event t HappyHour)
searchTab xs = elClass "div" "box" $ do
  eCreate <- b_button "Create new"
  dynMaybeHH <- removingModal eCreate createModal
  let flattenMaybe :: (Reflex t, Show a) => Maybe (Event t a) -> Event t a
      flattenMaybe Nothing  = never
      flattenMaybe (Just a) = a 
      flattened = switchDyn $ flattenMaybe <$> dynMaybeHH
  elClass "table" "table is-bordered is-striped" $ do 
    el "thead" $ 
      el "tr" $ 
        mapM_ (elAttr "th" ("scope" =: "col") . text) cols
    _ <- dyn (mkTableBody <$> xs)
    return ()
  elAttr "iframe" (
        "width" =: "600"
    <> "height" =: "450"
    <> "frameborder" =: "0"
    <> "style" =: "border:0"
    <> "src" =: "https://www.google.com/maps/embed/v1/place?key=AIzaSyDxM3_sjDAP1kDHzbRMkZ6Ky7BYouXfVOs&q=place_id:ChIJMSuIlbnSJIgRbUFj__-VGdA&q=ChIJMUfEOWEtO4gRddDKWmgPMpI"
    ) blank
  return flattened

createModal :: MonadWidget t m => () -> m (Event t HappyHour, Event t ())
createModal _ = do
  elClass "div" "modal-background" $ blank
  elClass "div" "modal-card" $ do
    eClose <- elClass "header" "modal-card-head" $ do
      elClass "p" "modal-card-title" $ text "Create a happy hour"
      b_delete
    dynHappyHour <- elClass "section" "modal-card-body" $ createFields
    (eSubmit, eCancel) <- elClass "footer" "modal-card-foot" $ do 
      eSubmit <- b_button "Submit"
      eCancel <- b_button "Cancel"
      return (eSubmit, eCancel)
    return (tagPromptlyDyn dynHappyHour eSubmit, leftmost [eClose, eCancel, eSubmit])

createTab :: MonadWidget t m => m ()
createTab = createFields >>= \_ -> return ()

createFields :: MonadWidget t m 
  => m (Dynamic t HappyHour)
createFields = elClass "div" "box" $ do
  restaurant <- _textInput_value <$> horizontalInput "Restaurant name:"
  city <- _textInput_value <$> horizontalInput "City name:"
  link <- _textInput_value <$> horizontalInput "Link to description:"
  dynSchedules <- scheduleInput
  return $ HappyHour <$> city <*> restaurant <*> dynSchedules <*> link

horizontalInput :: MonadWidget t m => T.Text -> m (TextInput t)
horizontalInput label = elAttr "div" ("class" =: "field") $ do
  elClass "label" "label" $ text label
  textInput $ def { _textInputConfig_attributes = constDyn ("class" =: "control" )}

scheduleInput :: MonadWidget t m => m (Dynamic t [Schedule])
scheduleInput = 
  elClass "div" "tile is-ancestor" $ 
    elClass "div" "tile is-vertical is-10 is-parent" $ mdo 
      let eAdd = AddAnother <$ domEvent Click btnAddAnother
          eScheduleChanged = leftmost $ [getScheduleCardEvent dynMapCardResult] ++ [eAdd]
      newBools <- foldDyn reduceScheduleCardEvent (0 =: True) eScheduleChanged
      dynMapCardResult <- listWithKey newBools singleScheduleCard
      (btnAddAnother, _)  <- elClass' "button" "button" $ text "Add another"
      let dynSchedules = join $ getScheduleCardSchedules <$> dynMapCardResult
      return dynSchedules

data ScheduleCardEvent = AddAnother | DeleteOne Int

getScheduleCardEvent :: (Reflex t, Ord k) => Dynamic t (M.Map k (Event t ScheduleCardEvent, a)) -> Event t ScheduleCardEvent
getScheduleCardEvent input = 
  let dynMapEvent = (fmap . fmap) fst input
      dynListEvent = fmap (fmap snd . M.toList) dynMapEvent
      dynEvent = leftmost <$> dynListEvent
  in  switchDyn dynEvent

getScheduleCardSchedules :: Reflex t => (Ord k) => M.Map k (a, Dynamic t Schedule) -> Dynamic t [Schedule]
getScheduleCardSchedules input = sequence $ (snd . snd) <$> M.toList input

reduceScheduleCardEvent :: ScheduleCardEvent -> M.Map Int Bool -> M.Map Int Bool
reduceScheduleCardEvent e xs = case e of
  AddAnother -> 
    let foldF key val acc = max key acc
        maxKey = M.foldrWithKey foldF (negate (1 :: Int)) xs
        newKey = maxKey + 1
    in  (const False <$> xs) <> (newKey =: True)
  DeleteOne i ->
    M.delete i xs

singleScheduleCard :: MonadWidget t m => Int -> Dynamic t Bool -> m (Event t ScheduleCardEvent, Dynamic t Schedule)
singleScheduleCard num isLast = elClass "div" "tile is-child message" $ do
  clicked <- elClass "div" "message-header" $ do 
    text $ "Schedule " <> (T.pack . show) (num + 1)
    (btn, _) <- elClass' "button" "delete" $ blank
    return $ domEvent Click btn
  schedule <- elClass "div" "message-body" $ elClass "div" "columns is-multiline" $ do
    days <- dayOfWeekBtns
    timeRange <- elClass "div" "field has-addons" $ do
      startTime <- elClass "div" "column is-narrow" timeSelect
      elClass "div" "column" $ text "to"
      endTime <- elClass "div" "column is-narrow" timeSelect
      return $ TimeRange <$> zipDyn startTime endTime
    description <- elClass "div" "column is-full" $ textInput $ def { _textInputConfig_attributes = constDyn $ 
        "class" =: "input" <> "type" =: "text" <> "placeholder" =: "Short description of deals" }
    return $ Schedule <$> days <*> timeRange <*> _textInput_value description
  return $ (DeleteOne num <$ clicked, schedule)

timeSelect :: MonadWidget t m => m (Dynamic t TimeOfDay)
timeSelect = 
  elClass "div" "field has-addons" $ do
    dynTimeOfDay <- timeOfDaySelect
    dynAmPm <- amPmSelect
    return $ zipDynWith adjustTime dynAmPm dynTimeOfDay

adjustTime :: AmPm -> TimeOfDay -> TimeOfDay
adjustTime AM tod = tod
adjustTime PM (TimeOfDay h m s) = TimeOfDay (h + 12) m s

timeOfDaySelect :: MonadWidget t m => m (Dynamic t TimeOfDay)
timeOfDaySelect = 
  elClass "div" "control" $ 
    elClass "span" "select" $ do
      selected <- dropdown (TimeOfDay 4 0 0) (constDyn timeOptions) def
      return (_dropdown_value selected)

amPmSelect :: MonadWidget t m => m (Dynamic t AmPm)
amPmSelect = 
  elClass "p" "control" $ 
    elClass "span" "select" $ do
      selected <- dropdown PM (constDyn amPmMap) def
      return (_dropdown_value selected)

data AmPm = AM | PM
  deriving (Eq, Ord, Read)

timeOptions :: M.Map TimeOfDay T.Text
timeOptions =
  let 
    startUtc = UTCTime (ModifiedJulianDay 0) 0
    transform = 
      -- pair TimeOfDay with its shown form
        (\timeOfDay -> (timeOfDay, printTimeOfDayNoMod timeOfDay))
      -- convert from UTCTime to TimeOfDay
      . (\utcTime -> localTimeOfDay (utcToLocalTime utc utcTime))
      -- convert to an offset UTCTime
      . (\seconds -> addUTCTime (fromInteger seconds) startUtc)
      -- convert to seconds
      . (\minutes -> minutes * 60)
  in 
    M.fromList $ transform <$> [0, 30 .. 30*23]

amPmMap :: M.Map AmPm T.Text
amPmMap = M.fromList [(AM, "am"), (PM, "pm")]

dayOfWeekBtns :: MonadWidget t m => m (Dynamic t [DayOfWeek])
dayOfWeekBtns = elClass "div" "column is-full" $ elClass "div" "buttons has-addons is-centered" $ do
  days <- mapM singleDayBtn [Sunday .. Saturday]
  return (mapBtnState <$> sequence days)

singleDayBtn :: (MonadWidget t m, MonadSample t m) 
  => DayOfWeek 
  -> m (Dynamic t (Bool, DayOfWeek))
singleDayBtn dow = mdo
  (btn, _) <- elDynAttr' "span" (singleDayBtnAttrs <$> isActive) $ text (printDay dow)
  isActive <- toggle False (domEvent Click btn)
  return $ (\bool -> (bool, dow)) <$> isActive

mapBtnState :: [(Bool, DayOfWeek)] -> [DayOfWeek]
mapBtnState = map snd . filter (\tuple -> fst tuple == True)

singleDayBtnAttrs isSelected = if isSelected
  then
    "class" =: "button is-active is-selected"
  else 
    "class" =: "button"

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

row :: MonadWidget t m
  => [m a]
  -> m ()
row xs = el "tr" $ mapM_ (el "td") xs

b_button :: MonadWidget t m => T.Text -> m (Event t ())
b_button s = do
  (e, _) <- elClass' "button" "button" $ text s
  return $ domEvent Click e

b_delete :: MonadWidget t m => m (Event t ())
b_delete = do 
  (e, _) <- elClass' "button" "delete" $ blank
  return $ domEvent Click e

removingModal
  :: MonadWidget t m
  => Event t a
  -- ^ Event to open the model
  -> (a -> m (b, Event t ()))
  -- ^ Widget rendering the body of the modal.  Returns an event with a
  -- success value and an event triggering the close of the modal.
  -> m (Dynamic t (Maybe b))
removingModal showm body = do
    rec let visE = leftmost [Just <$> showm, Nothing <$ closem]
        res <- widgetHold (removeFromDOMWrapper Nothing) (removeFromDOMWrapper <$> visE)
        let resE = fst <$> res
            closem = switchDyn $ snd <$> res
    return resE
  where
    removeFromDOMWrapper Nothing = return (Nothing, never)
    removeFromDOMWrapper (Just a) =
      elClass "div" "modal is-active" $
        first Just <$> body a

-- Utils

putDebugLnE :: MonadWidget t m => Event t a -> (a -> String) -> m ()
putDebugLnE e mkStr = do
    performEvent_ (liftIO . putStrLn . mkStr <$> e)
