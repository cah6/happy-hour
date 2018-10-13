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
import Data.Time
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
      elAttr "link" ("href" =: "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.7.1/css/bulma.min.css" 
                  <> "rel" =: "stylesheet" 
                  <> "type" =: "text/css"
                  ) blank
      return ()

body :: forall t m. MonadWidget t m => m ()
body = mdo
  eHHs <- liftIO loadHHs
  let 
    baseUrl = BaseFullUrl Http "localhost" 3000 "/"
    queryHH :: Event t ()  -> m (Event t (ReqResult () [HappyHour]))
    (createHH :<|> updateHH :<|> deleteHH :<|> getHH :<|> queryHH) = 
      client hhApi (Proxy :: Proxy m) (Proxy :: Proxy ()) (constDyn baseUrl)
    init = case eHHs of 
      Right a -> a
      Left err -> [defaultHH]
  started <- getPostBuild
  eQueryResult <- queryHH started
  dHHs <- (holdDyn init $ handleResponse <$> eQueryResult)
  tabbedPanels dHHs

handleResponse :: ReqResult () [HappyHour] -> [HappyHour]
handleResponse result = case result of 
  ResponseSuccess _ xs _ -> xs
  _ -> []

tabbedPanels :: MonadWidget t m => Dynamic t [HappyHour] -> m ()
tabbedPanels xs = tabDisplay "tabs is-toggle is-fullwidth" "is-active" $ 
  M.fromList $ zip [1..] [("Search", searchTab xs), ("Create", createTab)] 


searchTab :: MonadWidget t m => Dynamic t [HappyHour] -> m ()
searchTab xs = elClass "div" "box" $ 
  elClass "table" "table is-bordered is-striped" $ do 
    el "thead" $ 
      el "tr" $ 
        mapM_ (elAttr "th" ("scope" =: "col") . text) cols
    _ <- dyn (mkTableBody <$> xs)
    return ()

createTab :: MonadWidget t m => m ()
createTab = elClass "div" "box" $ do
  restaurant <- horizontalInput "Restaurant name:"
  city <- horizontalInput "City name:"
  link <- horizontalInput "Link to description:"
  scheduleInput
  return ()

horizontalInput :: MonadWidget t m => T.Text -> m (TextInput t)
horizontalInput label = elAttr "div" ("class" =: "field is-horizontal") $ do
  elClass "div" "field-label is-normal" $ 
    elClass "label" "label" $
      text label
  elClass "div" "field-body" $ 
    textInput $ def { _textInputConfig_attributes = constDyn ("class" =: "control" )}

scheduleInput :: MonadWidget t m => m ()
scheduleInput = elAttr "div" ("class" =: "field is-horizontal") $ do
  elClass "div" "field-label is-normal" $ 
    elClass "label" "label" $
      text "Schedules"
  elClass "div" "field-body" $ 
    elClass "div" "tile is-ancestor" $ 
      elClass "div" "tile is-vertical is-4 is-parent" $ mdo 
        newBools <- foldDyn reduceScheduleCardEvent (0 =: True) (getScheduleCardEvent dynMapCardResult)
        dynMapCardResult <- listWithKey newBools singleScheduleCard
        return ()
  return ()

data ScheduleCardEvent = AddAnother | DeleteOne Int

getScheduleCardEvent :: (Reflex t, Ord k) => Dynamic t (M.Map k (Event t ScheduleCardEvent, a)) -> Event t ScheduleCardEvent
getScheduleCardEvent input = 
  let dynMapEvent = (fmap . fmap) fst input
      dynListEvent = fmap (fmap snd . M.toList) dynMapEvent
      dynEvent = leftmost <$> dynListEvent
  in  switchDyn dynEvent

reduceScheduleCardEvent :: ScheduleCardEvent -> M.Map Int Bool -> M.Map Int Bool
reduceScheduleCardEvent e xs = case e of
  AddAnother -> 
    const False <$> xs <> (M.size xs =: True)
  DeleteOne i ->
    M.delete i xs

singleScheduleCard :: MonadWidget t m => Int -> Dynamic t Bool -> m (Event t ScheduleCardEvent, Dynamic t Schedule)
singleScheduleCard num isLast = do
  (clicked, description) <- elClass "div" "message-header" $ do 
    description <- textInput $ def { _textInputConfig_attributes = constDyn $ 
            "class" =: "input has-background-grey-dark has-text-light" <> "type" =: "text" <> "placeholder" =: "Short description of deals" }
    (btn, _) <- elClass' "button" "delete" $ blank
    return $ (domEvent Click btn, _textInput_value description)
  (days, timeRange) <- elClass "div" "message-body" $ do
    days <- dayOfWeekBtns
    timeRange <- elClass "div" "field has-addons" $ do
      startTime <- timeSelect
      text " to "
      endTime <- timeSelect
      return $ TimeRange <$> zipDyn startTime endTime
    return (days, timeRange)
  let cardEvent = mapCardEvent num <$> tagPromptlyDyn isLast clicked
  return $ (AddAnother <$ clicked, Schedule <$> days <*> timeRange <*> description)

mapCardEvent :: Int -> Bool -> ScheduleCardEvent
mapCardEvent _ True = AddAnother
mapCardEvent i False = DeleteOne i

timeSelect :: MonadWidget t m => m (Dynamic t TimeOfDay)
timeSelect = 
  elClass "div" "field has-addons" $ do
    dynTimeOfDay <- timeOfDaySelect
    dynAmPm <- amPmSelect
    return dynTimeOfDay

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
dayOfWeekBtns = elClass "div" "buttons has-addons" $ do
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