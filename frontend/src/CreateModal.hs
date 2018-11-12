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
module CreateModal(createModal) where

import qualified Data.Map.Lazy as M
import qualified Data.Text as T

import Control.Monad (join)
import Data.Time
import Reflex.Dom

import Common.Dto
import FrontendCommon

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

createFields :: MonadWidget t m 
  => m (Dynamic t HappyHour)
createFields = elClass "div" "box" $ do
  restaurant <- _textInput_value <$> horizontalInput "Restaurant name:"
  city <- _textInput_value <$> horizontalInput "City name:"
  linkVal <- _textInput_value <$> horizontalInput "Link to description:"
  dynSchedules <- scheduleInput
  return $ HappyHour <$> city <*> restaurant <*> dynSchedules <*> linkVal

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

getScheduleCardEvent :: Reflex t
  => Dynamic t (M.Map k (Event t ScheduleCardEvent, a)) 
  -> Event t ScheduleCardEvent
getScheduleCardEvent input = 
  let dynMapEvent = (fmap . fmap) fst input
      dynListEvent = fmap (fmap snd . M.toList) dynMapEvent
      dynEvent = leftmost <$> dynListEvent
  in  switchDyn dynEvent

getScheduleCardSchedules :: Reflex t => M.Map k (a, Dynamic t Schedule) -> Dynamic t [Schedule]
getScheduleCardSchedules input = sequence $ (snd . snd) <$> M.toList input

reduceScheduleCardEvent :: ScheduleCardEvent -> M.Map Int Bool -> M.Map Int Bool
reduceScheduleCardEvent e xs = case e of
  AddAnother -> 
    let foldF key _ acc = max key acc
        maxKey = M.foldrWithKey foldF (negate (1 :: Int)) xs
        newKey = maxKey + 1
    in  (const False <$> xs) <> (newKey =: True)
  DeleteOne i ->
    M.delete i xs

singleScheduleCard :: MonadWidget t m => Int -> Dynamic t Bool -> m (Event t ScheduleCardEvent, Dynamic t Schedule)
singleScheduleCard num _ = elClass "div" "tile is-child message" $ do
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

dayOfWeekBtns :: MonadWidget t m => m (Dynamic t [DayOfWeek])
dayOfWeekBtns = elClass "div" "column is-full" $ elClass "div" "buttons has-addons is-centered" $ do
  days <- mapM singleDayBtn [Sunday .. Saturday]
  return (mapBtnState <$> sequence days)

singleDayBtn :: (MonadWidget t m) 
  => DayOfWeek 
  -> m (Dynamic t (Bool, DayOfWeek))
singleDayBtn dow = mdo
  (btn, _) <- elDynAttr' "span" (singleDayBtnAttrs <$> isActive) $ text (printDay dow)
  isActive <- toggle False (domEvent Click btn)
  return $ (\bool -> (bool, dow)) <$> isActive

mapBtnState :: [(Bool, DayOfWeek)] -> [DayOfWeek]
mapBtnState = map snd . filter (\tuple -> fst tuple == True)

singleDayBtnAttrs :: Bool -> M.Map T.Text T.Text
singleDayBtnAttrs isSelected = if isSelected
  then
    "class" =: "button is-active is-selected"
  else 
    "class" =: "button"

-- Time selector functions

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