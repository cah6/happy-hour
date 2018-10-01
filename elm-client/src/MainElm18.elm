import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Data.HappyHour exposing (..)
import Data.Schedule exposing (..)
import Request.Default exposing (..)
import Table as Table


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { tableEntries : List HappyHour
  , toSend : HappyHour
  , errorMessage : String
  , tableState : Table.State
  }

setTableEntries : List HappyHour -> Model -> Model
setTableEntries xs model =
  { model | tableEntries = xs }

asTableEntriesIn : Model -> List HappyHour -> Model
asTableEntriesIn model xs =
  setTableEntries xs model

setToSend : HappyHour -> Model -> Model
setToSend x model =
  { model | toSend = x }

asToSendIn : Model -> HappyHour -> Model
asToSendIn model x =
  setToSend x model

setErrorMessage : String -> Model -> Model
setErrorMessage err model = 
  { model | errorMessage = err }

init : (Model, Cmd Msg)
init =
  let
    model = 
      { tableEntries = [HappyHour "Detroit" "Hopcat" [] "link here"]
      , toSend = emptyHH
      , errorMessage = ""
      , tableState = Table.initialSort "Restaurant"
      }
  in 
    ( model, getHappyHours )

emptyHH : HappyHour
emptyHH = 
  { city = ""
  , restaurant = ""
  , schedule = []
  , link = ""
  }

setCity : String -> HappyHour -> HappyHour
setCity x hh = 
  { hh | city = x }

setRestaurant : String -> HappyHour -> HappyHour
setRestaurant x hh = 
  { hh | restaurant = x }

setSchedule : List Schedule -> HappyHour -> HappyHour
setSchedule xs hh = 
  { hh | schedule = xs }

setLink : String -> HappyHour -> HappyHour
setLink x hh = 
  { hh | link = x }

-- UPDATE


type Msg
  = City String
  | Restaurant String
  | Link String
  | GetData
  | SearchResponse (Result Http.Error (List HappyHour))
  | SetTableState Table.State


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    City city ->
      (  model.toSend 
      |> setCity city 
      |> asToSendIn model
      , Cmd.none)

    Restaurant restaurant ->
      (  model.toSend 
      |> setRestaurant restaurant 
      |> asToSendIn model
      , Cmd.none)

    Link link ->
      (  model.toSend 
      |> setLink link 
      |> asToSendIn model
      , Cmd.none)

    GetData ->
      ( model
      , getHappyHours)

    SearchResponse result ->
      case result of
        Ok xs -> 
          (  model 
          |> setTableEntries xs
          , Cmd.none
          )
        
        Err e -> case e of
          Http.BadUrl errMsg  -> ( setErrorMessage errMsg model, Cmd.none )
          Http.Timeout        -> ( setErrorMessage "timeout" model, Cmd.none )
          Http.NetworkError   -> ( setErrorMessage "network error" model, Cmd.none )
          Http.BadStatus _    -> ( setErrorMessage "bad status" model, Cmd.none )
          Http.BadPayload errMsg r -> ( setErrorMessage errMsg model, Cmd.none )

    SetTableState state -> 
      ( { model | tableState = state }
      , Cmd.none
      )


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ type_ "text", placeholder "City name...", value model.toSend.city, onInput City ] []
    , input [ type_ "text", placeholder "Restaurant name...", value model.toSend.restaurant, onInput Restaurant ] []
    , input [ type_ "text", placeholder "Link to info...", value model.toSend.link, onInput Link ] []
    , button [ onClick GetData ] [ text "More Please!" ] 
    --, Table.view tableConfig model.tableState model.tableEntries 
    , mkTable model.tableEntries
    ]

-- tableConfig : Table.Config HappyHour Msg
-- tableConfig = 
--   Table.customConfig
--     { toId = \hh -> hh.city ++ hh.restaurant
--     , toMsg = SetTableState
--     , columns = 
--       [ Table.stringColumn "Restaurant" .restaurant
--       , Table.stringColumn "City" .city
--       , Table.stringColumn "Time" (\_ -> "")
--       , Table.stringColumn "Description" .link
--       ]
--     , customizations = myCustomizations
--     }

-- myCustomizations : Table.Customizations d m
-- myCustomizations =
--   let 
--     oldCustomization = Table.defaultCustomizations
--   in 
--     { oldCustomization 
--     | tableAttrs = [ class "table table-striped table-bordered" ] }

mkTable : List HappyHour -> Html Msg
mkTable xs = 
  table [ class "table table-hover table-bordered" ] [ mkHeaderRow,  mkTableRows xs ]

mkHeaderRow : Html Msg
mkHeaderRow = thead [] (List.map mkHeader cols)

mkHeader : String -> Html Msg
mkHeader name = 
  th [ scope "col" ] [text name]

cols : List String
cols =
  [ "Restaurant"
  , "City"
  , "Time"
  , "Description"
  ]

mkTableRows : List HappyHour -> Html Msg
mkTableRows xs =
  tbody [] (List.concatMap mkTableRow xs)

mkTableRow : HappyHour -> List (Html Msg)
mkTableRow hh =
  let
    spanVertical = rowspan (List.length hh.schedule)
    tdRestaurant = td [scope "row", spanVertical] [a [ href hh.link ] [ text hh.restaurant ]]
    tdCity = td [scope "row", spanVertical] [text hh.city]
    mapSchedules : Int -> Schedule -> Html Msg
    mapSchedules i schedule = case i of 
      0 -> tr [] ([tdRestaurant, tdCity] ++ scheduleToData schedule)
      _ -> tr [] (scheduleToData schedule)
  in 
    List.indexedMap mapSchedules hh.schedule

scheduleToData : Schedule -> List (Html Msg)
scheduleToData schedule = 
  [ td [] [ text schedule.time ]
  , td [] [ text schedule.scheduleDescription ]
  ]

mkTableEntry : String -> Html Msg
mkTableEntry x = 
  td [] [text x]

viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
  input [ type_ t, placeholder p, value v, onInput toMsg ] []




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- -- HTTP

getHappyHours : Cmd Msg
getHappyHours =
  Http.send SearchResponse happyHoursGet