module Generated exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


type alias HappyHour =
    { city : String
    , restaurant : String
    , schedule : List (Schedule)
    , link : String
    }

decodeHappyHour : Decoder HappyHour
decodeHappyHour =
    decode HappyHour
        |> required "city" string
        |> required "restaurant" string
        |> required "schedule" (list decodeSchedule)
        |> required "link" string

postHappyhours : HappyHour -> Http.Request (String)
postHappyhours body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "happy-hours"
                ]
        , body =
            Http.jsonBody (encodeHappyHour body)
        , expect =
            Http.expectJson string
        , timeout =
            Nothing
        , withCredentials =
            False
        }

putHappyhoursByHappyhourid : String -> HappyHour -> Http.Request (NoContent)
putHappyhoursByHappyhourid capture_happyHourId body =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "happy-hours"
                , capture_happy-hour-id |> Http.encodeUri
                ]
        , body =
            Http.jsonBody (encodeHappyHour body)
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteHappyhoursByHappyhourid : String -> Http.Request (NoContent)
deleteHappyhoursByHappyhourid capture_happyHourId =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "happy-hours"
                , capture_happy-hour-id |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectStringResponse
                (\{ body } ->
                    if String.isEmpty body then
                        Ok NoContent
                    else
                        Err "Expected the response body to be empty"
                )
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getHappyhoursByHappyhourid : String -> Http.Request (HappyHour)
getHappyhoursByHappyhourid capture_happyHourId =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "happy-hours"
                , capture_happy-hour-id |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeHappyHour
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getHappyhours : Http.Request (List (HappyHour))
getHappyhours =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "happy-hours"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeHappyHour)
        , timeout =
            Nothing
        , withCredentials =
            False
        }