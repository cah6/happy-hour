{-
   
   No description provided (generated by Swagger Codegen https://github.com/swagger-api/swagger-codegen)

   OpenAPI spec version: 
   

   NOTE: This file is auto generated by the swagger code generator program.
   https://github.com/swagger-api/swagger-codegen.git
   Do not edit this file manually.
-}


module Request.Default exposing (happyHoursGet, happyHoursHappyHourIdDelete, happyHoursHappyHourIdGet, happyHoursHappyHourIdPut, happyHoursPost)

import Data.HappyHour exposing (HappyHour, happyHourDecoder, happyHourEncoder)
import Http
import Json.Decode as Decode


basePath : String
basePath =
    "http://localhost:3000"


{-
   
-}
happyHoursGet : Http.Request (List HappyHour)
happyHoursGet =
    { method = "GET"
    , url = basePath ++ "/happy-hours"
    , headers = []
    , body = Http.emptyBody
    , expect = Http.expectJson (Decode.list happyHourDecoder)
    , timeout = Just 30000
    , withCredentials = False
    }
        |> Http.request


{-
   
-}
happyHoursHappyHourIdDelete : String -> Http.Request ()
happyHoursHappyHourIdDelete happyHourId =
    { method = "DELETE"
    , url = basePath ++ "/happy-hours/{happy-hour-id}"
    , headers = []
    , body = Http.emptyBody
    , expect = Http.expectStringResponse (\_ -> Ok ())
    , timeout = Just 30000
    , withCredentials = False
    }
        |> Http.request


{-
   
-}
happyHoursHappyHourIdGet : String -> Http.Request HappyHour
happyHoursHappyHourIdGet happyHourId =
    { method = "GET"
    , url = basePath ++ "/happy-hours/{happy-hour-id}"
    , headers = []
    , body = Http.emptyBody
    , expect = Http.expectJson happyHourDecoder
    , timeout = Just 30000
    , withCredentials = False
    }
        |> Http.request


{-
   
-}
happyHoursHappyHourIdPut : String -> HappyHour -> Http.Request ()
happyHoursHappyHourIdPut happyHourId model =
    { method = "PUT"
    , url = basePath ++ "/happy-hours/{happy-hour-id}"
    , headers = []
    , body = Http.jsonBody <| happyHourEncoder model
    , expect = Http.expectStringResponse (\_ -> Ok ())
    , timeout = Just 30000
    , withCredentials = False
    }
        |> Http.request


{-
   
-}
happyHoursPost : HappyHour -> Http.Request String
happyHoursPost model =
    { method = "POST"
    , url = basePath ++ "/happy-hours"
    , headers = []
    , body = Http.jsonBody <| happyHourEncoder model
    , expect = Http.expectJson Decode.string
    , timeout = Just 30000
    , withCredentials = False
    }
        |> Http.request

