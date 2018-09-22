module Common.RestEndpoints where 

import Servant

import Common.Dto

type HappyHourApi =
      "happy-hour" :> Capture "happy-hour-id" Integer :> ReqBody '[JSON] HappyHour :> Put '[JSON] ()
  :<|>"happy-hour" :> Capture "happy-hour-id" Integer :> Get '[JSON] HappyHour