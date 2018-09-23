module Common.Routes where 

import Servant
import Data.UUID (UUID)

import Common.Dto

type HappyHourApi =
      "happy-hour" :> ReqBody '[JSON] HappyHour :> Post '[JSON] UUID
  :<|>"happy-hour" :> Capture "happy-hour-id" Integer :> Get '[JSON] HappyHour
  :<|>"happy-hour" :> Get '[JSON] [HappyHour]