module Common.Routes where 

import Servant
import Data.UUID (UUID)

import Common.Dto

hhApi :: Proxy HappyHourApi
hhApi = Proxy

type HappyHourApi =
      -- createHH
      "happy-hours" :> ReqBody '[JSON] HappyHour :> Post '[JSON] UUID
      -- updateHH
  :<|>"happy-hours" :> Capture "happy-hour-id" UUID :> ReqBody '[JSON] HappyHour :> Put '[JSON] NoContent
      -- deleteHH
  :<|>"happy-hours" :> Capture "happy-hour-id" UUID :> Delete '[JSON] NoContent
      -- getHH
  :<|>"happy-hours" :> Capture "happy-hour-id" UUID :> Get '[JSON] HappyHour
      -- queryHHs
  :<|>"happy-hours" :> Get '[JSON] [HappyHour]
