module Backend.ServerHelpers where

-- import Common.Dto as DTO

-- jnkMenu :: IO B.ByteString
-- jnkMenu = B.readFile "resources/data/johnny_noodle_king.json"

-- parseHH :: B.ByteString -> Either String DTO.HappyHour
-- parseHH bs = eitherDecode bs

-- happyHourDtoToDb :: UUID -> DTO.HappyHour -> DB.HappyHour
-- happyHourDtoToDb uuid DTO.HappyHour{..} = DB.HappyHour { _id = uuid, ..}

-- happyHourDbToDto :: DB.HappyHour -> DTO.HappyHour
-- happyHourDbToDto DB.HappyHour{..} = defaultHH 
--   { DTO._city = _city
--   , DTO._restaurant = _restaurant
--   }