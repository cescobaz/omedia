module Time where

import qualified Data.Time.Clock  as Clock
import qualified Data.Time.Format as Format

utcDateTimeFormat :: String
utcDateTimeFormat = "%Y-%m-%dT%T%QZ"

currentUTCDateTimeString :: IO String
currentUTCDateTimeString =
    Format.formatTime Format.defaultTimeLocale utcDateTimeFormat
    <$> Clock.getCurrentTime
