module Seraph.Log (getFormattedTime) where

import Control.Applicative
import Data.Time.LocalTime
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

getFormattedTime :: IO String
getFormattedTime = cleanCalendar <$> getZonedTime

cleanCalendar :: ZonedTime -> String
cleanCalendar = formatTime defaultTimeLocale "%Y/%m/%d %H:%M:%S"

