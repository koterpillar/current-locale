-- |Get the current system locale in 'System.Locale' format.
module System.CurrentLocale (currentLocale) where

import Control.Applicative
import Data.List.Split (splitOn)
import System.Locale
import System.Process

getLocaleData :: IO [String]
getLocaleData =
    lines <$> readProcess "locale"
                          ["abday", "day",
                           "abmon", "mon",
                           "am_pm",
                           "d_t_fmt",
                           "d_fmt",
                           "t_fmt",
                           "t_fmt_ampm"]
                          ""

split :: String -> [String]
split = splitOn ";"

-- |Get the current system locale. This function does not initialize the
-- 'System.Locale.TimeLocale.intervals' field.
currentLocale :: IO TimeLocale
currentLocale = do
    [abday, day, abmon, mon, am_pm, d_t_fmt, d_fmt, t_fmt, t_fmt_ampm] <- getLocaleData
    let [am, pm] = split am_pm

    return defaultTimeLocale
        { wDays = zip (split day) (split abday)
        , months = zip (split mon) (split abmon)
        , amPm = (am, pm)
        , dateTimeFmt = d_t_fmt
        , dateFmt = d_fmt
        , timeFmt = t_fmt
        , time12Fmt = t_fmt_ampm
        }
