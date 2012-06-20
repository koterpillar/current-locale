module System.CurrentLocale where

import System.Locale
import System.Process

format :: String -> String -> IO String
format fmt date = do
    readProcess "date" ["+" ++ fmt, "-d", date] ""

formatWDay :: (String, String) -> IO (String, String)
formatWDay (wday, _) = do
    full <- format "%A" wday
    short <- format "%a" wday
    return (full, short)

formatMonth :: (String, String) -> IO (String, String)
formatMonth (month, _) = do
    let mdate = month ++ " 1"
    full <- format "%B" mdate
    short <- format "%b" mdate
    return (full, short)

currentLocale :: IO TimeLocale
currentLocale = do
    wDays <- mapM formatWDay $ wDays defaultTimeLocale
    months <- mapM formatMonth $ months defaultTimeLocale
    return defaultTimeLocale
        { wDays = wDays
        , months = months
        }
