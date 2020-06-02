module View.DisplayTime exposing (displayTime, stringDateTime, timeSpendString)

import Time exposing (..)
import Html exposing (..)
import Model.Model exposing (Msg, TimeFrame(..))
import Element as Element

displayTime: Time.Posix -> Time.Zone -> Element.Element Msg
displayTime time zone =
    Element.text (stringDateTime time zone Nothing)

padTime: String -> String
padTime time =
    if String.length time < 2
    then padTime ("0" ++ time)
    else time


monthToString: Time.Month -> String
monthToString month =
    case month of
        Jan -> "Jan"
        Feb -> "Feb"
        Mar -> "Mar"
        Apr -> "Apr"
        May -> "May"
        Jun -> "Jun"
        Jul -> "Jul"
        Aug -> "Aug"
        Sep -> "Sep"
        Oct -> "Oct"
        Nov -> "Nov"
        Dec -> "Dec"

stringDateTime: Time.Posix -> Time.Zone -> Maybe TimeFrame -> String
stringDateTime time zone timeframe =
    let
        year    = String.fromInt (Time.toYear zone time)
        month   = monthToString (Time.toMonth zone time)
        day     = String.fromInt (Time.toDay zone time)
        hour    = padTime (String.fromInt (Time.toHour zone time))
        minute  = padTime (String.fromInt (Time.toMinute zone time))
        second  = padTime (String.fromInt (Time.toSecond zone time))
    in
        case timeframe of
            Nothing          -> (hour ++ ":" ++ minute ++ ":" ++ second ++ " " ++ day ++ " " ++ month ++ " " ++ year )
            Just Year        -> year
            Just Month       -> month
            Just Day         -> day
            Just Hour        -> hour
            Just Minute      -> minute
            Just Second      -> second

timeSpendString: Time.Posix -> Time.Posix -> String
timeSpendString startTime endTime =
    let
        totalMs = Time.posixToMillis endTime - Time.posixToMillis startTime
        hours   = totalMs // 3600000
        minutes = modBy 60 (totalMs // 60000)
        seconds = modBy 60 (totalMs // 1000)
    in
        padTime (String.fromInt hours) ++ ":" ++ padTime (String.fromInt minutes) ++ ":" ++ padTime (String.fromInt seconds)