module View.DisplayTime exposing (displayTime, stringTime)

import Time
import Html exposing (..)
import Model.Model exposing (Msg)

displayTime: Time.Posix -> Time.Zone -> Html Msg
displayTime time zone =
    span [] [ text (stringTime time zone) ]

padTime: String -> String
padTime time =
    if String.length time < 2
    then padTime ("0" ++ time)
    else time

stringTime: Time.Posix -> Time.Zone -> String
stringTime time zone =
    let
        hour    = padTime (String.fromInt (Time.toHour zone time))
        minute  = padTime (String.fromInt (Time.toMinute zone time))
        second  = padTime (String.fromInt (Time.toSecond zone time))
    in
        (hour ++ ":" ++ minute ++ ":" ++ second)

calcTimeSpendFrom: Time.Posix -> Time.Posix -> String
calcTimeSpendFrom: startTime endTime =
    let
        totalMs = (Time.posixToMillis endTime) - (Time.posixToMillis startTime)
        hours   = totalMs // 3600000
        minutes = (total - hours)