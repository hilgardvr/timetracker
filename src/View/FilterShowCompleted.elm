module View.FilterShowCompleted exposing (..)

import Time exposing (..)
import Html exposing (..)
import Model.Model exposing (..)

filterShowCompleted: Model -> List Completed
filterShowCompleted model =
    model.completedList


getListElementByIndex: List a -> Int -> Maybe a
getListElementByIndex lst index =
    case lst of
        [] -> Nothing
        h::t ->
            if index < 1
            then List.head lst
            else getListElementByIndex t (index - 1)

getShowDateTimeFromString: String -> String -> Time.Posix
getShowDateTimeFromString dateString timeString =
    let
        dates = String.split ("-") dateString

        secs = 1000
        mins = secs * 60
        hours = mins * 60
        days = hours * 24
        months = days * 30
        years = days * 365
    in
        Time.millisToPosix 0