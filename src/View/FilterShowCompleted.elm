module View.FilterShowCompleted exposing (..)

import Time exposing (..)
import Html exposing (..)
import Model.Model exposing (..)

filterShowCompleted: Model -> List Completed
filterShowCompleted model =
    let
        from = Time.posixToMillis (getShowDateTimeFromString model.showCompletedFromDate model.showCompletedFromTime)
        to = Time.posixToMillis (getShowDateTimeFromString model.showCompletedToDate model.showCompletedToTime)
    in
        List.filter 
            (\completedItem -> (Time.posixToMillis completedItem.startTime > from) && (Time.posixToMillis completedItem.startTime < to))
            model.completedList


getListElementByIndex: List a -> Int -> Maybe a
getListElementByIndex lst index =
    case lst of
        [] -> Nothing
        h::t ->
            if index < 1
            then Just h
            else getListElementByIndex t (index - 1)


getIntFromStringList: (List String -> Int -> Maybe String) -> List String -> Int -> Int
getIntFromStringList f lst index =
    case f lst index of
        Just y -> Maybe.withDefault 0 (String.toInt y)
        Nothing -> 0


getShowDateTimeFromString: String -> String -> Time.Posix
getShowDateTimeFromString dateString timeString =
    let
        dates = String.split "-" dateString
        times = String.split ":" timeString

        mins = 1000 * 60
        hours = mins * 60
        days = hours * 24
        months = days * 30
        years = days * 365
        year = (getIntFromStringList getListElementByIndex dates 0 - 1970) * years
        month = (getIntFromStringList getListElementByIndex dates 1 - 1) * months
        day = (getIntFromStringList getListElementByIndex dates 2 - 1) * days
        hour = getIntFromStringList getListElementByIndex times 0 * hours
        minute = getIntFromStringList getListElementByIndex times 1 * mins
    in
        Time.millisToPosix (year + month + day + hour + minute)