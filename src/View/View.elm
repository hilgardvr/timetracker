module View.View exposing (view)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (type_, placeholder, value)
import Time
import Model.Model exposing (..)

-- view

view: Model -> Html Msg
view model =
    if not model.timing
    then viewDefault model
    else viewTiming model

viewDefault: Model -> Html Msg
viewDefault model =
    div []
        [ displayTime model.currentTime model.timeZone
        , input [ type_ "text", placeholder "What do you want to time?", value model.currentActivity, onInput ChangeActivity ] []
        , button  
            [ onClick ToggleTimer ]
            [ if model.timing then text "Stop" else text "Start" ]
        , listCompleted model.completed
        ] 

viewTiming: Model -> Html Msg
viewTiming model =
    div []
        [ displayTime 
            (calcTimeSpend model.startTime model.currentTime)
            Time.utc
        , text model.currentActivity
        , button  
            [ onClick ToggleTimer ]
            [ if model.timing then text "Stop" else text "Start" ]
        , listCompleted model.completed
        ]

listCompleted: List Completed -> Html Msg
listCompleted completed =
    ul [] 
        ( List.map 
            ( \elem -> 
                li [] 
                    [ text (elem.activity ++ "\t: " ++ stringTime (calcTimeSpend elem.startTime elem.endTime) Time.utc)
                    , br [] []
                    , text ("start time: " ++ "\t: " ++ stringTime elem.startTime Time.utc ++ " end: " ++ stringTime elem.endTime Time.utc ) ]
            )
            completed
        )

calcTimeSpend: Time.Posix -> Time.Posix -> Time.Posix
calcTimeSpend startTime endTime = 
    Time.millisToPosix (Time.posixToMillis endTime - Time.posixToMillis startTime)

stringTime: Time.Posix -> Time.Zone -> String
stringTime time zone =
    let
        hour    = padTime (String.fromInt (Time.toHour zone time))
        minute  = padTime (String.fromInt (Time.toMinute zone time))
        second  = padTime (String.fromInt (Time.toSecond zone time))
    in
        (hour ++ ":" ++ minute ++ ":" ++ second)

displayTime: Time.Posix -> Time.Zone -> Html Msg
displayTime time zone =
    span [] [ text (stringTime time zone) ]

padTime: String -> String
padTime time =
    if String.length time < 2
    then padTime ("0" ++ time)
    else time