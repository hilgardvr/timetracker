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
    then 
        div []
            [ viewAddProject model
            , viewDefault model
            ]
    else 
        div []
            [ viewAddProject model
            ,viewTiming model
            ]


viewAddProject: Model -> Html Msg
viewAddProject model =
    div []
        [ input [ type_ "text", placeholder "New project", value model.newProject, onInput NewProject ] []
        , button
            [ onClick AddProject ]
            [ text "New project to time track" ]
        ]

viewDefault: Model -> Html Msg
viewDefault model =
    if List.isEmpty model.projectList
    then div [] []
    else
        div []
            [ displayTime model.currentTime model.timeZone
            , select [ onInput ChangeCurrentProject ]
                ( List.map (\project -> option [ value project ] [ text project ]) model.projectList )
            , button  
                [ onClick ToggleTimer ]
                [ text "Start" ]
            , listCompleted model.completedList
            ] 


viewTiming: Model -> Html Msg
viewTiming model =
    div []
        [ displayTime 
            (calcTimeSpend model.startTime model.currentTime)
            Time.utc
        , text model.currentProject
        , button  
            [ onClick ToggleTimer ]
            [ text "Stop" ]
        , listCompleted model.completedList
        ]

listCompleted: List Completed -> Html Msg
listCompleted completed =
    ul [] 
        ( List.map 
            ( \elem -> 
                li [] 
                    [ text (elem.project ++ "\t: " ++ stringTime (calcTimeSpend elem.startTime elem.endTime) Time.utc)
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