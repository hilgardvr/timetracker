module View.View exposing (view)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (type_, placeholder, value)
import Time
import Model.Model exposing (..)
import View.DisplayTime exposing (displayTime, stringTime)

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
            , input [ type_ "text", placeholder "Add a note?", value model.note, onInput EditNote ] []
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
        , input [ type_ "text", placeholder "Add a note?", value model.note, onInput EditNote ] []
        , listCompleted model.completedList
        ]

listCompleted: List Completed -> Html Msg
listCompleted completed =
    ul [] 
        ( List.map 
            ( \elem -> 
                li [] 
                    -- [ text (elem.project ++ "\t: " ++ stringTime (calcTimeSpend elem.startTime elem.endTime) Time.utc)
                    [ text ("Project: " ++ elem.project)
                    , br [] []
                    , text "time spend: "
                    , displayTime (calcTimeSpend elem.startTime elem.endTime) Time.utc
                    , text ("\t" ++ elem.note)
                    , br [] []
                    , text "start time: "
                    , displayTime elem.startTime Time.utc
                    , br [] []
                    , text "end time : "
                    , displayTime elem.endTime Time.utc
                    ]
            )
            completed
        )

calcTimeSpend: Time.Posix -> Time.Posix -> Time.Posix
calcTimeSpend startTime endTime = 
    Time.millisToPosix (Time.posixToMillis endTime - Time.posixToMillis startTime)
