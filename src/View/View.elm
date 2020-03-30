module View.View exposing (view)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (type_, placeholder, value)
import Time
import Model.Model exposing (..)
import View.DisplayTime exposing (displayTime)

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
    if model.timing
    then div [] []
    else
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
            , input [ type_ "text", placeholder "Add a note?", value model.note, onInput ChangeNote ] []
            , showEditingOrCompleted model
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
        , input [ type_ "text", placeholder "Add a note?", value model.note, onInput ChangeNote ] []
        , showEditingOrCompleted model
        ]


showEditingOrCompleted: Model -> Html Msg
showEditingOrCompleted model =
    if model.editing
    then showEditing model
    else showCompleted model

showEditing: Model -> Html Msg
showEditing model =
    let
        maybeCompleted = List.head (List.filter (\completedItem -> completedItem.id == model.editingId) model.completedList)
    in
        case maybeCompleted of
            Just completed ->
                div []
                    (displayEditCompletedItem model completed)
            Nothing ->
                div []
                    [ text "The item does not exist - this should not happen and is a logic error" 
                    , button  
                        [ onClick (Editing (Completed "" "" (Time.millisToPosix 0) (Time.millisToPosix 0) "")) ]
                        [ text "Return" ]
                    ]


showCompleted: Model -> Html Msg
showCompleted model =
    ul [] 
        ( List.map 
            ( \elem -> 
                li [] 
                    (displayCompletedItem elem)
            )
            model.completedList
        )

displayCompletedItem: Completed -> List (Html Msg)
displayCompletedItem completed =
    [ text ("Project: " ++ completed.project)
    , br [] []
    , text "time spend: "
    , displayTime (calcTimeSpend completed.startTime completed.endTime) Time.utc
    , br [] []
    , text ("note: " ++ completed.note)
    , br [] []
    , text "start time: "
    , displayTime completed.startTime Time.utc
    , br [] []
    , text "end time : "
    , displayTime completed.endTime Time.utc
    , br [] []
    -- , text ("id : " ++ completed.id)
    -- , br [] []
    , button  
        [ onClick (Editing completed) ]
        [ text "Edit" ]
    , button  
        [ onClick (DeleteCompleted completed) ]
        [ text "Delete" ]
    ]

displayEditCompletedItem: Model -> Completed -> List (Html Msg)
displayEditCompletedItem model completed =
    [ text ("Project: " ++ completed.project)
    , select [ onInput ChangeEditProject ]
        ( List.map (\project -> option [ value project ] [ text project ]) model.projectList )
    , br [] []
    , text "time spend: "
    , displayTime (calcTimeSpend completed.startTime completed.endTime) Time.utc
    , br [] []
    , text ("note: " ++ completed.note)
    , br [] []
    , text "start time: "
    , displayTime completed.startTime Time.utc
    , br [] []
    , text "end time : "
    , displayTime completed.endTime Time.utc
    , br [] []
    -- , text ("id : " ++ completed.id)
    -- , br [] []
    , button  
        [ onClick (Editing completed) ]
        [ text "Save" ]
    ]


calcTimeSpend: Time.Posix -> Time.Posix -> Time.Posix
calcTimeSpend startTime endTime = 
    Time.millisToPosix (Time.posixToMillis endTime - Time.posixToMillis startTime)
