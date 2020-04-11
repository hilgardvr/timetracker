module View.View exposing (view)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (type_, placeholder, value, selected)
import Time
import Model.Model exposing (..)
import View.DisplayTime exposing (displayTime, timeSpendString)
import View.FilterShowCompleted exposing (filterShowCompleted)

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
            , viewTiming model
            ]


viewAddProject: Model -> Html Msg
viewAddProject model =
    if model.timing
    then div [] []
    else
        div []
            [ if List.isEmpty model.projectList
              then h3 [] [ text "Add a new project below to start..."]
              else h3 [] []
            , input [ type_ "text", placeholder "Add a new project here", value model.newProject, onInput NewProject ] []
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
                ( List.map 
                    (\project -> 
                        let 
                            isSelected = project == model.currentProject
                        in
                    
                            option [ value project, selected isSelected ] [ text project ]
                    )
                    model.projectList 
                )
            , button  
                [ onClick ToggleTimer ]
                [ text "Start" ]
            , input [ type_ "text", placeholder "Add a note?", value model.note, onInput ChangeNote ] []
            , showEditingOrCompleted model
            ] 


viewTiming: Model -> Html Msg
viewTiming model =
    div []
        [ text (timeSpendString model.startTime model.currentTime)
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
                    [ text ("No item found with id: " ++ model.editingId)
                    , button  
                        [ onClick (Editing (Completed "" "" (Time.millisToPosix 0) (Time.millisToPosix 0) "")) ]
                        [ text "Return" ]
                    ]


showCompleted: Model -> Html Msg
showCompleted model =
    let
        filteredCompletedList = filterShowCompleted model
    in
        if List.isEmpty model.completedList
        then 
            div []
                [ h4 [] [] 
                ]
        else 
            div [] 
                [ h4 [] [ text "Timed History" ]
                , text "From: "
                -- , displayAdjustTimes model.timeFrameList ChangeEditingEndTimeFrame
                , br [] []
                , text "To:   "
                -- , displayAdjustTimes model.timeFrameList ChangeEditingEndTimeFrame
                , ul [] 
                    ( List.map 
                        ( \elem -> 
                            li [] 
                                (displayCompletedItem model elem)
                        )
                        filteredCompletedList
                    )
                ]

displayCompletedItem: Model -> Completed -> List (Html Msg)
displayCompletedItem model completed =
    [ text ("Project: " ++ completed.project)
    , br [] []
    , text "time spend: "
    , text (timeSpendString completed.startTime completed.endTime) 
    , br [] []
    , text ("note: " ++ completed.note)
    , br [] []
    , text "start time: "
    , displayTime completed.startTime model.timeZone
    , br [] []
    , text "end time : "
    , displayTime completed.endTime model.timeZone
    , br [] []
    , button  
        [ onClick (Editing completed) ]
        [ text "Edit" ]
    ]

displayEditCompletedItem: Model -> Completed -> List (Html Msg)
displayEditCompletedItem model completed =
    [ text ("Project: " ++ completed.project)
    , select [ onInput ChangeEditProject ]
        ( List.map 
            (\project ->    
                let
                    isSelected = project == completed.project
                in
                    option [ value project, selected isSelected ] [ text project ]
            ) 
        model.projectList 
        )
    , br [] []
    , text "time spend: "
    , text (timeSpendString completed.startTime completed.endTime)
    , br [] []
    , text ("note: " ++ completed.note)
    , input [ type_ "text", placeholder "Edit note?", value model.editingNote, onInput ChangeEditNote ] []
    , br [] []
    , text "start time: "
    , displayTime model.editingStartTime model.timeZone
    , displayAdjustTimes model.timeFrameList ChangeEditingStartTimeFrame Start
    , br [] []
    , text "end time : "
    , displayTime model.editingEndTime model.timeZone
    , displayAdjustTimes model.timeFrameList ChangeEditingEndTimeFrame End
    , br [] []
    , button  
        [ onClick (Editing completed) ]
        [ text "Save" ]
    , button  
        [ onClick (DeleteCompleted completed) ]
        [ text "Delete" ]
    , button  
        [ onClick DiscardChanges ]
        [ text "Don't Save" ]
    ]

displayAdjustTimes: List String -> (String -> Msg) -> StartOrEnd -> Html Msg
displayAdjustTimes timeFrameList action startOrEnd = 
    span []
        [ button  
            [ onClick (ChangeEditTime startOrEnd Decrement) ]
            [ text "-" ]
        , select [ onInput action ]
            ( List.map 
                (\timeFrame ->    
                    let
                        isSelected = timeFrame == "Minute"
                    in
                        option [ value timeFrame, selected isSelected ] [ text timeFrame ]
                ) 
                timeFrameList
            )
        , button  
            [ onClick (ChangeEditTime startOrEnd Increment) ]
            [ text "+" ]
        ]
