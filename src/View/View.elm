module View.View exposing (view)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (type_, placeholder, value, selected, checked)
import Time
import Model.Model exposing (..)
import View.DisplayTime exposing (displayTime, timeSpendString)
import View.FilterView exposing (filterHistory)
import View.LoginView exposing (loginView, viewNavBar)
import View.Colors exposing (primaryColor, lightColor, darkColor, focussedColor, maxProjectShownSize)
import Element as Element
import Element.Input as Input
import Element.Background as Background
import Element.Events as Events


-- view

view: Model -> Html Msg
view model =
    case model.loginStatus of
        LoggedOut -> 
            Element.layout
                []
                (loginView model)
        Signup ->
            Element.layout
                []
                (loginView model)
        Pending -> 
            Element.layout
                []
                (loginView model)
        LoggedIn -> 
            if not model.timing
            then 
                div []
                    [ Element.layout [] (viewNavBar model)
                    , Element.layout [ Events.onClick CloseMenu ] (viewAddProject model)
                    , Element.layout [] (viewDefault model)
                    , showEditingOrCompleted model
                    ]
            else 
                div []
                    [ Element.layout [] <| viewNavBar model
                    -- , Element.layout [] (viewAddProject model)
                    , Element.layout [] (viewTiming model)
                    , showEditingOrCompleted model
                    ]


viewAddProject: Model -> Element.Element Msg
viewAddProject model =
    Element.row [ Element.centerX, Element.height (Element.px 100) ]
        [ Input.text
            [ Element.centerX ]
            { onChange = NewProject
            , text = model.newProject
            , placeholder = Just (Input.placeholder [] (Element.text "Add a new project?"))
            , label = Input.labelAbove [] Element.none
            }
        , Input.button
            [ Background.color primaryColor
            , Element.focused [ Background.color focussedColor ]
            ]
            { onPress = Just AddProject
            , label = Element.text "Add Project"
            } 
        ]

viewDefault: Model -> Element.Element Msg
viewDefault model =
    Element.column [ Element.width Element.fill ]
        [ Element.row [ Element.centerX ]
            [ Element.text "Project to time: "
            , Element.row 
                [ Events.onClick ToggleProjectDropDown
                , Element.centerX 
                , Element.below (projectDropDown model)
                ] 
                [ Element.el [ Element.width <| Element.px 100, Element.clip ] (Element.text model.currentProject)
                , Element.el [ Element.alignRight ] (Element.text (" ▾ "))
                ]
            , Input.button
                [ Background.color primaryColor
                , Element.focused [ Background.color focussedColor ]
                ]
                { onPress = Just ToggleTimer
                , label = Element.text "Start"
                } 
            ]
        , Element.row [ Element.centerX ]
            [ Input.text
                [ Element.centerX ]
                { onChange = ChangeNote
                , text = model.note
                , placeholder = Just (Input.placeholder [] (Element.text "Add a note?"))
                , label = Input.labelAbove [] Element.none
                }
            ]
        ]

projectDropDown: Model -> Element.Element Msg
projectDropDown model =
    if model.showProjectDropDown
    then
        Element.column []
            ( List.map
                (\project -> 
                    if project == model.currentProject
                    then Element.none
                    else 
                        Element.row 
                            [ Events.onClick (ChangeCurrentProject project)
                            , Element.width <| Element.px 100
                            ] 
                            [ Element.text project ]
                )
                model.projectList
            )
    else Element.none


viewTiming: Model -> Element.Element Msg
viewTiming model =
    Element.column [ Element.width Element.fill  ]
        [  Element.row [ Element.centerX, Element.padding 20 ] [ Element.text <| "Timing project: " ++ model.currentProject ]
        , Element.row [ Element.centerX ] [ Element.text <| timeSpendString model.startTime model.currentTime ]
        , Element.row [ Element.centerX ] 
            [ Input.text
                [ ]
                { onChange = ChangeNote
                , text = model.note
                , placeholder = Just (Input.placeholder [] (Element.text "Add a note?"))
                , label = Input.labelAbove [] Element.none
                }
            ]
        , Element.row [ Element.centerX ] 
            [ Input.button
                [ Background.color primaryColor
                , Element.focused [ Background.color focussedColor ]
                ]
                { onPress = Just ToggleTimer
                , label = Element.text "Stop"
                } 
            ]
        ]
    -- div []
    --     [ text (timeSpendString model.startTime model.currentTime)
    --     , text model.currentProject
    --     , button  
    --         [ onClick ToggleTimer ]
    --         [ text "Stop" ]
    --     , input [ type_ "text", placeholder "Add a note?", value model.note, onInput ChangeNote ] []
    --     , showEditingOrCompleted model
    --     ]


showEditingOrCompleted: Model -> Html Msg
showEditingOrCompleted model =
    if model.editing
    then showEditing model
    else viewTimedHistory model

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


viewTimedHistory: Model -> Html Msg
viewTimedHistory model =
    if List.isEmpty model.completedList
    then 
        div []
            [ h4 [] [ text "History - No Completed Timed Items Yet..."] 
            ]
    else 
        div [] 
            [ h4 [] [ text "Timed History" ]
            , text "Show history by: "
            , br [] []
            , input [ type_ "checkbox"
                    , checked model.showByStartTime
                    , onClick ToggleShowStarted 
                    ] []
            , text "Started timing between: "
            , displayTime model.completedFromTime model.timeZone
            , displayAdjustTimes model ChangeCompletedTime ChangeCompletedFromTimeFrame Start
            , text "\tand:   "
            , displayTime model.completedToTime model.timeZone
            , displayAdjustTimes model ChangeCompletedTime ChangeCompletedToTimeFrame End
            , br [] []
            , input [ type_ "checkbox"
                    , checked model.showByProject
                    , onClick ToggleShowByProject 
                    ] []
            , text "Filter by project "
            , select [ onInput ChangeShowByProject ]
                ( List.map 
                    (\project -> 
                        let 
                            isSelected = project == model.projectShown
                        in
                            option [ value project, selected isSelected ] [ text project ]
                    )
                    model.projectList
                )
            , displayTotalTime model
            , ul [] 
                ( List.map 
                    ( \elem -> 
                        li [] 
                            (displayCompletedItem model elem)
                    )
                    (filterHistory model)
                )
            ]

displayTotalTime: Model -> Html Msg
displayTotalTime model =
    let
        completedTimes = List.map (\item -> Time.posixToMillis item.endTime - Time.posixToMillis item.startTime) (filterHistory model)
        totalTime = List.foldl (+) 0 completedTimes
    in
        Element.layout []
            (Element.row [ Element.centerX ]
                [
                    Element.text ("Total time spent: " ++ View.DisplayTime.timeSpendString (Time.millisToPosix 0) (Time.millisToPosix totalTime))
                ]
            )

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
    , displayAdjustTimes model ChangeEditTime ChangeEditingStartTimeFrame Start
    , br [] []
    , text "end time : "
    , displayTime model.editingEndTime model.timeZone
    , displayAdjustTimes model ChangeEditTime ChangeEditingEndTimeFrame End
    , br [] []
    , button  
        [ onClick (Editing completed) ]
        [ text "Save" ]
    , button  
        [ onClick (DeleteCompleted completed) ]
        [ text "Delete" ]
    , button  
        [ onClick DiscardChanges ]
        [ text "Cancel" ]
    ]

displayAdjustTimes: Model -> (StartOrEnd -> IncOrDec -> Msg) -> (String -> Msg) -> StartOrEnd -> Html Msg
displayAdjustTimes model timeToChange timeFrameChanged startOrEnd = 
    span []
        [ button  
            [ onClick (timeToChange startOrEnd Decrement) ]
            [ text "-" ]
        , select [ onInput timeFrameChanged ]
            ( List.map 
                (\timeFrame ->    
                    let
                        isSelected = 
                            case timeFrameChanged("") of
                                ChangeEditingStartTimeFrame(_) -> model.editingStartTimeFrame == timeFrame
                                ChangeEditingEndTimeFrame(_) -> model.editingEndTimeFrame == timeFrame
                                ChangeCompletedFromTimeFrame(_) -> model.completedFromTimeFrame == timeFrame
                                ChangeCompletedToTimeFrame(_) -> model.completedToTimeFrame == timeFrame
                                _ -> timeFrame == Minute
                    in
                        option [ value (timeFrameToString timeFrame), selected isSelected ] [ text (timeFrameToString timeFrame) ]
                ) 
                model.timeFrameList
            )
        , button  
            [ onClick (timeToChange startOrEnd Increment) ]
            [ text "+" ]
        ]