module View.View exposing (view)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (type_, placeholder, value, selected, checked)
import Time
import Model.Model exposing (..)
import View.DisplayTime exposing (displayTime, timeSpendString, stringDateTime)
import View.FilterView exposing (filterHistory)
import View.LoginView exposing (loginView, viewNavBar)
import View.Colors exposing (..)
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
    let
        dropDownItems = createDropDown model.showProjectDropDown model.currentProject ChangeCurrentProject model.projectList
    in 
        Element.column [ Element.width Element.fill ]
            [ Element.row [ Element.centerX ]
                [ Element.text "Project to time: "
                , Element.row 
                    [ Events.onClick ToggleProjectDropDown
                    , Element.centerX 
                    , Element.below dropDownItems --(projectDropDown model)
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


createDropDown: Bool -> String -> (String -> Msg) -> List String -> Element.Element Msg
createDropDown showDropDown selected msg lst =
    if showDropDown
    then 
        Element.column [ Background.color lightColor ]
            ( List.map
                (\listItem -> 
                    if listItem == selected
                    then Element.none
                    else 
                        Element.row 
                            [ Events.onClick (msg listItem)
                            , Element.width <| Element.px 100
                            ] 
                            [ Element.text listItem ]
                )
                lst
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

inputTextChange: Element.Color -> (String -> Msg) -> String -> Element.Element Msg
inputTextChange color handler txt =
    Input.text
        [ Background.color color, Element.width <| Element.px 25, Element.centerY ]
        { onChange = handler
        , text = txt
        , placeholder = Nothing
        , label = Input.labelRight [] <| Element.none
        }

viewTimedHistory: Model -> Html Msg
viewTimedHistory model =
    if List.isEmpty model.completedList
    then 
        Element.layout [] <| Element.el [ Element.centerX, Element.height <| Element.px 150 ] <| Element.text "History - No Completed Timed Items Yet..." 
    else 
        let
            getFromTimeFrame = stringDateTime model.completedFromTime model.timeZone
            hour = getFromTimeFrame (Just Hour)
            minute = getFromTimeFrame (Just Minute)
            second = getFromTimeFrame (Just Second)
            day = getFromTimeFrame (Just Day)
            month = getFromTimeFrame (Just Month)
            year = getFromTimeFrame (Just Year)

            timeFrameStringList = List.map (\tf -> timeFrameToString tf) timeFrameList

            fromTimeFrame = timeFrameToString model.completedFromTimeFrame
            fromDropDownItems = createDropDown model.showTimeFrameFromDropDown fromTimeFrame ChangeCompletedFromTimeFrame timeFrameStringList

            filterTime = 
                if model.showByStartTime
                then Element.row [ Element.alignLeft ] 
                        [ Element.text <| stringDateTime model.completedFromTime model.timeZone Nothing
                        , Element.text " to " 
                        , Element.text <| stringDateTime model.completedToTime model.timeZone Nothing

                        , Element.text " -----> "
                        , inputTextChange debugColor HandleHourChange hour
                        , Element.text ":"
                        , inputTextChange debugColor HandleMinuteChange minute
                        , Element.text ":"
                        , inputTextChange debugColor HandleSecondChange second
                        , Element.text <| " " ++ day ++ " " ++ month ++ " " ++ year ++ " "

                        , Input.button
                            [ Background.color primaryColor
                            , Element.focused [ Background.color focussedColor ]
                            ]
                            { onPress = Just ( ChangeCompletedTime Start Decrement )
                            , label =  Element.el [ Element.padding 10 ] (Element.text "-")
                            } 
                        
                        , Element.row
                            [ Events.onClick ToggleTimeFrameFromDropDown
                            , Element.centerX 
                            , Element.below fromDropDownItems
                            ] 
                            [ Element.el [ Element.width <| Element.px 100, Element.clip ] <| Element.text fromTimeFrame
                            , Element.el [ Element.alignRight ] (Element.text (" ▾ "))
                            ]
                        , Input.button
                            [ Background.color primaryColor
                            , Element.focused [ Background.color focussedColor ]
                            ]
                            { onPress = Just ( ChangeCompletedTime Start Increment )
                            , label =  Element.el [ Element.padding 10 ] (Element.text "+")
                            } 
                        ]

                else Element.text "Filter by started time?"

        in
        div [] 
            [ 
            Element.layout [] <|
                Element.column [ Element.width Element.fill ]
                    [ Element.row [ Element.height <| Element.px 100, Element.centerX ] 
                        [ Element.el [ Background.color lightColor ] <| Element.text "Timed History" ]
                    , Element.row [ Element.centerX ] [ Element.text "Filter history by:" ]
                    , Element.row [ Element.alignLeft, Element.width Element.fill ] 
                        [ Input.checkbox [ Element.padding 10, Background.color darkColor, Element.width <| Element.px 40 ]
                            { onChange = ToggleShowStarted
                            , icon = Input.defaultCheckbox
                            , checked = model.showByStartTime
                            , label = Input.labelRight [] <| Element.none
                            }
                        , filterTime
                        ]
                    ]
            , text "Show history by: "
            , br [] []
            , displayTime model.completedFromTime model.timeZone
            , displayAdjustTimes model ChangeCompletedTime ChangeCompletedFromTimeFrame Start
            , text "\tand:   "
            , displayTime model.completedToTime model.timeZone
            , displayAdjustTimes model ChangeCompletedTime ChangeCompletedToTimeFrame End
            , br [] []
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