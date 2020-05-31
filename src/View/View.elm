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
                    , displayTime model.currentTime model.timeZone
                    , Element.layout [] (viewAddProject model)
                    , Element.layout [] (viewDefault model)
                    , showEditingOrCompleted model
                    ]
            else 
                div []
                    [ Element.layout [] <| viewNavBar model
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
        dropDownItems = createDropDownItems model.showProjectDropDown model.currentProject ChangeCurrentProject model.projectList
    in 
        Element.column [ Element.width Element.fill ]
            [ Element.row [ Element.centerX ]
                [ Element.text "Project to time: "
                , createDropDownRow ToggleProjectDropDown dropDownItems 100 model.currentProject
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


createDropDownRow: Msg -> Element.Element Msg -> Int -> String -> Element.Element Msg
createDropDownRow toggler dropDownItems width txt =
    Element.row
        [ Events.onClick toggler
        , Element.alignLeft
        , Element.below dropDownItems
        ] 
        [ Element.el [ Element.width <| Element.px width, Element.clip ] <| Element.text txt
        , Element.el [ Element.alignRight ] (Element.text (" ▾ "))
        ]

createDropDownItems: Bool -> String -> (String -> Msg) -> List String -> Element.Element Msg
createDropDownItems showDropDown selected msg lst =
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

inputTextChange: Element.Color -> (TimeFrame -> StartOrEnd -> String -> Msg) -> TimeFrame -> String -> StartOrEnd -> Element.Element Msg
inputTextChange color handler timeFrame txt startOrEnd =
    Input.text
        [ Background.color color, Element.width <| Element.px 25, Element.centerY ]
        { onChange = (handler timeFrame startOrEnd)
        , text = txt
        , placeholder = Nothing
        , label = Input.labelRight [] <| Element.none
        }

createFilterByTimeRow: Model -> Element.Element Msg
createFilterByTimeRow model =
    if model.showByStartTime
    then
        let
            timeFrameStringList = List.map (\tf -> timeFrameToString tf) timeFrameList

            getFromTimeFrame = stringDateTime model.completedFromTime model.timeZone
            fromHour = getFromTimeFrame (Just Hour)
            fromMinute = getFromTimeFrame (Just Minute)
            fromSecond = getFromTimeFrame (Just Second)
            fromDay = getFromTimeFrame (Just Day)
            fromMonth = getFromTimeFrame (Just Month)
            fromYear = getFromTimeFrame (Just Year)
            fromTimeFrame = timeFrameToString model.completedFromTimeFrame
            fromDropDownItems = createDropDownItems model.showTimeFrameFromDropDown fromTimeFrame ChangeCompletedFromTimeFrame timeFrameStringList

            getToTimeFrame = stringDateTime model.completedToTime model.timeZone
            toHour = getToTimeFrame (Just Hour)
            toMinute = getToTimeFrame (Just Minute)
            toSecond = getToTimeFrame (Just Second)
            toDay = getToTimeFrame (Just Day)
            toMonth = getToTimeFrame (Just Month)
            toYear = getToTimeFrame (Just Year)
            toTimeFrame = timeFrameToString model.completedToTimeFrame
            toDropDownItems = createDropDownItems model.showTimeFrameToDropDown toTimeFrame ChangeCompletedToTimeFrame timeFrameStringList
        in
            Element.row [ Element.alignLeft ] 
                [ inputTextChange lightColor HandleTimeChange Hour fromHour Start
                , Element.text ":"
                , inputTextChange lightColor HandleTimeChange Minute fromMinute Start
                , Element.text ":"
                , inputTextChange lightColor HandleTimeChange Second fromSecond Start
                , Element.text <| " " ++ fromDay ++ " " ++ fromMonth ++ " " ++ fromYear ++ " "
                , Input.button
                    [ Background.color primaryColor
                    , Element.focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just <| ChangeCompletedTime Start Decrement
                    , label =  Element.el [ Element.padding 10 ] (Element.text "-")
                    } 
                , createDropDownRow ToggleTimeFrameFromDropDown fromDropDownItems 70 fromTimeFrame
                , Input.button
                    [ Background.color primaryColor
                    , Element.focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just <| ChangeCompletedTime Start Increment
                    , label =  Element.el [ Element.padding 10 ] (Element.text "+")
                    } 

                , Element.text "   to   "

                , inputTextChange lightColor HandleTimeChange Hour toHour End
                , Element.text ":"
                , inputTextChange lightColor HandleTimeChange Minute toMinute End
                , Element.text ":"
                , inputTextChange lightColor HandleTimeChange Second toSecond End
                , Element.text <| " " ++ toDay ++ " " ++ toMonth ++ " " ++ toYear ++ " "
                , Input.button
                    [ Background.color primaryColor
                    , Element.focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just <| ChangeCompletedTime End Decrement
                    , label =  Element.el [ Element.padding 10 ] (Element.text "-")
                    } 
                , createDropDownRow ToggleTimeFrameToDropDown toDropDownItems 70 toTimeFrame
                , Input.button
                    [ Background.color primaryColor
                    , Element.focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just <| ChangeCompletedTime End Increment
                    , label =  Element.el [ Element.padding 10 ] (Element.text "+")
                    } 
                ]
    else Element.text "Filter by started time?"

createFilterByProjectRow: Model -> Element.Element Msg
createFilterByProjectRow model =
    if model.showFilterByProject
    then 
        let 
            dropDownItems = createDropDownItems model.showFilterByProjectDropDown model.projectShown ChangeShowByProject model.projectList
        in
            createDropDownRow ToggleFilterProjectDropDown dropDownItems 100 model.projectShown
    else Element.text "Filter by project?"

viewTimedHistory: Model -> Html Msg
viewTimedHistory model =
    if List.isEmpty model.completedList
    then 
        Element.layout [] <| Element.el [ Element.centerX, Element.height <| Element.px 150 ] <| Element.text "History - No Completed Timed Items Yet..." 
    else 
        div [] 
            [ 
            Element.layout [] <|
                Element.column [ Element.width Element.fill ]
                    [ Element.row [ Element.height <| Element.px 100, Element.centerX ] 
                        [ Element.el [ Background.color lightColor ] <| Element.text "Timed History" ]
                    , Element.row [ Element.centerX ] [ Element.text "Filter history by:" ]

                    -- filter by time
                    , Element.row [ Element.alignLeft, Element.width Element.fill ] 
                        [ Input.checkbox [ Element.padding 10, Background.color darkColor, Element.width <| Element.px 40 ]
                            { onChange = ToggleShowStarted
                            , icon = Input.defaultCheckbox
                            , checked = model.showByStartTime
                            , label = Input.labelRight [] <| Element.none
                            }
                        , createFilterByTimeRow model
                        ]

                    -- filter by project
                    , Element.row [ Element.alignLeft, Element.width  Element.fill ]
                        [ Input.checkbox [ Element.padding 10, Background.color darkColor, Element.width <| Element.px 40 ]
                            { onChange = ToggleShowFilterProject
                            , icon = Input.defaultCheckbox
                            , checked = model.showFilterByProject
                            , label = Input.labelRight [] <| Element.none
                            }
                        , createFilterByProjectRow model
                        ]
                    ]
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