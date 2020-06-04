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
import Element.Font as Font


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
                    , Element.layout [ Element.width Element.fill, Element.centerX ] 
                        <| Element.el [ Element.centerX ] 
                            <| Element.text 
                                <| stringDateTime model.currentTime model.timeZone Nothing
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
        [ Background.color color, Element.width <| Element.px 25 ]
        { onChange = (handler timeFrame startOrEnd)
        , text = txt
        , placeholder = Nothing 
        , label = Input.labelRight [] <| Element.text txt --Element.none
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
                [ inputTextChange lightColor HandleFilterTimeChange Hour fromHour Start
                , Element.text ":"
                , inputTextChange lightColor HandleFilterTimeChange Minute fromMinute Start
                , Element.text ":"
                , inputTextChange lightColor HandleFilterTimeChange Second fromSecond Start
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

                , inputTextChange lightColor HandleFilterTimeChange Hour toHour End
                , Element.text ":"
                , inputTextChange lightColor HandleFilterTimeChange Minute toMinute End
                , Element.text ":"
                , inputTextChange lightColor HandleFilterTimeChange Second toSecond End
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
        Element.layout [] <|
            Element.column [ Element.width Element.fill ]
                [ Element.row [ Element.height <| Element.px 100, Element.centerX ] 
                    [ Element.el [ Background.color lightColor, Font.bold, Font.underline ] <| Element.text "Timed History" ]
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
                , displayTotalTime model
                , displayItemList model
                ]

displayTotalTime: Model -> Element.Element Msg
displayTotalTime model =
    let
        completedTimes = List.map (\item -> Time.posixToMillis item.endTime - Time.posixToMillis item.startTime) (filterHistory model)
        totalTime = List.foldl (+) 0 completedTimes

    in
        Element.row [ Element.centerX, Element.padding 20, Font.bold ]
            [ Element.text ("Total time spent: " ++ View.DisplayTime.timeSpendString (Time.millisToPosix 0) (Time.millisToPosix totalTime)) ]

displayItemList: Model -> Element.Element Msg
displayItemList model =
    Element.column [ Element.width Element.fill ]
        ( List.map
            (\item -> displayCompletedItem model item )
            <| filterHistory model )
        

displayCompletedItem: Model -> Completed -> (Element.Element Msg)
displayCompletedItem model completed =
    Element.column [ Element.width Element.fill ]
        [ Element.el [ Element.centerX ] (Element.text <| "Project: " ++ completed.project )
        , Element.el [ Element.centerX ] (Element.text <| "Time spent: " ++ timeSpendString completed.startTime completed.endTime )
        , Element.el [ Element.centerX ] (Element.text <| "Note: " ++ completed.note )
        , Element.el [ Element.centerX ] (Element.text <| "Start time: " ++ stringDateTime completed.startTime model.timeZone Nothing )
        , Element.el [ Element.centerX ] (Element.text <| "End time: " ++ stringDateTime completed.endTime model.timeZone Nothing )
        , Input.button
            [ Background.color primaryColor
            , Element.focused [ Background.color focussedColor ]
            , Element.centerX
            ]
            { onPress = Just <| Editing completed
            , label =  Element.el [ Element.padding 10 ] (Element.text "Edit")
            } 
        ]

displayEditCompletedItem: Model -> Completed -> List (Html Msg)
displayEditCompletedItem model completed =
-- createDropDownItems: Bool -> String -> (String -> Msg) -> List String -> Element.Element Msg
-- createDropDownItems showDropDown selected msg lst =
    let
        timeFrameStringList = List.map (\tf -> timeFrameToString tf) timeFrameList
        dropDownItems = createDropDownItems model.showEditingCompletedProjectDropDown model.editingProject ChangeEditProject model.projectList
        startDropDownItems = createDropDownItems model.showEditingStartTimeDropDown (timeFrameToString model.editingStartTimeFrame) ChangeEditingStartTimeFrame timeFrameStringList
        -- endDropDownItems = createDropDownItems model.showEditingEndTimeDropDown model.editingProject ChangeEditProject model.projectList
    in
        [
        Element.layout []
            <|
            Element.column [ Element.width Element.fill ]
                [ Element.row [ Element.centerX ] 
                    [ Element.text <| "Project: "
                    , createDropDownRow ToggleShowEditingCompletedProjectDropDown dropDownItems 100 model.editingProject
                    ]
                , Element.row [ Element.centerX ]
                    [ Element.text <| "Time Spend: " ++ timeSpendString completed.startTime completed.endTime ]
                , Element.row [ Element.centerX ]
                    [ Element.text "Note: "
                    , Input.text
                        [ Element.width <| Element.px 200 ]
                        { onChange = ChangeEditNote
                        , text = model.editingNote
                        , placeholder = Just <| Input.placeholder [] <| Element.text "Add a note?"
                        , label = Input.labelLeft [] <| Element.none
                        }
                    ]
                , Element.row [ Element.centerX ]
                    [ Element.text <| "Start Time: "
                    , displayTime model.editingStartTime model.timeZone
                    -- , inputTextChange lightColor HandleEditTimeChange Hour startHout Start
                    , Input.button
                        [ Background.color primaryColor
                        , Element.focused [ Background.color focussedColor ]
                        ]
                        { onPress = Just <| ChangeCompletedTime End Decrement
                        , label =  Element.el [ Element.padding 10 ] (Element.text "-")
                        } 
                    , createDropDownRow ToggleShowEditingStartTimeDropDown startDropDownItems 70 (timeFrameToString model.editingStartTimeFrame)
                    , Input.button
                        [ Background.color primaryColor
                        , Element.focused [ Background.color focussedColor ]
                        ]
                        { onPress = Just <| ChangeCompletedTime End Increment
                        , label =  Element.el [ Element.padding 10 ] (Element.text "+")
                        } 
                    ]
                -- inputTextChange lightColor HandleTimeChange Hour fromHour Start Filter
-- inputTextChange: Element.Color -> (TimeFrame -> StartOrEnd -> String -> Msg) -> TimeFrame -> String -> StartOrEnd -> Element.Element Msg
-- inputTextChange color handler timeFrame txt startOrEnd =
                ]

    , text "start time: "
    , Element.layout [] <| displayTime model.editingStartTime model.timeZone
    , displayAdjustTimes model ChangeEditTime ChangeEditingStartTimeFrame Start
    , br [] []
    , text "end time : "
    , Element.layout [] <| displayTime model.editingEndTime model.timeZone
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