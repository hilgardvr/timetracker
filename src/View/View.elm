module View.View exposing (view)

import Html exposing (Html)
import Time
import Model.Model exposing (..)
import View.DisplayTime exposing (displayTime, timeSpendString, stringDateTime)
import View.FilterView exposing (filterHistory)
import View.LoginView exposing (loginView, viewNavBar)
import View.Styles exposing (..)
import Element exposing (..)
import Element.Input as Input
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font


-- view
view: Model -> Html Msg
view model =
    layout [] <| generateGenerateView model

generateGenerateView: Model -> Element Msg
generateGenerateView model =
    case model.loginStatus of
        LoggedOut -> loginView model
        Signup -> loginView model
        Pending -> loginView model
        LoggedIn -> 
            case model.loggedInPage of
                HomeScreen -> 
                    column [ width fill ] 
                        [ loginView model
                        , showCurrentDateTime model
                        , viewDefault model
                        ]
                Timing -> 
                    column [ width fill ]
                        [ loginView model
                        , showCurrentDateTime model
                        , viewTiming model
                        ]
                EditingCompleted ->
                    column [ width fill ]
                        [ loginView model
                        , showEditing model
                        ]
                History -> 
                    column [ width fill ]
                        [ loginView model
                        , viewTimedHistory model
                        ]

showCurrentDateTime: Model -> Element Msg
showCurrentDateTime model =
    el [ centerX, paddingEach { edges | top = 30, bottom = 30 } ] <| text <| stringDateTime model.currentTime model.timeZone Nothing

viewAddProject: Model -> Element Msg
viewAddProject model =
    row [ centerX, paddingEach { edges | top = 20, left = 5, right = 5, bottom = 20 }]
        [ Input.text
            [ centerX, width <| px 200 ]
            { onChange = NewProject
            , text = model.newProject
            , placeholder = Just (Input.placeholder [] (text "New project?"))
            , label = Input.labelAbove [] none
            }
        , Input.button
            [ Background.color primaryColor
            , focused [ Background.color focussedColor ]
            ]
            { onPress = Just AddProject
            , label = el [ padding 10 ] <| text "Add"
            } 
        ]

viewDefault: Model -> Element Msg
viewDefault model =
    let
        dropDownItems = createDropDownItems model.showProjectDropDown model.currentProject ChangeCurrentProject model.projectList
        projectList = 
            if model.currentProject == ""
            then none
            else
                row [ centerX, paddingEach { edges | top = 20, left = 5, right = 5 } ]
                    [ text "Project: "
                    , el [ Font.bold ] <| createDropDownRow ToggleProjectDropDown dropDownItems 100 model.currentProject
                    , Input.button
                        [ Background.color primaryColor
                        , focused [ Background.color focussedColor ]
                        ]
                        { onPress = Just ToggleTimer
                        , label = el [ padding 10 ] <| text "Start"
                        } 
                    ]
    in 
        column (cardAttributes model)
            [ projectList
            , viewAddProject model
            ]

createDropDownRow: Msg -> Element Msg -> Int -> String -> Element Msg
createDropDownRow toggler dropDownItems width txt =
    row
        [ Events.onClick toggler
        , alignLeft
        , below dropDownItems
        ] 
        [ el [ Element.width <| px width, clip ] <| text txt
        , el [ alignRight ] (text (" ▾ "))
        ]

createDropDownItems: Bool -> String -> (String -> Msg) -> List String -> Element Msg
createDropDownItems showDropDown selected msg lst =
    if showDropDown
    then 
        column [ Background.color lightColor ]
            ( List.map
                (\listItem -> 
                    if listItem == selected
                    then none
                    else 
                        row 
                            [ Events.onClick (msg listItem)
                            , width <| px 150
                            , clip
                            ] 
                            [ text listItem ]
                )
                lst
            )
    else none

viewTiming: Model -> Element Msg
viewTiming model =
    let
        cardWidth = View.Styles.cardWidth model
    in
        column (cardAttributes model)
            [ row [ paddingEach { edges | top = 15, bottom = 10 }, Font.bold, width fill, scrollbarX ] 
                [ el [ centerX ] <| text model.currentProject ]
            , row [ centerX, Font.bold, padding 10 ] [ text <| timeSpendString model.startTime model.currentTime ]
            , row [ centerX ] 
                [ Input.text
                    [ centerX, paddingXY 5 0]
                    { onChange = ChangeNote
                    , text = model.note
                    , placeholder = Just (Input.placeholder [  ] (text "Add additional info?"))
                    , label = Input.labelAbove [] none
                    }
                ]
            , row [ centerX, padding 10 ] 
                [ Input.button
                    [ Background.color primaryColor
                    , focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just ToggleTimer
                    , label = el [ padding 10 ] <| text "Stop"
                    } 
                ]
            ]


showEditing: Model -> Element Msg
showEditing model =
    let
        maybeCompleted = List.head (List.filter (\completedItem -> completedItem.id == model.editingId) model.completedList)
    in
        case maybeCompleted of
            Just completed ->
                (displayEditCompletedItem model completed)
            Nothing ->
                (row []
                [ text ("No item found with id: " ++ model.editingId)
                , Input.button  
                    [ Background.color lightColor
                    , focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just Logout 
                    , label = text "Return"
                    }
                ])

createFilterByTimeRow: Model -> StartOrEnd -> Element Msg
createFilterByTimeRow model startOrEnd =
    if model.showByStartTime
    then
        let
            timeFrameStringList = List.map (\tf -> timeFrameToString tf) timeFrameList

            getTimeFrame = 
                case startOrEnd of
                    Start -> stringDateTime model.completedFromTime model.timeZone
                    End -> stringDateTime model.completedToTime model.timeZone

            timeFrameString =
                case startOrEnd of
                    Start -> timeFrameToString model.completedFromTimeFrame
                    End -> timeFrameToString model.completedToTimeFrame

            dropDownItems =
                case startOrEnd of
                    Start -> createDropDownItems model.showTimeFrameFromDropDown timeFrameString ChangeCompletedFromTimeFrame timeFrameStringList
                    End -> createDropDownItems model.showTimeFrameToDropDown timeFrameString ChangeCompletedToTimeFrame timeFrameStringList

            wording =
                case startOrEnd of
                    Start -> "Started after:"
                    End -> "Ended before:"

            dropDownToggler =
                case startOrEnd of
                    Start -> ToggleTimeFrameFromDropDown
                    End -> ToggleTimeFrameToDropDown

            hour = getTimeFrame (Just Hour)
            minute = getTimeFrame (Just Minute)
            second = getTimeFrame (Just Second)
            day = getTimeFrame (Just Day)
            month = getTimeFrame (Just Month)
            year = getTimeFrame (Just Year)
        in 
            column [ width fill ] 
                [ el [ centerX ] <| text <| wording
                , el [ centerX, paddingXY 0 5 ] <| text <| hour ++ ":" ++ minute ++ ":" ++ second ++ " " ++ day ++ " " ++ month ++ " " ++ year 
                , row [ centerX ] 
                    [ Input.button
                        [ Background.color primaryColor
                        , focused [ Background.color focussedColor ]
                        ]
                        { onPress = Just <| ChangeCompletedTime startOrEnd Decrement
                        , label =  el [ padding 10 ] (text "-")
                        } 
                    , createDropDownRow dropDownToggler dropDownItems 70 timeFrameString
                    , Input.button
                        [ Background.color primaryColor
                        , focused [ Background.color focussedColor ]
                        ]
                        { onPress = Just <| ChangeCompletedTime startOrEnd Increment
                        , label =  el [ padding 10 ] (text "+")
                        } 
                    ]
                ]
    else none

createFilterByProjectRow: Model -> Element Msg
createFilterByProjectRow model =
    if model.showFilterByProject
    then 
        let 
            dropDownItems = createDropDownItems model.showFilterByProjectDropDown model.projectShown ChangeShowByProject model.projectList
        in
            createDropDownRow ToggleFilterProjectDropDown dropDownItems 100 model.projectShown
    else text "Filter by project?"

viewTimedHistory: Model -> Element Msg
viewTimedHistory model =
    if List.isEmpty model.completedList
    then 
        column [ width fill ]
            [ row [ paddingXY 0 10, centerX ] 
                [ el [ Font.bold, Font.underline ] <| text "Timed History" ]
            , row [ paddingXY 0 10, centerX ]
                [ el [ ] <| text "No Completed Timed Items" ]
            , Input.button
                [ Background.color primaryColor
                , centerX
                , focused [ Background.color focussedColor ]
                ]
                { onPress = Just Home
                , label =  el [ padding 10 ] (text "Back")
                } 
            ]
    else 
        column [ width fill ]
            [ row [ centerX, paddingXY 0 10 ]
                [ Input.button
                    [ Background.color primaryColor
                    , focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just Home
                    , label =  el [ padding 10 ] (text "Back")
                    } 
                ]

            , row [ paddingXY 0 10, centerX ] 
                [ el [ Font.bold, Font.underline ] <| text "Timed History" ]

            -- filter by start time
            , row [ paddingEach { edges | left = model.window.width // 2 - 125, top = 10 }, spacing 5 ] 
                [ Input.checkbox [ padding 10, Background.color darkColor, width <| px 35 ]
                    { onChange = ToggleShowStarted
                    , icon = Input.defaultCheckbox
                    , checked = model.showByStartTime
                    , label = Input.labelRight [] none
                    }
                , el [ spacing 5 ] <| text "Filter for time period?"
                ]
            , row [ width fill ] 
                [ createFilterByTimeRow model Start ] 
            , row [ width fill ] 
                [ createFilterByTimeRow model End ] 

            -- filter by project
            , row [ paddingEach { edges | left = model.window.width // 2 - 125, top = 10 }, spacing 5 ] 
                [ Input.checkbox [ padding 10, Background.color darkColor, width <| px 35 ]
                    { onChange = ToggleShowFilterProject
                    , icon = Input.defaultCheckbox
                    , checked = model.showFilterByProject
                    , label = Input.labelRight [] <| none
                    }
                , createFilterByProjectRow model
                ]
            , displayTotalTime model
            , displayItemList model
            ]

displayTotalTime: Model -> Element Msg
displayTotalTime model =
    let
        completedTimes = List.map (\item -> Time.posixToMillis item.endTime - Time.posixToMillis item.startTime) (filterHistory model)
        totalTime = List.foldl (+) 0 completedTimes

    in
        row [ centerX, padding 20, Font.bold ]
            [ text ("Total time spent: " ++ View.DisplayTime.timeSpendString (Time.millisToPosix 0) (Time.millisToPosix totalTime)) ]

displayItemList: Model -> Element Msg
displayItemList model =
    column [ width fill ]
        ( List.map
            (\item -> displayCompletedItem model item )
            <| filterHistory model )
        

displayCompletedItem: Model -> Completed -> (Element Msg)
displayCompletedItem model completed =
    column [ width fill, paddingXY 0 5, scrollbarX ]
        [ row [ width fill ] 
            [ el [ centerX ] <| text "Project: " 
            , el [ centerX, Font.bold, paddingXY 5 5 ] <| text completed.project
            ]
        , row [ width fill ] 
            [ el [ centerX ] <| text "Time spent: "
            , el [ centerX, Font.bold ] <| text <| timeSpendString completed.startTime completed.endTime
            ]
        , row [ width fill ]
            [ el [ centerX ] <| text "Note: "
            , el [ centerX, Font.bold ] <| text completed.note
            ]
        , el [ centerX ] (text <| "Started: " ++ stringDateTime completed.startTime model.timeZone Nothing )
        , el [ centerX ] (text <| "Ended: " ++ stringDateTime completed.endTime model.timeZone Nothing )
        , Input.button
            [ Background.color primaryColor
            , focused [ Background.color focussedColor ]
            , centerX
            ]
            { onPress = Just <| Editing completed
            , label =  el [ padding 10 ] (text "Edit")
            } 
        ]

displayEditCompletedItem: Model -> Completed -> Element Msg
displayEditCompletedItem model completed =
    let
        timeFrameStringList = List.map (\tf -> timeFrameToString tf) timeFrameList
        dropDownItems = createDropDownItems model.showEditingCompletedProjectDropDown model.editingProject ChangeEditProject model.projectList
        startDropDownItems = createDropDownItems model.showEditingStartTimeDropDown (timeFrameToString model.editingStartTimeFrame) ChangeEditingStartTimeFrame timeFrameStringList
        endDropDownItems = createDropDownItems model.showEditingEndTimeDropDown (timeFrameToString model.editingEndTimeFrame) ChangeEditingEndTimeFrame timeFrameStringList
    in
        column [ width fill ]
            [ row [ centerX, paddingEach { edges | top = 20 } ] 
                [ text <| "Project: "
                , createDropDownRow ToggleShowEditingCompletedProjectDropDown dropDownItems 100 model.editingProject
                ]
            , row [ centerX, paddingEach { edges | top = 20 } ]
                [ text <| "Time Spend: " ++ timeSpendString completed.startTime completed.endTime ]
            , row [ centerX, paddingEach { edges | top = 20 } ]
                [ text "Note: "
                , Input.text
                    [ width <| px 200 ]
                    { onChange = ChangeEditNote
                    , text = model.editingNote
                    , placeholder = Just <| Input.placeholder [] <| text "Add a note?"
                    , label = Input.labelLeft [] <| none
                    }
                ]
            , row [ centerX, paddingEach { edges | top = 20 } ]
                [ text <| "Started: "
                , displayTime model.editingStartTime model.timeZone
                ]
            , row [ centerX, paddingEach { edges | top = 10 } ]
                [ Input.button
                    [ Background.color primaryColor
                    , focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just <| ChangeEditTime Start Decrement
                    , label =  el [ padding 10 ] (text "-")
                    } 
                , createDropDownRow ToggleShowEditingStartTimeDropDown startDropDownItems 70 (timeFrameToString model.editingStartTimeFrame)
                , Input.button
                    [ Background.color primaryColor
                    , focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just <| ChangeEditTime Start Increment
                    , label =  el [ padding 10 ] (text "+")
                    } 
                ]
            , row [ centerX, paddingEach { edges | top = 20 } ]
                [ text <| "Ended: "
                , displayTime model.editingEndTime model.timeZone
                ]
            , row [ centerX, paddingEach { edges | top = 10 } ]
                [ Input.button
                    [ Background.color primaryColor
                    , focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just <| ChangeEditTime End Decrement
                    , label =  el [ padding 10 ] (text "-")
                    } 
                , createDropDownRow ToggleShowEditingEndTimeDropDown endDropDownItems 70 (timeFrameToString model.editingEndTimeFrame)
                , Input.button
                    [ Background.color primaryColor
                    , focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just <| ChangeEditTime End Increment
                    , label =  el [ padding 10 ] (text "+")
                    } 
                ]
            , row [ centerX, paddingEach { edges | top = 30 } ]
                [ Input.button
                    [ Background.color lightColor
                    , width <| px 80
                    , focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just <| Editing completed
                    , label = el [ padding 10 ] (text "Save")
                    }
                , text " "
                , Input.button
                    [ Background.color lightColor
                    , width <| px 80
                    , focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just <| DeleteCompleted completed
                    , label = el [ padding 10 ] (text "Delete")
                    }
                , text " "
                , Input.button
                    [ Background.color lightColor
                    , width <| px 80
                    , focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just <| DiscardChanges
                    , label = el [ padding 10 ] (text "Cancel")
                    }
                ]
    ]