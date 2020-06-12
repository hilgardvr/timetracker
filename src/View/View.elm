module View.View exposing (view)

import Html exposing (Html)
import Time
import Model.Model exposing (..)
import View.DisplayTime exposing (displayTime, timeSpendString, stringDateTime)
import View.FilterView exposing (filterHistory)
import View.LoginView exposing (loginView, viewNavBar)
import View.Colors exposing (..)
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
            [ centerX ]
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
            , label = el [ padding 10 ] <| text "Add Project"
            } 
        ]

viewDefault: Model -> Element Msg
viewDefault model =
    let
        dropDownItems = createDropDownItems model.showProjectDropDown model.currentProject ChangeCurrentProject model.projectList
    in 
        column [ width <| px 300, Background.color lightColor, centerX ]
            [ row [ centerX, paddingEach { edges | top = 20, left = 5, right = 5 } ]
                [ text "Project: "
                , createDropDownRow ToggleProjectDropDown dropDownItems 100 model.currentProject
                , Input.button
                    [ Background.color primaryColor
                    , focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just ToggleTimer
                    , label = el [ padding 10 ] <| text "Start"
                    } 
                ]
            , row [ centerX, paddingEach { edges | top = 20, left = 5, right = 5 } ]
                [ Input.text
                    [ centerX ]
                    { onChange = ChangeNote
                    , text = model.note
                    , placeholder = Just (Input.placeholder [] (text "Add a note?"))
                    , label = Input.labelAbove [] none
                    }
                ]
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
                            , width <| px 100
                            ] 
                            [ text listItem ]
                )
                lst
            )
    else none

viewTiming: Model -> Element Msg
viewTiming model =
    column [ width fill ]
        [  row [ centerX, padding 10 ] [ text <| "Timing project: " ++ model.currentProject ]
        , row [ centerX ] [ text <| timeSpendString model.startTime model.currentTime ]
        , row [ centerX ] 
            [ Input.text
                [ ]
                { onChange = ChangeNote
                , text = model.note
                , placeholder = Just (Input.placeholder [] (text "Add a note?"))
                , label = Input.labelAbove [] none
                }
            ]
        , row [ centerX ] 
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

-- inputTextChange: Color -> (TimeFrame -> StartOrEnd -> String -> Msg) -> TimeFrame -> String -> StartOrEnd -> Element Msg
-- inputTextChange color handler timeFrame txt startOrEnd =
--     Input.text
--         [ Background.color color, width <| px 25 ]
--         { onChange = (handler timeFrame startOrEnd)
--         , text = txt
--         , placeholder = Nothing 
--         , label = Input.labelRight [] <| text txt --none
--         }

createFilterByStartTimeRow: Model -> Element Msg
createFilterByStartTimeRow model =
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

            -- getToTimeFrame = stringDateTime model.completedToTime model.timeZone
            -- toHour = getToTimeFrame (Just Hour)
            -- toMinute = getToTimeFrame (Just Minute)
            -- toSecond = getToTimeFrame (Just Second)
            -- toDay = getToTimeFrame (Just Day)
            -- toMonth = getToTimeFrame (Just Month)
            -- toYear = getToTimeFrame (Just Year)
            -- toTimeFrame = timeFrameToString model.completedToTimeFrame
            -- toDropDownItems = createDropDownItems model.showTimeFrameToDropDown toTimeFrame ChangeCompletedToTimeFrame timeFrameStringList
        in
            column [ width fill ]
                [
                row [ ] 
                    [ text <| "Started after: " ++ fromHour ++ ":" ++ fromMinute ++ ":" ++ fromSecond ++ " " ++ fromDay ++ " " ++ fromMonth ++ " " ++ fromYear
                    ]
                , row [] 
                    [ Input.button
                        [ Background.color primaryColor
                        , focused [ Background.color focussedColor ]
                        ]
                        { onPress = Just <| ChangeCompletedTime Start Decrement
                        , label =  el [ padding 10 ] (text "-")
                        } 
                    , createDropDownRow ToggleTimeFrameFromDropDown fromDropDownItems 70 fromTimeFrame
                    , Input.button
                        [ Background.color primaryColor
                        , focused [ Background.color focussedColor ]
                        ]
                        { onPress = Just <| ChangeCompletedTime Start Increment
                        , label =  el [ padding 10 ] (text "+")
                        } 
                    ]
                ]

                -- , text "   to   "

                -- , text <| toHour ++ ":" ++ toMinute ++ ":" ++ toSecond ++ " " ++ toDay ++ " " ++ toMonth ++ " " ++ toYear
                -- , Input.button
                --     [ Background.color primaryColor
                --     , focused [ Background.color focussedColor ]
                --     ]
                --     { onPress = Just <| ChangeCompletedTime End Decrement
                --     , label =  el [ padding 10 ] (text "-")
                --     } 
                -- , createDropDownRow ToggleTimeFrameToDropDown toDropDownItems 70 toTimeFrame
                -- , Input.button
                --     [ Background.color primaryColor
                --     , focused [ Background.color focussedColor ]
                --     ]
                --     { onPress = Just <| ChangeCompletedTime End Increment
                --     , label =  el [ padding 10 ] (text "+")
                --     } 
                -- ]
    else text "Filter by start time?"

createFilterByEndTimeRow: Model -> Element Msg
createFilterByEndTimeRow model =
    if model.showByStartTime
    then
        let
            timeFrameStringList = List.map (\tf -> timeFrameToString tf) timeFrameList

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
            column [ width fill ] 
                [ row [] [ text <| "Ended before: " ++ toHour ++ ":" ++ toMinute ++ ":" ++ toSecond ++ " " ++ toDay ++ " " ++ toMonth ++ " " ++ toYear ] 
                , row [] 
                    [ Input.button
                        [ Background.color primaryColor
                        , focused [ Background.color focussedColor ]
                        ]
                        { onPress = Just <| ChangeCompletedTime End Decrement
                        , label =  el [ padding 10 ] (text "-")
                        } 
                    , createDropDownRow ToggleTimeFrameToDropDown toDropDownItems 70 toTimeFrame
                    , Input.button
                        [ Background.color primaryColor
                        , focused [ Background.color focussedColor ]
                        ]
                        { onPress = Just <| ChangeCompletedTime End Increment
                        , label =  el [ padding 10 ] (text "+")
                        } 
                    ]
                ]
    else text "Filter by end time?"

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
            [ el [ centerX, height <| px 150 ] <| text "History - No Completed Timed Items Yet..." 
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
                [ el [ Background.color lightColor, Font.bold, Font.underline ] <| text "Timed History" ]

            , row [ centerX ] [ text "Filter history by:" ]

            -- filter by start time
            , row [ paddingEach { edges | left = model.window.width // 2 - 125, top = 10 }, spacing 5 ] 
                [ Input.checkbox [ padding 10, Background.color darkColor, width <| px 35 ]
                    { onChange = ToggleShowStarted
                    , icon = Input.defaultCheckbox
                    , checked = model.showByStartTime
                    , label = Input.labelRight [] <| none
                    }
                , createFilterByStartTimeRow model
                ]

            --  filter by end time
            , row [ paddingEach { edges | left = model.window.width // 2 - 125, top = 10 }, spacing 5 ] 
                [ Input.checkbox [ padding 10, Background.color darkColor, width <| px 35 ]
                    { onChange = ToggleShowStarted
                    , icon = Input.defaultCheckbox
                    , checked = model.showByStartTime
                    , label = Input.labelRight [] <| none
                    }
                , createFilterByEndTimeRow model
                ]

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
    column [ width fill, paddingXY 0 5 ]
        [ el [ centerX, Font.bold ] (text <| "Project: " ++ completed.project )
        , el [ centerX ] (text <| "Time spent: " ++ timeSpendString completed.startTime completed.endTime )
        , el [ centerX ] (text <| "Note: " ++ completed.note )
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