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
    let
        viewGenerate =
            case model.loginStatus of
                LoggedOut -> loginView model
                Signup -> loginView model
                Pending -> loginView model
                LoggedIn -> 
                    if not model.timing
                    then
                        column [ width fill ] 
                            [ viewNavBar model
                            , showCurrentDateTime model
                            , viewAddProject model
                            , viewDefault model
                            , showEditingOrCompleted model
                            ]
                    else
                        column [ width fill ]
                            [ viewNavBar model
                            , showCurrentDateTime model
                            , viewTiming model
                            , showEditingOrCompleted model
                            ]
        in
            layout [] viewGenerate

showCurrentDateTime: Model -> Element Msg
showCurrentDateTime model =
    el [ centerX ] <| text <| stringDateTime model.currentTime model.timeZone Nothing

viewAddProject: Model -> Element Msg
viewAddProject model =
    row [ centerX, height (px 100) ]
        [ Input.text
            [ centerX ]
            { onChange = NewProject
            , text = model.newProject
            , placeholder = Just (Input.placeholder [] (text "Add a new project?"))
            , label = Input.labelAbove [] none
            }
        , Input.button
            [ Background.color primaryColor
            , focused [ Background.color focussedColor ]
            ]
            { onPress = Just AddProject
            , label = text "Add Project"
            } 
        ]

viewDefault: Model -> Element Msg
viewDefault model =
    let
        dropDownItems = createDropDownItems model.showProjectDropDown model.currentProject ChangeCurrentProject model.projectList
    in 
        column [ width fill ]
            [ row [ centerX ]
                [ text "Project to time: "
                , createDropDownRow ToggleProjectDropDown dropDownItems 100 model.currentProject
                , Input.button
                    [ Background.color primaryColor
                    , focused [ Background.color focussedColor ]
                    ]
                    { onPress = Just ToggleTimer
                    , label = text "Start"
                    } 
                ]
            , row [ centerX ]
                [ Input.text
                    [ centerX ]
                    { onChange = ChangeNote
                    , text = model.note
                    , placeholder = Just (Input.placeholder [] (text "Add a note?"))
                    , label = Input.labelAbove [] none
                    }
                ]
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
    column [ width fill  ]
        [  row [ centerX, padding 20 ] [ text <| "Timing project: " ++ model.currentProject ]
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
                , label = text "Stop"
                } 
            ]
        ]

showEditingOrCompleted: Model -> Element Msg
showEditingOrCompleted model =
    if model.editing
    then showEditing model
    else viewTimedHistory model

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
                    { onPress = Just (Editing (Completed "" "" (Time.millisToPosix 0) (Time.millisToPosix 0) "")) 
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

createFilterByTimeRow: Model -> Element Msg
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
            row [ ] 
                [ text <| fromHour ++ ":" ++ fromMinute ++ ":" ++ fromSecond ++ " " ++ fromDay ++ " " ++ fromMonth ++ " " ++ fromYear
                , Input.button
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

                , text "   to   "

                , text <| fromHour ++ ":" ++ fromMinute ++ ":" ++ fromSecond ++ " " ++ fromDay ++ " " ++ fromMonth ++ " " ++ fromYear
                , Input.button
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
    else text "Filter by started time?"

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
        el [ centerX, height <| px 150 ] <| text "History - No Completed Timed Items Yet..." 
    else 
        column [ width fill ]
            [ row [ height <| px 100, centerX ] 
                [ el [ Background.color lightColor, Font.bold, Font.underline ] <| text "Timed History" ]
            , row [ centerX ] [ text "Filter history by:" ]

            -- filter by time
            , row [ alignLeft, width fill ] 
                [ Input.checkbox [ padding 10, Background.color darkColor, width <| px 40 ]
                    { onChange = ToggleShowStarted
                    , icon = Input.defaultCheckbox
                    , checked = model.showByStartTime
                    , label = Input.labelRight [] <| none
                    }
                , createFilterByTimeRow model
                ]

            -- filter by project
            , row [ alignLeft, width  fill ]
                [ Input.checkbox [ padding 10, Background.color darkColor, width <| px 40 ]
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
    column [ width fill ]
        [ el [ centerX ] (text <| "Project: " ++ completed.project )
        , el [ centerX ] (text <| "Time spent: " ++ timeSpendString completed.startTime completed.endTime )
        , el [ centerX ] (text <| "Note: " ++ completed.note )
        , el [ centerX ] (text <| "Start time: " ++ stringDateTime completed.startTime model.timeZone Nothing )
        , el [ centerX ] (text <| "End time: " ++ stringDateTime completed.endTime model.timeZone Nothing )
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
            [ row [ centerX ] 
                [ text <| "Project: "
                , createDropDownRow ToggleShowEditingCompletedProjectDropDown dropDownItems 100 model.editingProject
                ]
            , row [ centerX ]
                [ text <| "Time Spend: " ++ timeSpendString completed.startTime completed.endTime ]
            , row [ centerX ]
                [ text "Note: "
                , Input.text
                    [ width <| px 200 ]
                    { onChange = ChangeEditNote
                    , text = model.editingNote
                    , placeholder = Just <| Input.placeholder [] <| text "Add a note?"
                    , label = Input.labelLeft [] <| none
                    }
                ]
            , row [ centerX ]
                [ text <| "Start Time: "
                , displayTime model.editingStartTime model.timeZone
                , Input.button
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
            , row [ centerX ]
                [ text <| "End Time: "
                , displayTime model.editingEndTime model.timeZone
                , Input.button
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
            , row [ centerX ]
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