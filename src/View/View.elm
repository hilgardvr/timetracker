module View.View exposing (view)

import Html exposing (Html)
import Time
import Model.Model exposing (..)
import View.DisplayTime exposing (displayTime, timeSpendString, stringDateTime)
import View.FilterView exposing (filterHistory)
import View.LoginView exposing (loginView)
import View.Styles exposing (..)
import Element exposing (..)
import Element.Input as Input
import Element.Background as Background
import Element.Events as Events
import Element.Font as Font
import Element.Border as Border
import Dialog exposing (..)


-- view
view: Model -> Html Msg
view model =
    layout [ Background.color lightColor, dialog model ] <| generateGenerateView model

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


dialog: Model -> Attribute Msg
dialog model =
    let
        config =
            { closeMessage = Just CloseDialog
            , maskAttributes = []
            , containerAttributes = [ padding 50, alignBottom, centerX ]
            , headerAttributes = [ Background.color (Element.rgb 255 0 0) ]
            , bodyAttributes = [ Background.color (Element.rgb 255 0 0) ]
            , footerAttributes = []
            , header = Just (text model.dialogHeader)            
            , body = Just (text model.dialogBody)
            , footer = Nothing 
            }
        dialogConfig =
            if model.showDialog
            then Just config
            else Nothing
    in
        inFront (Dialog.view dialogConfig)

showCurrentDateTime: Model -> Element Msg
showCurrentDateTime model =
    el [ centerX, paddingEach { edges | top = 30, bottom = 30 } ] <| text <| stringDateTime model.currentTime model.timeZone Nothing

viewAddProject: Model -> Element Msg
viewAddProject model =
    column [ centerX, paddingEach { edges | top = 20, left = 5, right = 5, bottom = 20 }] [ Input.text
            [ centerX, getWidth model, Background.color lightColor, Font.color <| Element.rgb 0 0 0 ]
            { onChange = NewProject
            , text = model.newProject
            , placeholder = Just (Input.placeholder [ Font.color <| Element.rgb 0 0 0 ] (text "New project?"))
            , label = Input.labelAbove [] none
            }
        , el [ centerX, padding 5 ] <|
            Input.button
                buttonAttributes
                { onPress = Just AddProject
                , label = el [ padding 10, centerX ] <| text "Add"
                } 
        ]

viewDefault: Model -> Element Msg
viewDefault model =
    let
        dropDownItems = createDropDownItems model.showProjectDropDown (getWidth model) model.currentProject ChangeCurrentProject model.projectList
        projectList = 
            if model.currentProject == ""
            then none
            else
                column [ width fill ]
                    [ el [ Font.bold, paddingEach { edges | left = 10, right = 10, top = 20, bottom = 10}, width fill ] <| createDropDownRow model ToggleProjectDropDown dropDownItems (getWidth model) model.currentProject
                    , el [ padding 5, centerX ] <|
                        Input.button
                            buttonAttributes
                            { onPress = Just ToggleTimer
                            , label = el [ centerX, paddingXY 0 10 ] <| text "Start"
                            } 
                    ]
    in 
        column (cardAttributes model)
            [ projectList
            , viewAddProject model
            ]

createDropDownRow: Model -> Msg -> Element Msg -> Attribute Msg -> String -> Element Msg
createDropDownRow model toggler dropDownItems elemWidth txt =
    row
        [ Border.glow darkColor 2
        , paddingXY 0 10
        , Events.onClick toggler
        , below dropDownItems
        , elemWidth
        , centerX
        ]
        [ el [ Element.width fill, clip, paddingXY 5 0] <| text txt
        , el [ alignRight ] (text " ▾ ")
        ]

createDropDownItems: Bool -> Attribute Msg -> String -> (String -> Msg) -> List String -> Element Msg
createDropDownItems showDropDown rowWidth selected msg lst =
    if showDropDown
    then 
        column 
            [ Background.color darkColor
            , Border.rounded 5
            , paddingEach { edges | top = 5}
            , rowWidth
            ]
            ( List.map
                (\listItem -> 
                    if listItem == selected
                    then none
                    else 
                        row 
                            [ Events.onClick (msg listItem)
                            , width fill
                            , clip
                            , paddingEach { edges | bottom = 5 }
                            , Border.glow darkColor 2
                            ] 
                            [ text listItem ]
                )
                lst
            )
    else none

viewTiming: Model -> Element Msg
viewTiming model =
    column (cardAttributes model)
        [ row [ paddingEach { edges | top = 15, bottom = 10 }, Font.bold, width fill, scrollbarX ] 
            [ el [ centerX ] <| text model.currentProject ]
        , row [ centerX, Font.bold, padding 10 ] [ text <| timeSpendString model.startTime model.currentTime ]
        , row [ centerX ] 
            [ Input.text
                [ centerX
                , Background.color lightColor
                , Font.color <| Element.rgb 0 0 0
                , Border.rounded 5
                , paddingXY 5 0]
                { onChange = ChangeNote
                , text = model.note
                , placeholder = Just (Input.placeholder [  ] (text "Add additional info?"))
                , label = Input.labelAbove [] none
                }
            ]
        , row [ centerX, padding 10 ] 
            [ Input.button
                buttonAttributes
                { onPress = Just ToggleTimer
                , label = el [ padding 10, centerX ] <| text "Stop"
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
                    Start -> createDropDownItems model.showTimeFrameFromDropDown (width <| px 100) timeFrameString ChangeCompletedFromTimeFrame timeFrameStringList
                    End -> createDropDownItems model.showTimeFrameToDropDown (width <| px 100) timeFrameString ChangeCompletedToTimeFrame timeFrameStringList

            wording =
                case startOrEnd of
                    Start -> "Started after:"
                    End -> "Ended before:"

            dropDownToggler =
                case startOrEnd of
                    Start -> ToggleTimeFrameFromDropDown
                    End -> ToggleTimeFrameToDropDown

        in 
            column [ width fill ] 
                [ el [ centerX, Element.paddingEach { edges | top = 5 } ] <| text <| wording
                , el [ centerX, paddingXY 0 5, Font.bold ] <| text <| getTimeFrame Nothing
                , row [ centerX, spacing 5 ] 
                    [ Input.button
                        buttonAttributes
                        { onPress = Just <| ChangeCompletedTime startOrEnd Decrement
                        , label =  el [ paddingXY 0 10, centerX ] (text "-")
                        } 
                    , createDropDownRow model dropDownToggler dropDownItems (width <| px 100) timeFrameString
                    , Input.button
                        buttonAttributes
                        { onPress = Just <| ChangeCompletedTime startOrEnd Increment
                        , label =  el [ paddingXY 0 10, centerX ] (text "+")
                        } 
                    ]
                ]
    else none

createFilterByProjectRow: Model -> Element Msg
createFilterByProjectRow model =
    if model.showFilterByProject
    then 
        let 
            dropDownItems = createDropDownItems model.showFilterByProjectDropDown (width <| px 200) model.projectShown ChangeShowByProject model.projectList
        in
            createDropDownRow model ToggleFilterProjectDropDown dropDownItems (width <| px 200) model.projectShown
    else text "Filter by project?"

viewTimedHistory: Model -> Element Msg
viewTimedHistory model =
    if List.isEmpty model.completedList
    then 
        el [ paddingEach { edges | top = 20}, width fill ] <| column (cardAttributes model)
            [ row [ paddingXY 0 10, centerX ] 
                [ el [ Font.bold, Font.underline ] <| text "Timed History" ]
            , row [ paddingXY 0 10, centerX ]
                [ el [ ] <| text "No Completed Timed Items" ]
            , el [ paddingEach { edges | bottom = 10 }, width fill ] <|
                Input.button
                    [ Background.color darkColor
                    , centerX
                    , focused [ Background.color focussedColor ]
                    , Border.rounded 5
                    , Font.bold
                    ]
                    { onPress = Just Home
                    , label =  el [ padding 10 ] (text "Back")
                    } 
            ]
    else 
        el [ paddingEach { edges | top = 20}, width fill ] <| column (cardAttributes model)
            [ row [ centerX, paddingXY 0 10 ]
                [ Input.button
                    buttonAttributes
                    { onPress = Just Home
                    , label =  el [ padding 10, centerX ] (text "Back")
                    } 
                ]

            , row [ paddingXY 0 10, centerX ] 
                [ el [ Font.bold, Font.underline ] <| text "Timed History" ]

            -- filter by start time
            , row [ paddingEach { edges | left = cardWidth model // 2 - 125, top = 10 }, spacing 5 ] 
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
            , row [ paddingEach { edges | left = cardWidth model // 2 - 125, top = 10 }, spacing 5 ] 
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
    column [ ]
        ( List.map
            (\item -> displayCompletedItem model item )
            <| filterHistory model )
        

displayCompletedItem: Model -> Completed -> (Element Msg)
displayCompletedItem model completed =
    column [ width <| px (cardWidth model), paddingXY 5 5]
        [ row [ width fill, scrollbarX  ] 
            [ el [ centerX, Font.bold, paddingXY 5 2 ] <| text completed.project ]
        , row [ centerX ] 
            [ el [ centerX, Font.bold, paddingXY 5 2 ] <| text <| timeSpendString completed.startTime completed.endTime ]
        , if String.isEmpty completed.note
          then none
          else
            row [ centerX, width fill, scrollbarX ]
                [ el [ centerX, paddingXY 5 2 ] <| text completed.note ]
        , el [ centerX, Font.size 17, paddingXY 5 2 ] (text <| "Started: " ++ stringDateTime completed.startTime model.timeZone Nothing )
        , el [ centerX, Font.size 17, paddingXY 5 2 ] (text <| "Ended:  " ++ stringDateTime completed.endTime model.timeZone Nothing )
        , Input.button
            (List.append buttonAttributes [ centerX ])
            { onPress = Just <| Editing completed
            , label =  el [ padding 10, centerX ] (text "Edit")
            } 
        ]

displayEditCompletedItem: Model -> Completed -> Element Msg
displayEditCompletedItem model completed =
    let
        timeFrameStringList = List.map (\tf -> timeFrameToString tf) timeFrameList
        dropDownItems = createDropDownItems model.showEditingCompletedProjectDropDown (width <| ((px <| cardWidth model - 20) |> maximum maxWidth)) model.editingProject ChangeEditProject model.projectList
        startDropDownItems = createDropDownItems model.showEditingStartTimeDropDown (getTimeframeWidth model) (timeFrameToString model.editingStartTimeFrame) ChangeEditingStartTimeFrame timeFrameStringList
        endDropDownItems = createDropDownItems model.showEditingEndTimeDropDown (getTimeframeWidth model) (timeFrameToString model.editingEndTimeFrame) ChangeEditingEndTimeFrame timeFrameStringList
    in
        el [ paddingEach { edges | top = 30, bottom = 30 }, width fill ] <| column (centerX :: cardAttributes model)
            [ row [ centerX, paddingXY 0 10 ]
                [ Input.button
                    buttonAttributes
                    { onPress = Just Home
                    , label =  el [ padding 10, centerX ] (text "Back")
                    } 
                ]
            , row [ centerX, paddingEach { edges | top = 20 }, getWidth model ] 
                [ createDropDownRow model ToggleShowEditingCompletedProjectDropDown dropDownItems (width <| ((px <| cardWidth model - 20) |> maximum maxWidth)) model.editingProject ]
            , row [ centerX, paddingEach { edges | top = 20 }, Font.bold ]
                [ text <| "Time spend: " ++ timeSpendString model.editingStartTime model.editingEndTime ]
            , row [ centerX, paddingEach { edges | top = 20 } ]
                [ text "Note: "
                , Input.text
                    [ width <| px 200, Background.color lightColor, Font.color <| Element.rgb 0 0 0 ]
                    { onChange = ChangeEditNote
                    , text = model.editingNote
                    , placeholder = Just <| Input.placeholder [ Font.color <| Element.rgb 0 0 0 ] <| text "Add a note?"
                    , label = Input.labelLeft [] <| none
                    }
                ]
            , row [ centerX, paddingEach { edges | top = 20 }, Font.size 17 ]
                [ text <| "Started: "
                , displayTime model.editingStartTime model.timeZone
                ]
            , row [ centerX, paddingEach { edges | top = 10 }, spacing 5 ]
                [ Input.button
                    (List.append buttonAttributes [ width <| px 80 ])
                    { onPress = Just <| ChangeEditTime Start Decrement
                    , label =  el [ paddingXY 0 10, centerX ] (text "-")
                    } 
                , createDropDownRow model ToggleShowEditingStartTimeDropDown startDropDownItems (getTimeframeWidth model) (timeFrameToString model.editingStartTimeFrame)
                , Input.button
                    (List.append buttonAttributes [ width <| px 80 ])
                    { onPress = Just <| ChangeEditTime Start Increment
                    , label =  el [ paddingXY 0 10, centerX ] (text "+")
                    } 
                ]
            , row [ centerX, paddingEach { edges | top = 20 }, Font.size 17 ]
                [ text <| "Ended: "
                , displayTime model.editingEndTime model.timeZone
                ]
            , row [ centerX, paddingEach { edges | top = 10 }, spacing 5 ]
                [ Input.button
                    (List.append buttonAttributes [ width <| px 80 ])
                    { onPress = Just <| ChangeEditTime End Decrement
                    , label =  el [ paddingXY 0 10, centerX ] (text "-")
                    } 
                , createDropDownRow model ToggleShowEditingEndTimeDropDown endDropDownItems (getTimeframeWidth model) (timeFrameToString model.editingEndTimeFrame)
                , Input.button
                    (List.append buttonAttributes [ width <| px 80 ])
                    { onPress = Just <| ChangeEditTime End Increment
                    , label =  el [ paddingXY 0 10, centerX ] (text "+")
                    } 
                ]
            , row [ centerX, paddingEach { edges | top = 30, bottom = 20 } ]
                [ Input.button
                    buttonAttributes
                    { onPress = Just <| Editing completed
                    , label = el [ centerX, paddingXY 0 10 ] (text "Save")
                    }
                , text " "
                , Input.button
                    buttonAttributes
                    { onPress = Just <| DeleteCompleted completed
                    , label = el [ centerX, paddingXY 0 10 ] (text "Delete")
                    }
                , text " "
                , Input.button
                    buttonAttributes
                    { onPress = Just <| DiscardChanges
                    , label = el [centerX, paddingXY 0 10 ] (text "Cancel")
                    }
                ]
    ]