module View.View exposing (view)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (type_, placeholder, value, selected, checked)
import Time
import Model.Model exposing (..)
import View.DisplayTime exposing (displayTime, timeSpendString)
import View.FilterView exposing (filterHistory)
import View.LoginView exposing (loginView, viewNavBar)
import Element as Element
import Element.Input as Input
import Element.Background as Background
-- import Element exposing (Element)

-- view
primaryColor: Element.Color
primaryColor = Element.rgb255 157 255 209

lightColor: Element.Color
lightColor = Element.rgb255 209 255 255

darkColor: Element.Color
darkColor = Element.rgb255 106 203 160

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
                    , Element.layout [] (viewAddProject model)
                    , viewDefault model
                    ]
            else 
                div []
                    [ Element.layout [] (viewNavBar model)
                    , Element.layout [] (viewAddProject model)
                    , viewTiming model
                    ]


viewAddProject: Model -> Element.Element Msg
viewAddProject model =
    Element.row [ Element.centerX, Element.height (Element.px 100) ]
        [ Input.text
            [ Element.centerX ]
            { onChange = NewProject
            , text = model.newProject
            , placeholder = Just (Input.placeholder [] (Element.text "Add a new project here"))
            , label = Input.labelAbove [] (Element.text "Project")
            }
        , Input.button
            [ Background.color primaryColor
            , Element.focused [ Background.color (Element.rgb255 111 111 111) ]
            ]
            { onPress = Just AddProject
            , label = Element.text "Add Project"
            } 
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