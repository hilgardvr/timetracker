
module Main exposing (..)

import Html exposing (..)
import Browser
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (type_, placeholder, value)
import Time
import Task 

-- Four parts: model, view, update:w
main: Program () Model Msg
main  = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions}


-- model

type alias Completed =
    { activity: String
    , startTime: Time.Posix
    , endTime: Time.Posix
    }

type alias Model = 
    { completed: List Completed
    , timing: Bool
    , currentActivity: String
    , currentTime: Time.Posix
    , startTime: Time.Posix
    , endTime: Time.Posix
    , timeZone: Time.Zone
    }

type Msg =
    ToggleTimer
    | ChangeActivity String
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone

--init

init: () -> ( Model, Cmd Msg )
init _ = 
    ( Model [] False "" (Time.millisToPosix 0) (Time.millisToPosix 0) (Time.millisToPosix 0) Time.utc
    , Task.perform AdjustTimeZone Time.here
    )

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick

-- update

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
    case msg of
        ToggleTimer -> 
            ( doTimer model
            , Cmd.none
            )
        ChangeActivity currentActivity -> 
            ( { model | currentActivity = currentActivity}
            , Cmd.none
            )
        Tick time -> 
            ( { model | currentTime = time }
            , Cmd.none
            )
        AdjustTimeZone zone -> 
            ( { model | timeZone = zone}
            , Cmd.none
            )

doTimer: Model -> Model
doTimer model =
    if model.timing
    then 
        let
            completed = 
                { activity = model.currentActivity
                , startTime = model.startTime
                , endTime = model.endTime
                }
        in
        { model | completed = completed :: model.completed,
                   endTime = model.currentTime, 
                   timing = False, 
                   currentActivity = ""
         }
    else { model | startTime = model.currentTime, timing = True }


-- view

view: Model -> Html Msg
view model =
    if not model.timing
    then viewDefault model
    else viewTiming model

viewDefault: Model -> Html Msg
viewDefault model =
    div []
        [ displayTime model.currentTime model.timeZone
        , input [ type_ "text", placeholder "What do you want to time?", value model.currentActivity, onInput ChangeActivity ] []
        , button  
            [ onClick ToggleTimer ]
            [ if model.timing then text "Stop" else text "Start" ]
        , listCompleted model.completed
        ] 

viewTiming: Model -> Html Msg
viewTiming model =
    div []
        [ displayTime 
            (calcTimeSpend model.startTime model.currentTime)
            Time.utc
        , text model.currentActivity
        , button  
            [ onClick ToggleTimer ]
            [ if model.timing then text "Stop" else text "Start" ]
        , listCompleted model.completed
        ]

listCompleted: List Completed -> Html Msg
listCompleted completed =
    ul [] 
        ( List.map 
            ( \elem -> 
                li [] 
                    [ text (elem.activity ++ "\t: " ++ stringTime (calcTimeSpend elem.startTime elem.endTime) Time.utc ) ]
            )
            completed
        )

calcTimeSpend: Time.Posix -> Time.Posix -> Time.Posix
calcTimeSpend startTime endTime = 
    Time.millisToPosix (Time.posixToMillis endTime - Time.posixToMillis startTime)

stringTime: Time.Posix -> Time.Zone -> String
stringTime time zone =
    let
        hour    = padTime (String.fromInt (Time.toHour zone time))
        minute  = padTime (String.fromInt (Time.toMinute zone time))
        second  = padTime (String.fromInt (Time.toSecond zone time))
    in
        (hour ++ ":" ++ minute ++ ":" ++ second)

displayTime: Time.Posix -> Time.Zone -> Html Msg
displayTime time zone =
    span [] [ text (stringTime time zone) ]

padTime: String -> String
padTime time =
    if String.length time < 2
    then padTime ("0" ++ time)
    else time