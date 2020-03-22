
module Main exposing (..)

import Html exposing (div, input, Html, button, text, span)
import Browser
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (type_, placeholder, value)
import Time
import Task 

-- Four parts: model, view, update:w
main  = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions}


-- model

type alias Model = 
    { timing: Bool
    , activity: String
    , currentTime: Time.Posix
    , startTime: Time.Posix
    , endTime: Time.Posix
    , timeZone: Time.Zone
    }

type alias TimedEvent =
    { activity: String
    , startTime: Time.Posix
    , endTime: Time.Posix
    }

type Msg =
    ToggleTimer
    | ChangeActivity String
    | Tick Time.Posix
    | AdjustTimeZone Time.Zone

--init

init: () -> ( Model, Cmd Msg )
init _ = 
    ( Model False "" (Time.millisToPosix 0) (Time.millisToPosix 0) (Time.millisToPosix 0) Time.utc
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
        ChangeActivity activity -> 
            ( { model | activity = activity}
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
    then { model | endTime = model.currentTime, timing = False}
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
        , input [ type_ "text", placeholder "What do you want to time?", value model.activity, onInput ChangeActivity ] []
        , button  
            [ onClick ToggleTimer ]
            [ if model.timing then text "Stop" else text "Start" ]
        ] 

viewTiming: Model -> Html Msg
viewTiming model =
    div []
        [ displayTime 
            (Time.millisToPosix (Time.posixToMillis model.currentTime - Time.posixToMillis model.startTime))
            Time.utc
        , text model.activity
        , button  
            [ onClick ToggleTimer ]
            [ if model.timing then text "Stop" else text "Start" ]
        ]

    
displayTime: Time.Posix -> Time.Zone -> Html Msg
displayTime time zone =
    let
        hour    = padTime (String.fromInt (Time.toHour zone time))
        minute  = padTime (String.fromInt (Time.toMinute zone time))
        second  = padTime (String.fromInt (Time.toSecond zone time))
    in
        span [] [ text (hour ++ ":" ++ minute ++ ":" ++ second)]

padTime: String -> String
padTime time =
    if String.length time < 2
    then padTime ("0" ++ time)
    else time